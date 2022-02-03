use std::convert::TryFrom;
use std::mem;

use super::{
    owned, CFFVariant, CIDData, Charset, CustomCharset, DictDelta, FDSelect, Font, FontDict,
    MaybeOwnedIndex, Operand, Operator, ParseError, Range, ADOBE, CFF, IDENTITY,
    ISO_ADOBE_LAST_SID, OFFSET_ZERO,
};
use crate::binary::read::ReadArrayCow;
use crate::binary::write::{WriteBinaryDep, WriteBuffer};
use crate::subset::SubsetGlyphs;

pub struct SubsetCFF<'a> {
    table: CFF<'a>,          // FIXME: pub
    new_to_old_id: Vec<u16>, // FIXME: pub
}

impl<'a> From<SubsetCFF<'a>> for CFF<'a> {
    fn from(subset: SubsetCFF<'a>) -> CFF<'a> {
        subset.table
    }
}

impl<'a> SubsetGlyphs for SubsetCFF<'a> {
    fn len(&self) -> usize {
        self.new_to_old_id.len()
    }

    fn old_id(&self, index: usize) -> u16 {
        self.new_to_old_id[index]
    }
}

impl<'a> CFF<'a> {
    /// The `Vec<u16>` in the output is a mapping from new to old glyph id.
    ///
    /// `glpyh_ids` contains the ids of the glyphs to retain.
    /// When subsetting a Type 1 CFF font and retaining more than 255 glyphs the
    /// `convert_cff_to_cid_if_more_than_255_glyphs` argument controls whether the Type 1 font
    /// is converted to a CID keyed font in the process. The primary motivation for this is
    /// broader compatibility, especially if the subset font is embedded in a PDF.
    ///
    /// **Known Limitations**
    ///
    /// Currently the subsetting process does not produce the smallest possible output font.
    /// There are various parts of the source font that are copied to the output font as-is.
    /// Specifically the subsetting process does not subset the String INDEX, or the Local or
    /// Global subroutines.
    ///
    /// Subsetting the String INDEX requires updating all String IDs (SID) in the font so
    /// that they point at their new position in the String INDEX. Subsetting the subroutines
    /// requires parsing the CharStrings, which describe the glyph outlines. The CharStrings
    /// format is non-trivial so this has been left for now.
    pub fn subset(
        &'a self,
        glyph_ids: &[u16],
        convert_cff_to_cid_if_more_than_255_glyphs: bool,
    ) -> Result<SubsetCFF<'a>, ParseError> {
        let mut cff = self.to_owned();
        let font: &mut Font<'_> = &mut cff.fonts[0];
        let mut charset = Vec::with_capacity(glyph_ids.len());
        let mut fd_select = Vec::with_capacity(glyph_ids.len());
        let mut new_to_old_id = Vec::with_capacity(glyph_ids.len());
        let mut glyph_data = Vec::with_capacity(glyph_ids.len());

        for &glyph_id in glyph_ids {
            let data = font
                .char_strings_index
                .read_object(usize::from(glyph_id))
                .ok_or(ParseError::BadIndex)?;
            glyph_data.push(data.to_owned());
            new_to_old_id.push(glyph_id);

            if glyph_id != 0 {
                let sid_or_cid = font
                    .charset
                    .id_for_glyph(glyph_id)
                    .ok_or(ParseError::BadIndex)?;
                charset.push(sid_or_cid);
            }

            // Calculate CID/Type 1 specific updates
            match &font.data {
                CFFVariant::CID(cid) => {
                    // Find out which font DICT this glyph maps to if it's a CID font
                    // Need to know which font DICT applies to each glyph, then ideally work out which FDSelect
                    // format is the best to use. For now it's probably good enough to just use format 0
                    let fd_index = cid
                        .fd_select
                        .font_dict_index(glyph_id)
                        .ok_or(ParseError::BadIndex)?;
                    fd_select.push(fd_index);
                }
                CFFVariant::Type1(_type1) => {}
            }
        }

        font.char_strings_index = MaybeOwnedIndex::Owned(owned::Index { data: glyph_data });

        if font.is_cid_keyed() {
            // Update CID/Type 1 specific structures
            match &mut font.data {
                CFFVariant::CID(cid) => {
                    cid.fd_select = FDSelect::Format0 {
                        glyph_font_dict_indices: ReadArrayCow::Owned(fd_select),
                    }
                }
                CFFVariant::Type1(_type1) => {}
            }

            font.charset = Charset::Custom(CustomCharset::Format0 {
                glyphs: ReadArrayCow::Owned(charset),
            });
        } else if convert_cff_to_cid_if_more_than_255_glyphs && font.char_strings_index.len() > 255
        {
            font.charset = convert_type1_to_cid(&mut cff.string_index, font)?;
        } else {
            let iso_adobe = 1..=ISO_ADOBE_LAST_SID;
            if charset
                .iter()
                .zip(iso_adobe)
                .all(|(sid, iso_adobe_sid)| *sid == iso_adobe_sid)
            {
                // As per section 18 of Technical Note #5176: There are no predefined charsets for CID
                // fonts. So this branch is only taken for Type 1 fonts.
                font.charset = Charset::ISOAdobe;
            } else {
                font.charset = Charset::Custom(CustomCharset::Format0 {
                    glyphs: ReadArrayCow::Owned(charset),
                });
            }
        }

        Ok(SubsetCFF {
            table: cff,
            new_to_old_id,
        })
    }
}

fn convert_type1_to_cid<'a>(
    string_index: &mut MaybeOwnedIndex<'a>,
    font: &mut Font<'a>,
) -> Result<Charset<'a>, ParseError> {
    assert!(!font.is_cid_keyed());

    // Retrieve the SIDs of Adobe and Identity, adding them if they're not in the String INDEX
    // already.
    let (adobe_sid, identity_sid) = match (string_index.index(ADOBE), string_index.index(IDENTITY))
    {
        (Some(adobe_sid), Some(identity_sid)) => (adobe_sid, identity_sid),
        (Some(adobe_sid), None) => (adobe_sid, string_index.push(IDENTITY.to_owned())),
        (None, Some(identity_sid)) => (string_index.push(ADOBE.to_owned()), identity_sid),
        (None, None) => (
            string_index.push(ADOBE.to_owned()),
            string_index.push(IDENTITY.to_owned()),
        ),
    };

    // Build Font DICT
    let mut font_dict = FontDict::new();
    font_dict.inner_mut().push((
        Operator::Private,
        vec![Operand::Offset(0), Operand::Offset(0)],
    )); // Size and Offset will be updated when written out

    let mut font_dict_buffer = WriteBuffer::new();
    FontDict::write_dep(&mut font_dict_buffer, &font_dict, DictDelta::new())
        .map_err(|_err| ParseError::BadValue)?;
    let font_dict_index = MaybeOwnedIndex::Owned(owned::Index {
        data: vec![font_dict_buffer.into_inner()],
    });

    let n_glyphs = u16::try_from(font.char_strings_index.len())?;

    let fd_select = FDSelect::Format3 {
        ranges: ReadArrayCow::Owned(vec![Range {
            first: 0,
            n_left: 0,
        }]),
        sentinel: n_glyphs,
    };
    let cid_data = CFFVariant::CID(CIDData {
        font_dict_index,
        private_dicts: Vec::new(),
        local_subr_indices: Vec::new(),
        fd_select,
    });

    // Swap Type1 data with CID data
    let type1_data = match mem::replace(&mut font.data, cid_data) {
        CFFVariant::Type1(data) => data,
        CFFVariant::CID(_) => unreachable!(),
    };
    match &mut font.data {
        CFFVariant::Type1(_type1) => unreachable!(),
        CFFVariant::CID(cid) => {
            cid.private_dicts = vec![type1_data.private_dict];
            cid.local_subr_indices = vec![type1_data.local_subr_index];
        }
    };

    // Update the Top DICT
    // Add ROS
    let registry = Operand::Integer(i32::try_from(adobe_sid)?);
    let ordering = Operand::Integer(i32::try_from(identity_sid)?);
    let supplement = Operand::Integer(0);
    let ros = (Operator::ROS, vec![registry, ordering, supplement]);
    font.top_dict.inner_mut().insert(0, ros);

    // Add FDSelect and FDArray offsets to Top DICT
    // Actual offsets will be filled in when writing
    font.top_dict
        .inner_mut()
        .push((Operator::FDArray, OFFSET_ZERO.to_vec()));
    font.top_dict
        .inner_mut()
        .push((Operator::FDSelect, OFFSET_ZERO.to_vec()));

    // Add CIDCount
    font.top_dict.inner_mut().push((
        Operator::CIDCount,
        vec![Operand::Integer(i32::from(n_glyphs))],
    ));

    // Remove Private DICT offset and encoding
    font.top_dict.remove(Operator::Private);
    font.top_dict.remove(Operator::Encoding);

    // Add charset
    Ok(Charset::Custom(CustomCharset::Format2 {
        ranges: ReadArrayCow::Owned(vec![Range {
            first: 1,
            n_left: n_glyphs.checked_sub(2).ok_or(ParseError::BadIndex)?,
        }]),
    }))
}
