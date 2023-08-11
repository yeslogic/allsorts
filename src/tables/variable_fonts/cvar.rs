// #![deny(missing_docs)]

//! `cvar` CVT Variations Table
//!
//! <https://learn.microsoft.com/en-us/typography/opentype/spec/cvar>

use crate::binary::read::{ReadBinaryDep, ReadCtxt};
use crate::error::ParseError;
use crate::tables::variable_fonts::TupleVariationStore;
use crate::tables::CvtTable;

/// `cvar` CVT Variations Table
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/cvar#table-format>
pub struct Cvar<'a> {
    /// Major version number of the glyph variations table.
    pub major_version: u16,
    /// Minor version number of the glyph variations table.
    pub minor_version: u16,
    store: TupleVariationStore<'a, super::Cvar>,
}

impl Cvar<'_> {
    // TODO: Provide an API for accessing variation data
}

impl ReadBinaryDep for Cvar<'_> {
    type Args<'a> = (u16, &'a CvtTable<'a>);
    type HostType<'a> = Cvar<'a>;

    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        (axis_count, cvt): (u16, &CvtTable<'_>),
    ) -> Result<Self::HostType<'a>, ParseError> {
        let major_version = ctxt.read_u16be()?;
        ctxt.check_version(major_version == 1)?;
        let minor_version = ctxt.read_u16be()?;
        let num_cvts = cvt.values.len() as u32; // FIXME: cast
        let store =
            ctxt.read_dep::<TupleVariationStore<'_, super::Cvar>>((axis_count, num_cvts))?;

        Ok(Cvar {
            major_version,
            minor_version,
            store,
        })
    }
}
