use std::convert::TryFrom;

use crate::binary::read::{
    ReadArray, ReadBinary, ReadBinaryDep, ReadCtxt, ReadFixedSizeDep, ReadScope,
};
use crate::bitmap::{
    Bitmap, BitmapGlyph, EncapsulatedBitmap, EncapsulatedFormat, Metrics, OriginOffset,
};
use crate::error::ParseError;
use crate::size;

pub struct SvgTable<'a> {
    pub version: u16,
    pub document_records: ReadArray<'a, SVGDocumentRecord<'a>>,
}

pub struct SVGDocumentRecord<'a> {
    pub start_glyph_id: u16,
    pub end_glyph_id: u16,
    pub svg_document: &'a [u8],
}

impl<'a> SvgTable<'a> {
    pub fn lookup_glyph(&self, glyph_id: u16) -> Result<Option<SVGDocumentRecord<'a>>, ParseError> {
        for record in self.document_records.iter_res() {
            let record = record?;
            if glyph_id >= record.start_glyph_id && glyph_id <= record.end_glyph_id {
                // TODO: Check for compression and inflate
                return Ok(Some(record));
            }
        }
        Ok(None)
    }
}

impl<'a> ReadBinary<'a> for SvgTable<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let version = ctxt.read_u16be()?;
        ctxt.check(version == 0)?;
        let document_records_offset = usize::try_from(ctxt.read_u32be()?)?;

        let records_scope = scope.offset(document_records_offset);
        let mut records_ctxt = records_scope.ctxt();
        let num_records = records_ctxt.read_u16be().map(usize::from)?;
        let document_records = records_ctxt.read_array_dep(num_records, records_scope)?;

        Ok(SvgTable {
            version,
            document_records,
        })
    }
}

impl<'a> ReadBinaryDep<'a> for SVGDocumentRecord<'a> {
    type Args = ReadScope<'a>;
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, scope: ReadScope<'a>) -> Result<Self, ParseError> {
        let start_glyph_id = ctxt.read_u16be()?;
        let end_glyph_id = ctxt.read_u16be()?;
        let svg_doc_offset = usize::try_from(ctxt.read_u32be()?)?;
        let svg_doc_length = usize::try_from(ctxt.read_u32be()?)?;
        let svg_data = scope.offset_length(svg_doc_offset, svg_doc_length)?;
        let svg_document = svg_data.data();

        Ok(SVGDocumentRecord {
            start_glyph_id,
            end_glyph_id,
            svg_document,
        })
    }
}
impl<'a> ReadFixedSizeDep<'a> for SVGDocumentRecord<'a> {
    fn size(_: Self::Args) -> usize {
        // uint16   startGlyphID
        // uint16   endGlyphID
        // Offset32 svgDocOffset
        // uint32   svgDocLength
        // — https://docs.microsoft.com/en-us/typography/opentype/spec/svg#svg-document-list
        (2 * size::U16) + (2 * size::U32)
    }
}

impl<'a> TryFrom<&SVGDocumentRecord<'a>> for BitmapGlyph {
    type Error = ParseError;

    fn try_from(svg_record: &SVGDocumentRecord<'a>) -> Result<Self, ParseError> {
        // If the document is compressed then inflate it. &[0x1F, 0x8B, 0x08] is a gzip member
        // header indicating "deflate" as the compression method. See section 2.3.1 of
        // https://www.ietf.org/rfc/rfc1952.txt
        let data = if svg_record.svg_document.starts_with(&[0x1F, 0x8B, 0x08]) {
            let mut gz = GzDecoder::new(svg_record.svg_document);
            let mut uncompressed = Vec::with_capacity(svg_record.svg_document.len());
            gz.read_to_end(&mut uncompressed)
                .map_err(|_err| ParseError::CompressionError)?;
            uncompressed.into_boxed_slice()
        } else {
            Box::from(svg_record.svg_document)
        };

        let encapsulated = EncapsulatedBitmap {
            format: EncapsulatedFormat::Svg,
            data: Box::from(svg_record.svg_document),
        };
        BitmapGlyph {
            bitmap: Bitmap::Encapsulated(encapsulated),
            metrics: Metrics::HmtxVmtx(OriginOffset { x: 0, y: 0 }),
            ppem_x: None,
            ppem_y: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fontfile::FontFile;
    use crate::tables::FontTableProvider;
    use crate::tag;
    use crate::tests::read_fixture;

    #[test]
    fn test_read_svg() {
        let buffer = read_fixture("tests/fonts/opentype/TwitterColorEmoji-SVGinOT.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope
            .read::<FontFile<'_>>()
            .expect("unable to parse font file");
        let table_provider = font_file
            .table_provider(0)
            .expect("unable to create font provider");
        let svg_data = table_provider
            .read_table_data(tag::SVG)
            .expect("unable to read SVG table data");
        let svg = ReadScope::new(&svg_data).read::<SvgTable<'_>>().unwrap();

        let records = svg
            .document_records
            .iter_res()
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(records.len(), 3075);

        let record = &records[0];
        assert_eq!(record.start_glyph_id, 5);
        assert_eq!(record.end_glyph_id, 5);
        assert_eq!(record.svg_document.len(), 751);
        let doc = std::str::from_utf8(record.svg_document).unwrap();
        assert_eq!(&doc[0..43], "<?xml version='1.0' encoding='UTF-8'?>\n<svg");
    }
}
