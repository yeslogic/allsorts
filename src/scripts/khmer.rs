use crate::error::ShapingError;
use crate::gsub::RawGlyph;
use crate::layout::{GDEFTable, LayoutCache, LayoutTable, GSUB};
use crate::unicode::mcc::sort_by_modified_combining_class;

pub(super) fn preprocess_khmer(cs: &mut Vec<char>) {
    decompose_matra(cs);
    sort_by_modified_combining_class(cs);
}

fn decompose_matra(cs: &mut Vec<char>) {
    let mut i = 0;
    while i < cs.len() {
        match cs[i] {
            '\u{17BE}' | '\u{17BF}' | '\u{17C0}' | '\u{17C4}' | '\u{17C5}' => {
                cs.insert(i, '\u{17C1}');
                i += 2;
            }
            _ => i += 1,
        }
    }
}

pub fn gsub_apply_khmer(
    dotted_circle_index: u16,
    gsub_cache: &LayoutCache<GSUB>,
    gsub_table: &LayoutTable<GSUB>,
    gdef_table: Option<&GDEFTable>,
    script_tag: u32,
    lang_tag: Option<u32>,
    glyphs: &mut Vec<RawGlyph<()>>,
) -> Result<(), ShapingError> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    mod decompose_matra {
        use super::*;

        #[test]
        fn test_decomposition1() {
            let mut cs = vec!['\u{17C0}'];
            decompose_matra(&mut cs);

            assert_eq!(vec!['\u{17C1}', '\u{17C0}'], cs);
        }

        #[test]
        fn test_decomposition2() {
            let mut cs = vec!['\u{17C0}', '\u{17C0}'];
            decompose_matra(&mut cs);

            assert_eq!(vec!['\u{17C1}', '\u{17C0}', '\u{17C1}', '\u{17C0}'], cs);
        }
    }
}
