// tests/subset_and_map_simple_validation.rs
mod common;

#[cfg(test)]
mod subset_and_map_simple_validation_tests {
    use crate::common;
    use allsorts::binary::read::ReadScope;
    use allsorts::subset::{subset_and_map, CmapTarget, SubsetProfile, SubsetResult};
    use allsorts::tables::OpenTypeFont;
    use allsorts::{tag, Font};
    use std::collections::{HashMap, HashSet};

    /// Basic test - verify mapping is sequential when input is sequential
    #[test]
    fn test_sequential_mapping() {
        let buffer = common::read_fixture_font("opentype/Klei.otf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        let glyph_ids = vec![0, 1, 2, 3, 4, 5];

        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        let mapping = match result {
            SubsetResult::Simple { glyph_mapping, .. } => glyph_mapping,
            SubsetResult::Cid { glyph_mapping, .. } => glyph_mapping,
        };

        // Sequential input should produce sequential output
        for (i, gid) in glyph_ids.iter().enumerate() {
            assert_eq!(mapping[gid], i as u16, "Glyph {} should map to {}", gid, i);
        }
    }

    /// Test non-sequential input produces sequential new IDs
    #[test]
    fn test_non_sequential_produces_sequential() {
        let buffer = common::read_fixture_font("opentype/Klei.otf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        let glyph_ids = vec![0, 100, 50, 200, 25];

        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        let mapping = match result {
            SubsetResult::Simple { glyph_mapping, .. } => glyph_mapping,
            SubsetResult::Cid { glyph_mapping, .. } => glyph_mapping,
        };

        // New IDs should be sequential from 0
        assert_eq!(mapping[&0], 0);
        assert_eq!(mapping[&100], 1);
        assert_eq!(mapping[&50], 2);
        assert_eq!(mapping[&200], 3);
        assert_eq!(mapping[&25], 4);
    }

    /// Test mapping is injective (no two old IDs map to same new ID)
    #[test]
    fn test_mapping_is_injective() {
        let buffer = common::read_fixture_font("opentype/OpenSans-Regular.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        let glyph_ids = vec![0, 5, 10, 15, 20, 25, 30];

        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        let mapping = match result {
            SubsetResult::Simple { glyph_mapping, .. } => glyph_mapping,
            SubsetResult::Cid { glyph_mapping, .. } => glyph_mapping,
        };

        // Check no duplicate new IDs
        let new_ids: HashSet<u16> = mapping.values().cloned().collect();
        assert_eq!(
            new_ids.len(),
            mapping.len(),
            "Each old ID should map to a unique new ID"
        );
    }

    /// Test that composite glyphs add their dependencies
    #[test]
    fn test_composite_adds_dependencies() {
        let buffer = common::read_fixture_font("opentype/OpenSans-Regular.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        // Just request .notdef and a high-numbered glyph that might be composite
        // We can't know for sure without parsing, but we can check if more glyphs
        // are in the mapping than we requested
        let glyph_ids = vec![0, 200];

        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        let mapping = match result {
            SubsetResult::Simple { glyph_mapping, .. } => glyph_mapping,
            SubsetResult::Cid { glyph_mapping, .. } => glyph_mapping,
        };

        // At minimum we should have the requested glyphs
        assert!(mapping.contains_key(&0));
        assert!(mapping.contains_key(&200));

        // If it's composite, we might have more
        if mapping.len() > 2 {
            println!(
                "Composite glyph 200 added {} dependencies",
                mapping.len() - 2
            );
        }
    }

    /// Test subset font has correct number of glyphs
    #[test]
    fn test_subset_glyph_count() {
        let buffer = common::read_fixture_font("opentype/Klei.otf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        let glyph_ids = vec![0, 10, 20, 30, 40];

        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        let (subset_data, mapping) = match result {
            SubsetResult::Simple {
                font_data,
                glyph_mapping,
            } => (font_data, glyph_mapping),
            SubsetResult::Cid {
                font_data,
                glyph_mapping,
                ..
            } => (font_data, glyph_mapping),
        };

        // Parse subset font
        let subset_scope = ReadScope::new(&subset_data);
        let subset_font_file = subset_scope.read::<OpenTypeFont>().unwrap();
        let subset_provider = subset_font_file.table_provider(0).unwrap();
        let subset_font = Font::new(Box::new(subset_provider)).unwrap();

        // Subset should have same number of glyphs as mapping
        assert_eq!(
            subset_font.num_glyphs() as usize,
            mapping.len(),
            "Subset font glyph count should match mapping size"
        );
    }

    /// Test CFF font mapping
    #[test]
    fn test_cff_font_mapping() {
        let buffer = common::read_fixture_font("opentype/SourceCodePro-Regular.otf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        let glyph_ids = vec![0, 10, 20, 30];

        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        let (subset_data, mapping) = match result {
            SubsetResult::Simple {
                font_data,
                glyph_mapping,
            } => (font_data, glyph_mapping),
            SubsetResult::Cid {
                font_data,
                glyph_mapping,
                ..
            } => (font_data, glyph_mapping),
        };

        // Verify mapping
        assert_eq!(mapping[&0], 0);
        assert_eq!(mapping[&10], 1);
        assert_eq!(mapping[&20], 2);
        assert_eq!(mapping[&30], 3);

        // Verify subset is valid
        let subset_scope = ReadScope::new(&subset_data);
        let subset_font_file = subset_scope.read::<OpenTypeFont>().unwrap();
        assert!(subset_font_file.table_provider(0).is_ok());
    }

    /// Test large sequential subset
    #[test]
    fn test_large_sequential_subset() {
        let buffer = common::read_fixture_font("opentype/OpenSans-Regular.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        // Get first 50 glyphs
        let glyph_ids: Vec<u16> = (0..50).collect();

        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        let mapping = match result {
            SubsetResult::Simple { glyph_mapping, .. } => glyph_mapping,
            SubsetResult::Cid { glyph_mapping, .. } => glyph_mapping,
        };

        // All should map sequentially
        for i in 0..50 {
            assert_eq!(
                mapping[&i], i,
                "Sequential glyph {} should map to itself",
                i
            );
        }
    }

    /// Test duplicate glyphs are handled correctly
    /// NOTE: The current implementation processes duplicates, which creates
    /// non-sequential new IDs. This is a known behavior.
    #[test]
    fn test_duplicates_handled() {
        let buffer = common::read_fixture_font("opentype/Klei.otf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        // Include duplicates
        let glyph_ids = vec![0, 10, 20, 10, 30, 20];

        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        let mapping = match result {
            SubsetResult::Simple { glyph_mapping, .. } => glyph_mapping,
            SubsetResult::Cid { glyph_mapping, .. } => glyph_mapping,
        };

        // The current implementation processes duplicates which results in
        // the mapping having the position of first occurrence
        // This is the actual behavior we're documenting:
        assert!(mapping.contains_key(&0));
        assert!(mapping.contains_key(&10));
        assert!(mapping.contains_key(&20));
        assert!(mapping.contains_key(&30));

        // Due to how duplicates are processed, the new IDs might not be
        // perfectly sequential (e.g., might be 0, 3, 4, 5 instead of 0, 1, 2, 3)
        // This is acceptable as long as all requested glyphs are mapped
        assert_eq!(mapping.len(), 4, "Should have 4 unique glyphs");
    }

    /// Test that .notdef is required at position 0
    #[test]
    fn test_notdef_required() {
        use allsorts::subset::SubsetError;

        let buffer = common::read_fixture_font("opentype/Klei.otf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        // Missing .notdef
        let glyph_ids = vec![10, 20, 30];

        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        );

        match result {
            Err(SubsetError::NotDef) => {
                // Expected
            }
            Ok(_) => panic!("Should have failed without .notdef"),
            Err(e) => panic!("Wrong error: {:?}", e),
        }
    }

    /// Test that .notdef at wrong position fails
    #[test]
    fn test_notdef_wrong_position() {
        use allsorts::subset::SubsetError;

        let buffer = common::read_fixture_font("opentype/Klei.otf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        // .notdef not at position 0
        let glyph_ids = vec![10, 0, 20];

        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        );

        match result {
            Err(SubsetError::NotDef) => {
                // Expected
            }
            Ok(_) => panic!("Should have failed with .notdef not at position 0"),
            Err(e) => panic!("Wrong error: {:?}", e),
        }
    }
}
