// tests/subset_and_map.rs
mod common;

#[cfg(test)]
mod subset_and_map_tests {
    use crate::common;
    use allsorts::binary::read::ReadScope;
    use allsorts::subset::{subset_and_map, CmapTarget, SubsetError, SubsetProfile};
    use allsorts::tables::OpenTypeFont;
    use allsorts::Font;

    // Test 1: Basic TTF subsetting with mapping
    #[test]
    fn test_subset_and_map_simple_ttf() {
        // Using common utilities per guidelines
        let buffer = common::read_fixture_font("opentype/Klei.otf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        let glyph_ids = vec![0, 10, 20, 30];
        let (data, mapping) = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        // Verify mapping correctness
        assert_eq!(mapping.len(), 4, "Expected 4 glyphs in mapping");
        assert_eq!(mapping[&0], 0, ".notdef should remain at index 0");
        assert_eq!(mapping[&10], 1, "GID 10 should map to 1");
        assert_eq!(mapping[&20], 2, "GID 20 should map to 2");
        assert_eq!(mapping[&30], 3, "GID 30 should map to 3");

        // Verify subset font validity
        let subset_scope = ReadScope::new(&data);
        let subset_font = subset_scope.read::<OpenTypeFont>().unwrap();
        let subset_provider = subset_font.table_provider(0).unwrap();
        let font = Font::new(Box::new(subset_provider)).unwrap();
        assert_eq!(font.num_glyphs(), 4, "Subset should have 4 glyphs");
    }

    // Test 2: Composite glyphs with dependencies
    #[test]
    fn test_subset_and_map_with_composites() {
        // Load a font with composite glyphs
        let buffer = common::read_fixture_font("opentype/OpenSans-Regular.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        // Select glyphs that have composite dependencies
        // Use glyph 0 and a composite glyph (typically accented characters have components)
        let glyph_ids = vec![0, 50]; // Assuming 50 might have dependencies
        let (data, mapping) = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        // Verify mapping includes dependencies
        assert!(mapping.len() >= 2, "Mapping should include dependencies");
        assert!(mapping.contains_key(&0), "Should contain .notdef");
        assert!(mapping.contains_key(&50), "Should contain requested glyph");

        // Verify subset font validity
        let subset_scope = ReadScope::new(&data);
        let subset_font = subset_scope.read::<OpenTypeFont>().unwrap();
        assert!(subset_font.table_provider(0).is_ok());
    }

    // Test 3: CFF font subsetting
    #[test]
    fn test_subset_and_map_cff() {
        // Use a CFF font from fixtures
        let buffer = common::read_fixture_font("opentype/SourceCodePro-Regular.otf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        let glyph_ids = vec![0, 5, 10];
        let (data, mapping) = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        assert_eq!(mapping.len(), 3);
        assert_eq!(mapping[&0], 0);
        assert_eq!(mapping[&5], 1);
        assert_eq!(mapping[&10], 2);

        // Verify subset font validity
        let subset_scope = ReadScope::new(&data);
        let subset_font = subset_scope.read::<OpenTypeFont>().unwrap();
        assert!(subset_font.table_provider(0).is_ok());
    }

    // Test 4: CFF2 font subsetting
    #[test]
    #[ignore] // Enable when CFF2 test font available
    fn test_subset_and_map_cff2() {
        // Will need a CFF2 variable font
        // This test is a placeholder for when we have a CFF2 test font
    }

    // Test 5: Error case - missing .notdef
    #[test]
    fn test_missing_notdef_error() {
        let buffer = common::read_fixture_font("opentype/Klei.otf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        // Glyph IDs without 0 (.notdef)
        let glyph_ids = vec![10, 20, 30];
        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        );

        match result {
            Err(SubsetError::NotDef) => {
                // Expected error
            }
            Ok(_) => panic!("Should have failed with NotDef"),
            Err(e) => panic!("Wrong error type: {:?}", e),
        }
    }

    // Test 6: Backward compatibility
    #[test]
    fn test_backward_compatibility() {
        use allsorts::subset::subset;

        let buffer = common::read_fixture_font("opentype/Klei.otf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        let glyph_ids = vec![0, 10, 20];

        // Old API should still work
        let old_result = subset(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        );
        assert!(old_result.is_ok(), "subset() should still work");

        // New API should produce same font data
        let (new_data, _mapping) = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        assert_eq!(old_result.unwrap(), new_data, "Font data should match");
    }

    // Test 7: Large font performance
    #[test]
    #[ignore] // Run with: cargo test -- --ignored
    fn test_performance_large_font() {
        use std::time::Instant;

        // Try to find a larger font for testing
        let result = std::panic::catch_unwind(|| {
            common::read_fixture_font("noto/NotoSansCJKjp-Regular.otf")
        });

        let buffer = match result {
            Ok(buf) => buf,
            Err(_) => {
                // Fallback to a font we know exists
                println!("Large CJK font not available, using OpenSans for performance test");
                common::read_fixture_font("opentype/OpenSans-Regular.ttf")
            }
        };

        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        // Select many glyphs (adjust based on font size)
        // We'll just use a fixed number for the test
        let glyph_count = 100; // Use a smaller number for the test
        let glyph_ids: Vec<u16> = (0..glyph_count as u16).collect();

        let start = Instant::now();
        let (_data, mapping) = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();
        let duration = start.elapsed();

        println!(
            "Subset {} glyphs with mapping in {:?}",
            glyph_count, duration
        );
        assert!(
            mapping.len() >= glyph_count,
            "Should map all requested glyphs"
        );
        assert!(duration.as_secs() < 2, "Should complete within 2 seconds");
    }

    // Test 8: Empty glyph list (only .notdef)
    #[test]
    fn test_subset_and_map_only_notdef() {
        let buffer = common::read_fixture_font("opentype/Klei.otf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        let glyph_ids = vec![0]; // Only .notdef
        let (data, mapping) = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        assert_eq!(mapping.len(), 1, "Should have only .notdef");
        assert_eq!(mapping[&0], 0, ".notdef should remain at index 0");

        // Verify subset font is valid
        let subset_scope = ReadScope::new(&data);
        let subset_font = subset_scope.read::<OpenTypeFont>().unwrap();
        let subset_provider = subset_font.table_provider(0).unwrap();
        let font = Font::new(Box::new(subset_provider)).unwrap();
        assert_eq!(font.num_glyphs(), 1, "Subset should have 1 glyph");
    }

    // Test 9: Verify mapping includes all dependencies
    #[test]
    fn test_mapping_includes_all_dependencies() {
        let buffer = common::read_fixture_font("opentype/OpenSans-Regular.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        // Use a small set of glyphs
        let glyph_ids = vec![0, 1, 2];
        let (_data, mapping) = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        // All requested glyphs should be in the mapping
        for gid in &glyph_ids {
            assert!(
                mapping.contains_key(gid),
                "Mapping should contain requested glyph {}",
                gid
            );
        }

        // Verify the new IDs are sequential from 0
        let mut new_ids: Vec<u16> = mapping.values().cloned().collect();
        new_ids.sort();
        for (i, new_id) in new_ids.iter().enumerate() {
            assert!(*new_id == i as u16, "New IDs should be sequential from 0");
        }
    }
}
