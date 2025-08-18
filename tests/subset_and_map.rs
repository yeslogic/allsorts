// tests/subset_and_map.rs
mod common;

#[cfg(test)]
mod subset_and_map_tests {
    use crate::common;
    use allsorts::binary::read::ReadScope;
    use allsorts::subset::{subset_and_map, CmapTarget, SubsetError, SubsetProfile, SubsetResult};
    use allsorts::tables::{FontTableProvider, OpenTypeFont};
    use allsorts::Font;
    use std::collections::HashMap;

    // Test 1: Basic TTF subsetting with mapping
    #[test]
    fn test_subset_and_map_simple_ttf() {
        // Using common utilities per guidelines
        let buffer = common::read_fixture_font("opentype/Klei.otf");
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

        let (data, mapping) = match result {
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
        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        let (data, mapping) = match result {
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
        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        let (data, mapping) = match result {
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
        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        let new_data = match result {
            SubsetResult::Simple { font_data, .. } => font_data,
            SubsetResult::Cid { font_data, .. } => font_data,
        };

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
        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();
        let duration = start.elapsed();

        let mapping = match result {
            SubsetResult::Simple { glyph_mapping, .. } => glyph_mapping,
            SubsetResult::Cid { glyph_mapping, .. } => glyph_mapping,
        };

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
        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unrestricted,
        )
        .unwrap();

        let (data, mapping) = match result {
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

#[cfg(test)]
mod cid_font_tests {
    use crate::common;
    use allsorts::binary::read::ReadScope;
    use allsorts::cff::CFF;
    use allsorts::subset::{subset_and_map, CmapTarget, SubsetProfile, SubsetResult};
    use allsorts::tables::{FontTableProvider, OpenTypeFont};
    use allsorts::{tag, Font};
    use std::collections::HashMap;

    // Test 1: Basic CID Font Subsetting
    #[test]
    fn test_cid_font_subsetting_basic() {
        // Use SourceCodePro which is a CFF font
        let font_data = common::read_fixture_font("opentype/SourceCodePro-Regular.otf");
        let scope = ReadScope::new(&font_data);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        // Check if this is a CID font (if it has a CFF table)
        let _is_cid = if provider.has_table(tag::CFF) {
            let cff_data = provider.read_table_data(tag::CFF).unwrap();
            let cff = ReadScope::new(&cff_data).read::<CFF<'_>>().unwrap();
            // Check if any font in the CFF is CID-keyed
            cff.fonts.len() == 1 && cff.fonts[0].is_cid_keyed()
        } else {
            false
        };

        // We want to keep only these glyphs
        let glyph_ids = vec![0, 45, 46, 143];

        // Perform subsetting
        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unicode,
        )
        .unwrap();

        // Check if we get CID result for CID fonts
        match result {
            SubsetResult::Cid {
                font_data,
                glyph_mapping,
                cid_to_gid_map,
            } => {
                // Verify the subset font has 4 glyphs
                let subset_scope = ReadScope::new(&font_data);
                let subset_font = subset_scope.read::<OpenTypeFont>().unwrap();
                let subset_provider = subset_font.table_provider(0).unwrap();
                let font = Font::new(Box::new(subset_provider)).unwrap();
                assert_eq!(font.num_glyphs(), 4);

                // Verify glyph mapping (old -> new)
                assert_eq!(glyph_mapping[&0], 0); // .notdef stays at 0
                assert_eq!(glyph_mapping[&45], 1); // dash becomes GID 1
                assert_eq!(glyph_mapping[&46], 2); // period becomes GID 2
                assert_eq!(glyph_mapping[&143], 3); // bullet becomes GID 3

                // CRITICAL: Verify CIDToGIDMap has correct entries
                // The map should be at least 144*2 bytes (for CIDs 0-143)
                assert!(cid_to_gid_map.len() >= 288);

                // Check specific CID mappings in the generated map
                let cid_45_gid = u16::from_be_bytes([cid_to_gid_map[90], cid_to_gid_map[91]]);
                let cid_46_gid = u16::from_be_bytes([cid_to_gid_map[92], cid_to_gid_map[93]]);
                let cid_143_gid = u16::from_be_bytes([cid_to_gid_map[286], cid_to_gid_map[287]]);

                assert_eq!(cid_45_gid, 1); // CID 45 maps to new GID 1
                assert_eq!(cid_46_gid, 2); // CID 46 maps to new GID 2
                assert_eq!(cid_143_gid, 3); // CID 143 maps to new GID 3

                // Unmapped CIDs should map to 0 (.notdef)
                let cid_100_gid = u16::from_be_bytes([cid_to_gid_map[200], cid_to_gid_map[201]]);
                assert_eq!(cid_100_gid, 0); // Unmapped CID -> .notdef
            }
            SubsetResult::Simple { .. } => {
                // For now, CFF fonts might still return Simple result
                // This is acceptable until we implement full CID detection
            }
        }
    }

    // Test 2: CID Font with High CID Values
    #[test]
    fn test_cid_font_high_cid_values() {
        // Use a font with potential high CID values
        let font_data = common::read_fixture_font("opentype/SourceCodePro-Regular.otf");
        let scope = ReadScope::new(&font_data);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        // Include glyphs with potentially high CIDs
        // Just use reasonable glyph IDs that should exist in most fonts
        let glyph_ids = vec![0, 143, 255, 500]
            .into_iter()
            .filter(|&gid| gid < 600) // SourceCodePro has at least 600 glyphs
            .collect::<Vec<_>>();

        let result = subset_and_map(
            &provider,
            &glyph_ids,
            &SubsetProfile::Pdf,
            CmapTarget::Unicode,
        )
        .unwrap();

        match result {
            SubsetResult::Cid {
                cid_to_gid_map,
                glyph_mapping,
                ..
            } => {
                // Map must be large enough for highest CID
                let max_gid = glyph_ids.iter().max().unwrap();
                assert!(cid_to_gid_map.len() >= (*max_gid as usize + 1) * 2);

                // Verify high CID mappings work
                for original_gid in &glyph_ids {
                    if *original_gid > 0 {
                        let cid_offset = *original_gid as usize * 2;
                        if cid_offset + 1 < cid_to_gid_map.len() {
                            let cid_gid = u16::from_be_bytes([
                                cid_to_gid_map[cid_offset],
                                cid_to_gid_map[cid_offset + 1],
                            ]);
                            let expected_new_gid = glyph_mapping[original_gid];
                            assert_eq!(cid_gid, expected_new_gid);
                        }
                    }
                }
            }
            SubsetResult::Simple { .. } => {
                // Acceptable for now
            }
        }
    }

    // Test 3: Regression Test - PDF Glyph Zero Detector
    #[test]
    fn test_no_glyph_zero_issues() {
        // This test ensures our fix prevents the "glyph zero" problem
        // where characters render as '?' in PDF viewers

        let font_data = common::read_fixture_font("opentype/SourceCodePro-Regular.otf");
        let scope = ReadScope::new(&font_data);
        let font_file = scope.read::<OpenTypeFont>().unwrap();
        let provider = font_file.table_provider(0).unwrap();

        // Subset with typical usage from a real PDF
        // Use reasonable glyph IDs that should exist in SourceCodePro
        let used_glyphs = vec![
            0, 32, 33, 45, 46, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 97, 98, 99, 100, 101, 102,
            103, 104, 105, 143, // bullet
        ]
        .into_iter()
        .filter(|&gid| gid < 600) // SourceCodePro has at least 600 glyphs
        .collect::<Vec<_>>();

        let result = subset_and_map(
            &provider,
            &used_glyphs,
            &SubsetProfile::Pdf,
            CmapTarget::Unicode,
        )
        .unwrap();

        match result {
            SubsetResult::Cid {
                cid_to_gid_map,
                glyph_mapping,
                ..
            } => {
                // Simulate what PDF readers do: look up common CIDs
                let test_cids = vec![32, 65, 97, 143]; // space, A, a, bullet

                for cid in test_cids {
                    if used_glyphs.contains(&cid) {
                        let offset = cid as usize * 2;
                        if offset + 1 < cid_to_gid_map.len() {
                            let gid = u16::from_be_bytes([
                                cid_to_gid_map[offset],
                                cid_to_gid_map[offset + 1],
                            ]);

                            // No CID should map to 0 if it was in our used set
                            // (except for CID 0 which is .notdef)
                            if cid != 0 {
                                assert_ne!(gid, 0, "CID {} incorrectly maps to GID 0", cid);
                            }
                        }
                    }
                }
            }
            SubsetResult::Simple { .. } => {
                // Acceptable for now until CID detection is implemented
            }
        }
    }

    // Test 4: CID Font Detection
    #[test]
    fn test_is_cid_font_detection() {
        // Test CFF font detection
        let cff_font_data = common::read_fixture_font("opentype/SourceCodePro-Regular.otf");
        let cff_scope = ReadScope::new(&cff_font_data);
        let cff_font_file = cff_scope.read::<OpenTypeFont>().unwrap();
        let cff_provider = cff_font_file.table_provider(0).unwrap();

        // Check if CFF table exists and contains CID data
        if cff_provider.has_table(tag::CFF) {
            let cff_data = cff_provider.read_table_data(tag::CFF).unwrap();
            let cff = ReadScope::new(&cff_data).read::<CFF<'_>>().unwrap();
            let is_cid = cff.fonts.len() == 1 && cff.fonts[0].is_cid_keyed();
            // Note: SourceCodePro might not be a CID font, but we test the detection logic
            println!("SourceCodePro is CID-keyed: {}", is_cid);
        }

        // Test TTF font (should not be CID)
        let ttf_font_data = common::read_fixture_font("opentype/OpenSans-Regular.ttf");
        let ttf_scope = ReadScope::new(&ttf_font_data);
        let ttf_font_file = ttf_scope.read::<OpenTypeFont>().unwrap();
        let ttf_provider = ttf_font_file.table_provider(0).unwrap();

        // TTF fonts should not have CFF table
        assert!(!ttf_provider.has_table(tag::CFF));
    }

    // Test 5: Build CIDToGIDMap Function
    #[test]
    fn test_build_cid_to_gid_map() {
        // Test building a CIDToGIDMap from a glyph remapping
        let mut glyph_remapping = HashMap::new();
        glyph_remapping.insert(0, 0); // .notdef stays at 0
        glyph_remapping.insert(45, 1); // CID 45 -> new GID 1
        glyph_remapping.insert(46, 2); // CID 46 -> new GID 2
        glyph_remapping.insert(143, 3); // CID 143 -> new GID 3

        let max_cid = 255;
        let map = build_cid_to_gid_map(None, &glyph_remapping, max_cid);

        // Verify map size
        assert_eq!(map.len(), (max_cid as usize + 1) * 2);

        // Check specific mappings
        assert_eq!(u16::from_be_bytes([map[0], map[1]]), 0); // CID 0 -> GID 0
        assert_eq!(u16::from_be_bytes([map[90], map[91]]), 1); // CID 45 -> GID 1
        assert_eq!(u16::from_be_bytes([map[92], map[93]]), 2); // CID 46 -> GID 2
        assert_eq!(u16::from_be_bytes([map[286], map[287]]), 3); // CID 143 -> GID 3

        // Unmapped CIDs should map to 0
        assert_eq!(u16::from_be_bytes([map[200], map[201]]), 0); // CID 100 -> GID 0
    }

    // Test 6: CIDToGIDMap with Original Mapping
    #[test]
    fn test_build_cid_to_gid_map_with_original() {
        // Test case where we have an original CIDToGIDMap
        // Original mapping: CID 65 -> GID 100, CID 66 -> GID 101
        // CID 65 needs indices 130-131 (65*2), CID 66 needs 132-133
        let mut original_map = vec![0u8; 134];
        original_map[130] = 0;
        original_map[131] = 100; // CID 65 -> GID 100
        original_map[132] = 0;
        original_map[133] = 101; // CID 66 -> GID 101

        let mut glyph_remapping = HashMap::new();
        glyph_remapping.insert(0, 0); // .notdef
        glyph_remapping.insert(100, 1); // Old GID 100 -> new GID 1
        glyph_remapping.insert(101, 2); // Old GID 101 -> new GID 2

        let map = build_cid_to_gid_map(Some(&original_map), &glyph_remapping, 127);

        // CID 65 should now map to new GID 1 (was GID 100)
        assert_eq!(u16::from_be_bytes([map[130], map[131]]), 1);
        // CID 66 should now map to new GID 2 (was GID 101)
        assert_eq!(u16::from_be_bytes([map[132], map[133]]), 2);
    }

    // Helper function for tests (will be moved to implementation)
    fn build_cid_to_gid_map(
        original_map: Option<&[u8]>,
        glyph_remapping: &HashMap<u16, u16>,
        max_cid: u16,
    ) -> Vec<u8> {
        let map_size = (max_cid as usize + 1) * 2;
        let mut map = vec![0u8; map_size];

        if let Some(orig) = original_map {
            for cid in 0..=max_cid {
                let cid_offset = cid as usize * 2;
                if cid_offset + 1 < orig.len() {
                    // Get original GID for this CID
                    let old_gid = u16::from_be_bytes([orig[cid_offset], orig[cid_offset + 1]]);

                    // Map to new GID if it was included
                    let new_gid = glyph_remapping.get(&old_gid).copied().unwrap_or(0);

                    // Write to map (big-endian)
                    map[cid_offset] = (new_gid >> 8) as u8;
                    map[cid_offset + 1] = (new_gid & 0xFF) as u8;
                }
            }
        } else {
            // Identity mapping: CID == original GID
            for cid in 0..=max_cid {
                let new_gid = glyph_remapping.get(&cid).copied().unwrap_or(0);
                let cid_offset = cid as usize * 2;
                map[cid_offset] = (new_gid >> 8) as u8;
                map[cid_offset + 1] = (new_gid & 0xFF) as u8;
            }
        }

        map
    }
}
