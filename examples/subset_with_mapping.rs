use allsorts::binary::read::ReadScope;
use allsorts::subset::{subset_and_map, CmapTarget, SubsetProfile};
use allsorts::tables::OpenTypeFont;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Read a font file
    let font_path = "tests/fonts/opentype/Klei.otf";
    let buffer = fs::read(font_path)?;
    let scope = ReadScope::new(&buffer);
    let font_file = scope.read::<OpenTypeFont>()?;
    let provider = font_file.table_provider(0)?;

    // Select glyphs to subset (must include glyph 0 as the first element)
    let glyph_ids = vec![0, 10, 20, 30, 40, 50];

    // Subset the font and get the mapping
    let (subset_data, mapping) = subset_and_map(
        &provider,
        &glyph_ids,
        &SubsetProfile::Pdf,
        CmapTarget::Unrestricted,
    )?;

    // Display the mapping
    println!("Glyph ID Mapping (old -> new):");
    let mut mappings: Vec<_> = mapping.iter().collect();
    mappings.sort_by_key(|&(old, _)| old);
    for (old_id, new_id) in mappings {
        println!("  {} -> {}", old_id, new_id);
    }

    // Save the subset font
    let output_path = "subset_font.otf";
    fs::write(output_path, subset_data)?;
    println!("\nSubset font saved to: {}", output_path);
    println!("Original font size: {} bytes", buffer.len());
    println!(
        "Subset font size: {} bytes",
        fs::metadata(output_path)?.len()
    );

    Ok(())
}
