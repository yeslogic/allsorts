
// -----------------------------------------------------------------------------
// Data types
//
// <https://www.microsoft.com/typography/otspec/otff.htm#dataTypes>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6.html#Types>
// -----------------------------------------------------------------------------

// FIXME: we need wrapper structs to dodge yeslogic/ddl#80

/// 32-bit signed fixed-point number (16.16)
Fixed = struct { value : u32be };

/// `i16be` that describes a quantity in font design units.
FWord = struct { value : i16be };

/// `u16be` that describes a quantity in font design units.
UfWord = struct { value : u16be };

/// Date represented in number of seconds since 12:00 midnight, January 1, 1904.
/// The value is represented as a signed 64-bit integer.
LongDateTime = struct { value : i64be };

/// Array of four uint8s (length = 32 bits) used to identify a script, language
/// system, feature, or baseline
Tag = struct { data : [u8; 4u8] };

/// Short offset to a table, same as `u16be`, NULL offset = 0x0000
Offset16 = struct { address : u16be };

/// Long offset to a table, same as `u32be`, NULL offset = 0x00000000
Offset32 = struct { address : u32be };



// =============================================================================
//
// OpenType Layout Common Table Formats
//
// <https://www.microsoft.com/typography/otspec/chapter2.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6cmap.html>
//
// =============================================================================


// -----------------------------------------------------------------------------
// Scripts and Languages
//
// <https://www.microsoft.com/typography/otspec/chapter2.htm#scriptsLangs>
// -----------------------------------------------------------------------------

/// Script Record
///
/// <https://www.microsoft.com/typography/otspec/chapter2.htm#scriptRec>
ScriptRecord = struct {
    /// 4-byte script tag identifier
    script_tag : Tag,
    /// Offset to Script table, from beginning of ScriptList
    script_offset : Offset16,
};

/// Script List Table
///
/// <https://www.microsoft.com/typography/otspec/chapter2.htm#scriptList>
ScriptList = struct {
    /// Number of ScriptRecords
    script_count : u16be,
    /// Array of ScriptRecords, listed alphabetically by script tag
    script_records : [ScriptRecord; script_count],
};


/// Language System Record
///
/// <https://www.microsoft.com/typography/otspec/chapter2.htm#langSysRec>
LangSysRecord = struct {
    /// 4-byte LangSysTag identifier
    lang_sys_tag : Tag,
    /// Offset to LangSys table, from beginning of Script table
    lang_sys_offset : Offset16,
};

/// Script Table
///
/// <https://www.microsoft.com/typography/otspec/chapter2.htm#scriptTable>
Script = struct {
    /// Offset to default LangSys table, from beginning of Script table — may be NULL
    default_lang_sys : Offset16,
    /// Number of LangSysRecords for this script — excluding the default LangSys
    lang_sys_count : u16be,
    /// Array of LangSysRecords, listed alphabetically by LangSys tag
    lang_sys_records : [LangSysRecord; lang_sys_count],
};


/// Language System Table
///
/// <https://www.microsoft.com/typography/otspec/chapter2.htm#langSysTable>
LangSys = struct {
    /// = NULL (reserved for an offset to a reordering table)
    lookup_order : Offset16,
    /// Index of a feature required for this language system; if no required
    /// features = 0xFFFF
    required_feature_index : u16be,
    /// Number of feature index values for this language system — excludes the
    /// required feature
    feature_index_count : u16be,
    /// Array of indices into the FeatureList, in arbitrary order
    feature_indices : [u16be; feature_index_count],
};


// -----------------------------------------------------------------------------
// Features and Lookups
//
// <https://www.microsoft.com/typography/otspec/chapter2.htm#feats_lookups>
// -----------------------------------------------------------------------------

/// Feature Record
///
/// <https://www.microsoft.com/typography/otspec/chapter2.htm#featureRec>
FeatureRecord = struct {
    /// 4-byte feature identification tag
    feature_tag : Tag,
    /// Offset to Feature table, from beginning of FeatureList
    feature_offset : Offset16,
};

/// Feature List table
///
/// <https://www.microsoft.com/typography/otspec/chapter2.htm#featureList>
FeatureList = struct {
    /// Number of FeatureRecords in this table
    feature_count : u16be,
    /// Array of FeatureRecords — zero-based (first feature has
    /// FeatureIndex = 0), listed alphabetically by feature tag
    feature_records : [FeatureRecord; feature_count],
};


/// Feature Table
///
/// <https://www.microsoft.com/typography/otspec/chapter2.htm#featureTable>
Feature = struct {
    /// = NULL (reserved for offset to FeatureParams)
    feature_params : Offset16,
    /// Number of LookupList indices for this feature
    lookup_index_count : u16be,
    /// Array of indices into the LookupList — zero-based (first lookup is
    /// LookupListIndex = 0)
    lookup_list_indices : [u16be; lookup_index_count],
};


/// Lookup List Table
///
/// <https://www.microsoft.com/typography/otspec/chapter2.htm#lookupList>
LookupList = struct {
    /// Number of lookups in this table
    lookup_count : u16be,
    /// Array of offsets to Lookup tables, from beginning of LookupList — zero
    /// based (first lookup is Lookup index = 0)
    lookups : [Offset16; lookup_count],
};


// FIXME: enumerations
// /// LookupFlag bit enumeration
// ///
// /// <https://www.microsoft.com/typography/otspec/chapter2.htm#lookupFlags>
// enum LookupFlag : u16 {
//     /// This bit relates only to the correct processing of the cursive
//     /// attachment lookup type (GPOS lookup type 3). When this bit is set, the
//     /// last glyph in a given sequence to which the cursive attachment lookup is
//     /// applied, will be positioned on the baseline.
//     ///
//     /// Note: Setting of this bit is not intended to be used by operating systems
//     /// or applications to determine text direction.
//     right_to_left = 0x0001,
//     /// If set, skips over base glyphs
//     ignore_base_glyphs = 0x0002,
//     /// If set, skips over ligatures
//     ignore_ligatures = 0x0004,
//     /// If set, skips over all combining marks
//     ignore_marks = 0x0008,
//     /// If set, indicates that the lookup table structure is followed by a
//     /// MarkFilteringSet field. The layout engine skips over all mark glyphs not
//     /// in the mark filtering set indicated.
//     use_mark_filtering_set = 0x0010,
//     /// For future use (Set to zero)
//     reserved = 0x00E0,
//     /// If not zero, skips over all marks of attachment type different from
//     /// specified.
//     mark_attachment_type = 0xFF00,
// };

/// Lookup Table
///
/// <https://www.microsoft.com/typography/otspec/chapter2.htm#lookupTable>
Lookup = struct {
    /// Different enumerations for GSUB and GPOS
    lookup_type : u16be,
    /// Lookup qualifiers
    lookup_flag : u16be,
    // FIXME: lookup_flag : LookupFlag,
    /// Number of subtables for this lookup
    sub_table_count : u16be,
    /// Array of offsets to lookup subtables, from beginning of Lookup table
    subtable_offsets : [Offset16; sub_table_count],
    /// Index (base 0) into GDEF mark glyph sets structure. This field is only
    /// present if bit useMarkFilteringSet of lookup flags is set.
    mark_filtering_set : u16be,
};

/// Coverage Format 1 table: Individual glyph indices
CoverageFormat1 = struct {
    /// Format identifier — format = 1
    coverage_format : u16be,
    /// Number of glyphs in the glyph array
    glyph_count : u16be,
    /// Array of glyph IDs — in numerical order
    glyph_array : [u16be; glyph_count],
};

/// Range Record
RangeRecord = struct {
    /// First glyph ID in the range
    start_glyph_id : u16be,
    /// Last glyph ID in the range
    end_glyph_id : u16be,
    /// Coverage Index of first glyph ID in range
    start_coverage_index : u16be,
};

/// Coverage Format 2 table: Range of glyphs
CoverageFormat2 = struct {
    /// Format identifier — format = 2
    coverage_format : u16be,
    /// Number of RangeRecords
    range_count : u16be,
    /// Array of glyph ranges — ordered by startGlyphID.
    range_records : [RangeRecord; range_count],
};

/// Coverage Table
///
/// <https://www.microsoft.com/typography/otspec/chapter2.htm#coverageTbl
Coverage = struct {
    /// Format identifier
    coverage_format : u16be,
    body : cond {
        format1 : coverage_format == 1u16 => CoverageFormat1,
        format2 : coverage_format == 2u16 => CoverageFormat2,
    },
};


// -----------------------------------------------------------------------------
// Class Definition Table
//
// <https://www.microsoft.com/typography/otspec/chapter2.htm#CDT>
// -----------------------------------------------------------------------------

// TODO


// -----------------------------------------------------------------------------
// Device and VariationIndex Tables
//
// <https://www.microsoft.com/typography/otspec/chapter2.htm#devVarIdxTbls>
// -----------------------------------------------------------------------------

// TODO


// -----------------------------------------------------------------------------
// FeatureVariations Table
//
// <https://www.microsoft.com/typography/otspec/chapter2.htm#featVarTbl>
// -----------------------------------------------------------------------------

// TODO


// -----------------------------------------------------------------------------
// ConditionSet Table
//
// <https://www.microsoft.com/typography/otspec/chapter2.htm#condSetTbl>
// -----------------------------------------------------------------------------

// TODO


// -----------------------------------------------------------------------------
// Condition Table
//
// <https://www.microsoft.com/typography/otspec/chapter2.htm#condTbl>
// -----------------------------------------------------------------------------

// TODO


// -----------------------------------------------------------------------------
// FeatureTableSubstitution Table
//
// <https://www.microsoft.com/typography/otspec/chapter2.htm#ftsTbl>
// -----------------------------------------------------------------------------

// TODO



// =============================================================================
//
// cmap - Character To Glyph Index Mapping Table
//
// <https://www.microsoft.com/typography/otspec/cmap.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6cmap.html>
//
// =============================================================================


// -----------------------------------------------------------------------------
// FORMAT 0
//
// <https://www.microsoft.com/typography/otspec/cmap.htm#format0>
// -----------------------------------------------------------------------------

/// Format 0: Byte encoding table
///
/// <https://www.microsoft.com/typography/otspec/cmap.htm#subtable_0>
CMapSubtable0 = struct {
    /// This is the length in bytes of the subtable.
    length : u16be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://www.microsoft.com/typography/otspec/cmap.htm#language)".
    language : u16be,
    /// An array that maps character codes to glyph index values.
    glyph_id_array : [u8; 256u16],
};


// -----------------------------------------------------------------------------
// FORMAT 2
//
// <https://www.microsoft.com/typography/otspec/cmap.htm#format2>
// -----------------------------------------------------------------------------

CMapSubtable2SubHeader = struct {
    /// First valid low byte for this SubHeader.
    first_code : u16be,
    /// Number of valid low bytes for this SubHeader.
    entry_count : u16be,
    /// See text below.
    id_delta : i16be,
    /// See text below.
    id_range_offset : u16be,
};

/// Format 2: High-byte mapping through table
///
/// <https://www.microsoft.com/typography/otspec/cmap.htm#subtable_2>
CMapSubtable2 = struct {
    /// This is the length in bytes of the subtable.
    length : u16be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://www.microsoft.com/typography/otspec/cmap.htm#language)".
    language : u16be,
    /// Array that maps high bytes to subHeaders: value is subHeader index * 8.
    sub_header_keys : [u16be; 256u16],
    // TODO
    // /// Variable-length array of `CMapSubtable2SubHeader` records.
    // subHeaders : [CMapSubtable2SubHeader],
    // /// Variable-length array containing subarrays used for mapping the low byte
    // /// of 2-byte characters.
    // glyphIndexArray : [u16be],
};


// -----------------------------------------------------------------------------
// FORMAT 4
//
// <https://www.microsoft.com/typography/otspec/cmap.htm#format4>
// -----------------------------------------------------------------------------

/// Format 4: Segment mapping to delta values
///
/// <https://www.microsoft.com/typography/otspec/cmap.htm#subtable_4>
CMapSubtable4 = struct {
    /// This is the length in bytes of the subtable.
    length : u16be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://www.microsoft.com/typography/otspec/cmap.htm#language)".
    language : u16be,
    /// `2 x seg_count.`
    seg_count_x2 : u16be,
    /// `2 x (2**floor(log2(seg_count)))`
    search_range : u16be,
    /// `log2(search_range/2)`
    entry_selector : u16be,
    /// `2 x seg_count - search_range`
    range_shift : u16be,
    /// End characterCode for each segment, `last = 0xFFFF`.
    end_count : [u16be; seg_count_x2 / 2u16],
    /// Set to `0`.
    reserved_pad : u16be,
    /// Start character code for each segment.
    start_count : [u16be; seg_count_x2 / 2u16],
    /// Delta for all character codes in segment.
    id_delta : [i16be; seg_count_x2 / 2u16],
    /// Offsets into `glyph_id_array` or 0
    id_range_offset : [u16be; seg_count_x2 / 2u16],
    /// Glyph index array (arbitrary length)
    glyph_id_array : [u16be; (length / 2u16 - 8u16) - (2u16 * seg_count_x2)],
};


// -----------------------------------------------------------------------------
// FORMAT 6
//
// <https://www.microsoft.com/typography/otspec/cmap.htm#format6>
// -----------------------------------------------------------------------------

/// Format 6: Trimmed table mapping
///
/// <https://www.microsoft.com/typography/otspec/cmap.htm#subtable_6>
CMapSubtable6 = struct {
    /// This is the length in bytes of the subtable.
    length : u16be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://www.microsoft.com/typography/otspec/cmap.htm#language)".
    language : u16be,
    /// First character code of subrange.
    first_code : u16be,
    /// Number of character codes in subrange.
    entry_count : u16be,
    /// Array of glyph index values for character codes in the range.
    glyph_id_array : [u16be; entry_count],
};


// -----------------------------------------------------------------------------
// FORMAT 8
//
// <https://www.microsoft.com/typography/otspec/cmap.htm#format8>
// -----------------------------------------------------------------------------

CMapSubtable8SequentialMapGroup = struct {
    /// First character code in this group; note that if this group is for one
    /// or more 16-bit character codes (which is determined from the is32
    /// array), this 32-bit value will have the high 16-bits set to zero
    start_char_code : u32be,
    /// Last character code in this group; same condition as listed above for
    /// the `start_char_code`
    end_char_code : u32be,
    /// Glyph index corresponding to the starting character code
    start_glyph_id : u32be,
};

/// Format 8: mixed 16-bit and 32-bit coverage
///
/// <https://www.microsoft.com/typography/otspec/cmap.htm#subtable_8>
CMapSubtable8 = struct {
    /// Reserved; set to 0
    reserved : u16be,
    /// Byte length of this subtable (including the header)
    length : u32be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://www.microsoft.com/typography/otspec/cmap.htm#language)".
    language : u32be,
    /// Tightly packed array of bits (8K bytes total) indicating whether the
    /// particular 16-bit (index) value is the start of a 32-bit character code
    is32 : [u8; 8192u16],
    /// Number of groupings which follow
    num_groups : u32be,
    /// Array of SequentialMapGroup records.
    groups : [CMapSubtable8SequentialMapGroup; num_groups],
};


// -----------------------------------------------------------------------------
// FORMAT 10
//
// <https://www.microsoft.com/typography/otspec/cmap.htm#format10>
// -----------------------------------------------------------------------------

/// Format 10: Trimmed array
///
/// <https://www.microsoft.com/typography/otspec/cmap.htm#subtable_10>
CmapSubtable10 = struct {
    /// Reserved; set to 0
    reserved : u16be,
    /// Byte length of this subtable (including the header)
    length : u32be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://www.microsoft.com/typography/otspec/cmap.htm#language)".
    language : u32be,
    /// First character code covered
    start_char_code : u32be,
    /// Number of character codes covered
    num_chars : u32be,
    // TODO:
    // /// Array of glyph indices for the character codes covered
    // glyphs : [u16be],
};


// -----------------------------------------------------------------------------
// FORMAT 12
//
// <https://www.microsoft.com/typography/otspec/cmap.htm#format12>
// -----------------------------------------------------------------------------

CMapSubtable12SequentialMapGroup = struct {
    /// First character code in this group
    start_char_code : u32be,
    /// Last character code in this group
    end_char_code : u32be,
    /// Glyph index corresponding to the starting character code
    start_glyph_id : u32be,
};

/// Format 12: Segmented coverage
///
/// <https://www.microsoft.com/typography/otspec/cmap.htm#subtable_12>
CMapSubtable12 = struct {
    /// Reserved; set to 0
    reserved : u16be,
    /// Byte length of this subtable (including the header)
    length : u32be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://www.microsoft.com/typography/otspec/cmap.htm#language)".
    language : u32be,
    /// Number of groupings which follow
    num_groups : u32be,
    /// Array of `CMapSubtable12SequentialMapGroup` records.
    groups : [CMapSubtable12SequentialMapGroup; num_groups],
};


// -----------------------------------------------------------------------------
// FORMAT 13
//
// <https://www.microsoft.com/typography/otspec/cmap.htm#format13>
// -----------------------------------------------------------------------------

CMapSubtable13ConstantMapGroup = struct {
    /// First character code in this group
    start_char_code : u32be,
    /// Last character code in this group
    end_char_code : u32be,
    /// Glyph index to be used for all the characters in the group's range.
    start_glyph_id : u32be,
};

/// Format 13: Many-to-one range mappings
///
/// <https://www.microsoft.com/typography/otspec/cmap.htm#subtable_13>
CMapSubtable13 = struct {
    /// Reserved; set to 0
    reserved : u16be,
    /// Byte length of this subtable (including the header)
    length : u32be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://www.microsoft.com/typography/otspec/cmap.htm#language)".
    language : u32be,
    /// Number of groupings which follow
    num_groups : u32be,
    /// Array of `CMapSubtable13ConstantMapGroup` records.
    groups : [CMapSubtable13ConstantMapGroup; num_groups],
};


// -----------------------------------------------------------------------------
// FORMAT 14
//
// <https://www.microsoft.com/typography/otspec/cmap.htm#format14>
// -----------------------------------------------------------------------------

CMapSubtable14VariationSelector = struct {
    /// Variation selector
    var_selector : u24be,
    /// Offset from the start of the format 14 subtable to Default UVS Table. May be 0.
    default_uvs_offset : Offset32,
    /// Offset from the start of the format 14 subtable to Non-Default UVS Table. May be 0.
    non_default_uvs_offset : Offset32,
};

/// Format 14: Unicode Variation Sequences
///
/// <https://www.microsoft.com/typography/otspec/cmap.htm#subtable_14>
CMapSubtable14 = struct {
    /// Byte length of this subtable (including this header)
    length : u32be,
    /// Number of variation Selector Records
    num_var_selector_records : u32be,
    /// Array of `CMapSubtable14VariationSelector` records.
    var_selector : [CMapSubtable14VariationSelector; num_var_selector_records],
};

UnicodeRange = struct {
    /// First value in this range
    start_unicode_value : u24be,
    /// Number of additional values in this range
    additional_count : u8,
};

/// Default UVS table
DefaultUvs = struct {
    /// Number of Unicode character ranges.
    num_unicode_value_ranges : u32be,
    /// Array of UnicodeRange records.
    ranges : [UnicodeRange; num_unicode_value_ranges],
};

UvsMapping = struct {
    /// Base Unicode value of the UVS
    unicode_value : u24be,
    /// Glyph ID of the UVS
    glyph_id : u16be,
};

/// Non-Default UVS Table
NonDefaultUvs = struct {
    /// Number of UVS Mappings that follow
    num_uvs_mappings : u32be,
    /// Array of `UvsMapping` records.
    uvs_mappings : [UvsMapping; num_uvs_mappings]
};


// -----------------------------------------------------------------------------
//
// CMap Header
//
// <https://www.microsoft.com/typography/otspec/cmap.htm>
//
// -----------------------------------------------------------------------------

/// CMap Subtable
CMapSubtable = struct {
    format : u16be,
    body : cond {
        subtable0 : format == 0u16 => CMapSubtable0,
        subtable2 : format == 2u16 => CMapSubtable2,
        subtable4 : format == 4u16 => CMapSubtable4,
        subtable6 : format == 6u16 => CMapSubtable6,
        subtable8 : format == 8u16 => CMapSubtable8,
        subtable10 : format == 10u16 => CmapSubtable10,
        subtable12 : format == 12u16 => CMapSubtable12,
        subtable13 : format == 13u16 => CMapSubtable13,
        subtable14 : format == 14u16 => CMapSubtable14,
    },
};

/// Specifies a particular encoding and the offset to the corresponding subtable
///
/// <https://www.microsoft.com/typography/otspec/cmap.htm#encodings>
EncodingRecord = struct {
    /// Platform ID.
    platform_id : u16be,
    /// Platform-specific encoding ID.
    encoding_id : u16be,
    /// Byte offset from beginning of table to the subtable for this encoding.
    subtable_offset : Offset32,
};

/// Character To Glyph Index Mapping Table
///
/// <https://www.microsoft.com/typography/otspec/cmap.htm#cmapHeader>
CMap = struct {
    /// Table version number (0)
    version : u16be where version => version == 0u16,
    /// Number of encoding records that follow
    num_tables : u16be,
    /// A list of encoding records
    encoding_records : [EncodingRecord; num_tables],
};



// =============================================================================
//
// head — Font Header Table
//
// <https://www.microsoft.com/typography/otspec/head.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6head.html>
//
// =============================================================================

/// Font header table
///
/// <https://www.microsoft.com/typography/otspec/head.htm>
FontHeaderTable = struct {
    /// Major version number of the font header table — set to `1`.
    major_version : u16be where mv => mv == 1u16,
    /// Minor version number of the font header table — set to `0`.
    minor_version : u16be where mv => mv == 0u16,
    /// Set by font manufacturer.
    font_revision : Fixed,
    /// To compute: set it to `0`, sum the entire font as `u32be`, then store
    /// `0xB1B0AFBA - sum`. If the font is used as a component in a font
    /// collection file, the value of this field will be invalidated by changes
    /// to the file structure and font table directory, and must be ignored.
    check_sum_adjustment : u32be,
    /// Set to `0x5F0F3CF5`.
    magic_number : u32be where magic_number => magic_number == 0x5F0F3CF5u32,
    // TODO: Docs
    flags : u16be,
    /// Set to a value from `16` to `16384`. Any value in this range is valid.
    /// In fonts that have TrueType outlines, a power of 2 is recommended as
    /// this allows performance optimizations in some rasterizers.
    units_per_em : u16be where x => x >= 16u16 && x <= 16384u16,
    /// Number of seconds since 12:00 midnight that started January 1st 1904 in
    /// GMT/UTC time zone. 64-bit integer
    created : LongDateTime,
    /// Number of seconds since 12:00 midnight that started January 1st 1904 in
    /// GMT/UTC time zone. 64-bit integer
    modified : LongDateTime,
    /// For all glyph bounding boxes.
    x_min : i16be,
    /// For all glyph bounding boxes.
    y_min : i16be,
    /// For all glyph bounding boxes.
    x_max : i16be,
    /// For all glyph bounding boxes.
    y_max : i16be,
    // TODO: Docs
    mac_style : u16be,
    /// Smallest readable size in pixels.
    lowest_rec_ppem : u16be,
    /// Deprecated (Set to 2).
    ///
    /// * `0`: Fully mixed directional glyphs
    /// * `1`: Only strongly left to right
    /// * `2`: Like `1` but also contains neutrals
    /// * `-1`: Only strongly right to left
    /// * `-2`: Like `-1` but also contains neutrals
    font_direction_hint : i16be where x => x == 0i16 || x == 1i16 || x == 2i16 || x == -1i16 || x == -2i16,
    /// `0` for short offsets (`Offset16`), `1` for long (`Offset32`).
    index_to_loc_format : i16be where x => x == 0i16 || x == 1i16,
    /// `0` for current format.
    glyph_data_format : i16be,
};



// =============================================================================
//
// hhea — Horizontal Header Table
//
// <https://www.microsoft.com/typography/otspec/hhea.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6hhea.html>
//
// =============================================================================

/// Horizontal Header Table
///
/// <https://www.microsoft.com/typography/otspec/hhea.htm>
HorizontalHeader = struct {
    /// Minor version number of the horizontal header table — set to 0.
    minor_version : u16be where x => x == 0u16,
    /// Major version number of the horizontal header table — set to 1.
    major_version : u16be where x => x == 1u16,
    /// Typographic ascent (Distance from baseline of highest ascender).
    ascender : FWord,
    /// Typographic descent (Distance from baseline of lowest descender).
    descender : FWord,
    /// Typographic line gap.
    ///
    /// Negative `line_gap` values are treated as zero in Windows 3.1, and in
    /// Mac OS System 6 and System 7.
    line_gap : FWord,
    /// Maximum advance width value in 'hmtx' table.
    advance_width_max : UfWord,
    /// Minimum left sidebearing value in 'hmtx' table.
    min_left_side_bearing : FWord,
    /// Minimum right sidebearing value; calculated as `min(aw - lsb - (x_max - x_min))`.
    min_right_side_bearing : FWord,
    /// `max(lsb + (x_max - x_min))`.
    x_max_extent : FWord,
    /// Used to calculate the slope of the cursor (rise/run); 1 for vertical.
    caret_slope_rise : i16be,
    /// 0 for vertical.
    caret_slope_run : i16be,
    /// The amount by which a slanted highlight on a glyph needs to be shifted
    /// to produce the best appearance. Set to 0 for non-slanted fonts
    caret_offset : i16be,
    /// (reserved) set to 0
    reserved0 : i16be where x => x == 0i16,
    /// (reserved) set to 0
    reserved1 : i16be where x => x == 0i16,
    /// (reserved) set to 0
    reserved2 : i16be where x => x == 0i16,
    /// (reserved) set to 0
    reserved3 : i16be where x => x == 0i16,
    /// 0 for current format.
    metric_data_format : i16be,
    /// Number of `h_metric` entries in 'hmtx' table
    number_of_h_metrics : u16be,
};



// =============================================================================
//
// hmtx - Horizontal Metrics
//
// <https://www.microsoft.com/typography/otspec/hmtx.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6hmtx.html>
//
// =============================================================================

// TODO: Require value dependent types
//
// /// <https://www.microsoft.com/typography/otspec/hmtx.htm#hmtxHeader>
// HorizontalMetrics(num_glyphs : u16, number_of_h_metrics : u16) = struct {
//     /// Paired advance width and left side bearing values for each glyph.
//     /// Records are indexed by glyph ID.
//     h_metrics : [LongHorMetric; number_of_h_metrics],
//     /// Left side bearings for glyph IDs greater than or equal to `number_of_h_metrics`.
//     left_side_bearings : [i16be; num_glyphs - number_of_h_metrics],
// };

/// <https://www.microsoft.com/typography/otspec/hmtx.htm#lhm>
LongHorMetric = struct {
    /// Advance width, in font design units.
    advance_width : u16be,
    /// Glyph left side bearing, in font design units.
    lsb : i16be,
};



// =============================================================================
//
// maxp - Maximum Profile
//
// <https://www.microsoft.com/typography/otspec/hmtx.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6maxp.html>
//
// =============================================================================

/// Version 0.5
///
/// Fonts with CFF data must use Version 0.5 of this table, specifying only the
/// `num_glyphs` field
Version_0_5 = struct {
    /// The number of glyphs in the font
    num_glyphs : u16be,
};

/// Version 1.0
///
/// Fonts with TrueType outlines must use Version 1.0 of this table, where all
/// data is required
Version_1_0 = struct {
    /// The number of glyphs in the font
    num_glyphs : u16be,
    /// Maximum points in a non-composite glyph.
    max_points : u16be,
    /// Maximum contours in a non-composite glyph.
    max_contours : u16be,
    /// Maximum points in a composite glyph.
    max_composite_points : u16be,
    /// Maximum contours in a composite glyph.
    max_composite_contours : u16be,
    /// 1 if instructions do not use the twilight zone (Z0), or 2 if
    /// instructions do use Z0; should be set to 2 in most cases.
    max_zones : u16be,
    /// Maximum points used in Z0.
    max_twilight_points : u16be,
    /// Number of Storage Area locations.
    max_storage : u16be,
    /// Number of FDEFs, equal to the highest function number + 1.
    max_function_defs : u16be,
    /// Number of IDEFs.
    max_instruction_defs : u16be,
    /// Maximum stack depth2.
    max_stack_elements : u16be,
    /// Maximum byte count for glyph instructions.
    max_size_of_instructions : u16be,
    /// Maximum number of components referenced at “top level” for any composite glyph.
    max_component_elements : u16be,
    /// Maximum levels of recursion; 1 for simple components.
    max_component_depth : u16be,
};

/// Maximum Profile
///
/// <https://www.microsoft.com/typography/otspec/maxp.htm>
///
/// Establishes the memory requirements for this font.
MaximumProfile = struct {
    version : Fixed,
    data : cond {
        version_0_5 : version.value == 0x00005000u32 => Version_0_5,
        version_0_1 : version.value == 0x00010000u32 => Version_1_0,
    },
};



// =============================================================================
//
// name — Naming Table
//
// <https://www.microsoft.com/typography/otspec/name.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6name.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// OS/2 — OS/2 and Windows Metrics Table
//
// <https://www.microsoft.com/typography/otspec/os2.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6OS2.html>
//
// =============================================================================

/// OS/2 and Windows Metrics Table
///
/// <https://www.microsoft.com/typography/otspec/os2.htm>
Os2 = struct {
    version : u16be,

    // FIXME: Proper version switching
    // TODO: Documentation

    // Version 0
    //
    // <https://www.microsoft.com/typography/otspec/os2ver0.htm>

    x_avg_char_width : i16be,
    us_weight_class : u16be,
    us_width_class : u16be,
    fs_type : u16be,
    y_subscript_x_size : i16be,
    y_subscript_y_size : i16be,
    y_subscript_x_offset : i16be,
    y_subscript_y_offset : i16be,
    y_superscript_x_size : i16be,
    y_superscript_y_size : i16be,
    y_superscript_x_offset : i16be,
    y_superscript_y_offset : i16be,
    y_strikeout_size : i16be,
    y_strikeout_position : i16be,
    s_family_class : i16be,
    panose : [u8; 10u8],
    /// Bits 0–31
    ul_unicode_range1 : u32be,
    /// Bits 32–63
    ul_unicode_range2 : u32be,
    /// Bits 64–95
    ul_unicode_range3 : u32be,
    /// Bits 96–127
    ul_unicode_range4 : u32be,
    ach_vend_id : Tag,
    fs_selection : u16be,
    us_first_char_index : u16be,
    us_last_char_index : u16be,

    // Version 1
    //
    // <https://www.microsoft.com/typography/otspec/os2ver1.htm>

    s_typo_ascender : i16be,
    s_typo_descender : i16be,
    s_typo_line_gap : i16be,
    us_win_ascent : u16be,
    us_win_descent : u16be,
    /// Bits 0–31
    ul_code_page_range1 : u32be,
    /// Bits 32–63
    ul_code_page_range2 : u32be,

    // Version 2
    //
    // <https://www.microsoft.com/typography/otspec/os2ver2.htm>

    sx_height : i16be,
    s_cap_height : i16be,
    us_default_char : u16be,
    us_break_char : u16be,
    us_max_context : u16be,

    // Version 5
    //
    // <https://www.microsoft.com/typography/otspec/os2.htm>

    us_lower_optical_point_size : u16be,
    us_upper_optical_point_size : u16be,
};



// =============================================================================
//
// post — PostScript Table
//
// <https://www.microsoft.com/typography/otspec/post.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6post.html>
//
// =============================================================================

/// PostScript Table
Post = struct {
    /// Version number.
    ///
    /// * `0x00010000` - for version 1.0
    /// * `0x00020000` - for version 2.0
    /// * `0x00025000` - for version 2.5 (deprecated)
    /// * `0x00030000` - for version 3.0
    version : Fixed,
    /// Italic angle in counter-clockwise degrees from the vertical. Zero for
    /// upright text, negative for text that leans to the right (forward).
    italic_angle : Fixed,
    /// This is the suggested distance of the top of the underline from the
    /// baseline (negative values indicate below baseline).
    ///
    /// The PostScript definition of this FontInfo dictionary key (the y
    /// coordinate of the center of the stroke) is not used for historical
    /// reasons. The value of the PostScript key may be calculated by
    /// subtracting half the `underline_thickness` from the value of this field.
    underline_position : FWord,
    /// Suggested values for the underline thickness.
    underline_thickness : FWord,
    /// Set to 0 if the font is proportionally spaced, non-zero if the font is
    /// not proportionally spaced (i.e. monospaced).
    is_fixed_pitch : u32be,
    /// Minimum memory usage when an OpenType font is downloaded.
    min_mem_type42 : u32be,
    /// Maximum memory usage when an OpenType font is downloaded.
    max_mem_type42 : u32be,
    /// Minimum memory usage when an OpenType font is downloaded as a Type 1 font.
    min_mem_type1 : u32be,
    /// Maximum memory usage when an OpenType font is downloaded as a Type 1 font.
    max_mem_type1 : u32be,

    // Version 2.0

    /// Number of glyphs (this should be the same as numGlyphs in 'maxp' table).
    num_glyphs : u16be,
    /// This is not an offset, but is the ordinal number of the glyph in 'post'
    /// string tables.
    glyph_name_index : [u16be; num_glyphs],
    // FIXME: numberNewGlyphs ???
    // /// Glyph names with length bytes [variable] (a Pascal string).
    // names : [i8; numberNewGlyphs],
};

// TODO - other version data



// =============================================================================
//
// cvt — Control Value Table
//
// <https://www.microsoft.com/typography/otspec/cvt.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6cvt.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// fpgm - Font Program
//
// <https://www.microsoft.com/typography/otspec/fpgm.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6fpgm.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// glyf - Glyf Data
//
// <https://www.microsoft.com/typography/otspec/glyf.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6glyf.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// loca - Index to Location
//
// <https://www.microsoft.com/typography/otspec/loca.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6loca.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// prep - Control Value Program
//
// <https://www.microsoft.com/typography/otspec/prep.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6prep.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// gasp — Grid-fitting And Scan-conversion Procedure Table
//
// <https://www.microsoft.com/typography/otspec/gasp.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6gasp.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// CFF - Compact Font Format table
//
// <https://www.microsoft.com/typography/otspec/cff.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// CFF - Compact Font Format table
//
// <https://www.microsoft.com/typography/otspec/cff2.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// VORG - Vertical Origin Table
//
// <https://www.microsoft.com/typography/otspec/vorg.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// SVG - The SVG (Scalable Vector Graphics) table
//
// <https://www.microsoft.com/typography/otspec/svg.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// EBDT - Embedded Bitmap Data Table
//
// <https://www.microsoft.com/typography/otspec/ebdt.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// EBLC - Embedded Bitmap Location Table
//
// <https://www.microsoft.com/typography/otspec/eblc.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// EBSC - Embedded Bitmap Scaling Table
//
// <https://www.microsoft.com/typography/otspec/ebsc.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6EBSC.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// CBDT - Color Bitmap Data Table
//
// <https://www.microsoft.com/typography/otspec/cbdt.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// CBLC - Color Bitmap Location Table
//
// <https://www.microsoft.com/typography/otspec/cblc.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// sbix — Standard Bitmap Graphics Table
//
// <https://www.microsoft.com/typography/otspec/sbix.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6sbix.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// BASE - Baseline Table
//
// <https://www.microsoft.com/typography/otspec/base.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// GDEF — Glyph Definition Table
//
// <https://www.microsoft.com/typography/otspec/gdef.htm>
//
// =============================================================================


// -----------------------------------------------------------------------------
// Glyph Class Definition Table
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#glyphClassDefTbl>
// -----------------------------------------------------------------------------

// TODO: GlyphClassDef = ClassDef(GlyphClassDefEnum);


// -----------------------------------------------------------------------------
// Attachment Point List Table
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#attachmentPointListTbl>
// -----------------------------------------------------------------------------

/// AttachPoint table
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#attachPointTable>
AttachPoint = struct {
    /// Number of attachment points on this glyph
    point_count : u16be,
    /// Array of contour point indices -in increasing numerical order
    point_indices : [u16be; point_count],
};

/// AttachList table
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#attachListTable>
AttachList = struct {
    /// Offset to Coverage table - from beginning of AttachList table
    coverage_offset : Offset16,
    /// Number of glyphs with attachment points
    glyph_count : u16be,
    /// Array of offsets to AttachPoint tables-from beginning of AttachList
    /// table-in Coverage Index order
    attach_point_offsets : [Offset16; glyph_count],
};


// -----------------------------------------------------------------------------
// Ligature Caret List Table
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#ligatureCaretListTbl>
// -----------------------------------------------------------------------------

/// Ligature Caret List Table
///
/// <https://www.microsoft.com/typography/otspec/gsub.htm#ligatureCaretListTbl>
LigCaretList = struct {
    /// Offset to Coverage table - from beginning of LigCaretList table
    coverage_offset : Offset16,
    /// Number of ligature glyphs
    lig_glyph_count : u16be,
    /// Array of offsets to LigGlyph tables, from beginning of LigCaretList
    /// table —in Coverage Index order
    lig_glyph_offsets : [Offset16; lig_glyph_count],
};

/// LigGlyph table
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#ligGlyphTable>
LigGlyph = struct {
    /// Number of CaretValue tables for this ligature (components - 1)
    caret_count : u16be,
    /// Array of offsets to CaretValue tables, from beginning of LigGlyph
    /// table — in increasing coordinate order
    caret_value_offsets : [Offset16; caret_count],
};

/// CaretValue Format 1: Design units only
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#caretValueTbl1>
CaretValueFormat1 = struct {
    /// X or Y value, in design units
    coordinate : i16be,
};

/// CaretValue Format 2: Contour point
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#caretValueTbl2>
CaretValueFormat2 = struct {
    /// Contour point index on glyph
    caret_value_point_index : u16be,
};

/// Caret Value Format 3: Design units plus Device or VariationIndex table
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#caretValueTable_3>
CaretValueFormat3 = struct {
    /// X or Y value, in design units
    coordinate : i16be,
    /// Offset to Device table (non-variable font) / Variation Index table
    /// (variable font) for X or Y value-from beginning of CaretValue table
    device_offset : Offset16,
};

/// Caret Value Tables
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#caretValueTbls>
CaretValue = struct {
    /// Format identifier
    caret_value_format : u16be,
    caret_value : cond {
        format1 : caret_value_format == 1u16 => CaretValueFormat1,
        format2 : caret_value_format == 2u16 => CaretValueFormat2,
        format3 : caret_value_format == 3u16 => CaretValueFormat3,
    },
};


// -----------------------------------------------------------------------------
// Mark Attachment Class Definition Table
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#ligatureCaretListTbl>
// -----------------------------------------------------------------------------

// TODO


// -----------------------------------------------------------------------------
//
// GDEF Header
//
// <https://www.microsoft.com/typography/otspec/gdef.htm#gdefHeader>
//
// -----------------------------------------------------------------------------

/// GDEF Header
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#gdefHeader>
GDef = struct {
    /// Major version of the GDEF table, = 1
    major_version : u16be,
    /// Minor version of the GDEF table, = 3
    minor_version : u16be,

    // FIXME: Proper version switching

    // GDEF Header, Version 1.0
    // <https://www.microsoft.com/typography/otspec/gdef.htm#gdefHeader_10>

    /// Offset to class definition table for glyph type, from beginning of GDEF header (may be NULL)
    glyph_class_def_offset : Offset16,
    /// Offset to attachment point list table, from beginning of GDEF header (may be NULL)
    attach_list_offset : Offset16,
    /// Offset to ligature caret list table, from beginning of GDEF header (may be NULL)
    lig_caret_list_offset : Offset16,
    /// Offset to class definition table for mark attachment type, from beginning of GDEF header (may be NULL)
    mark_attach_class_def_offset : Offset16,

    // GDEF Header, Version 1.2
    // <https://www.microsoft.com/typography/otspec/gdef.htm#gdefHeader_12>

    /// Offset to the table of mark glyph set definitions, from beginning of GDEF header (may be NULL)
    mark_glyph_sets_def_offset : Offset16,

    // GDEF Header, Version 1.3
    // <https://www.microsoft.com/typography/otspec/gdef.htm#gdefHeader_12>

    /// Offset to the Item Variation Store table, from beginning of GDEF header (may be NULL)
    item_var_store_offset : Offset32,
};



// =============================================================================
//
// GPOS — Glyph Positioning Table
//
// <https://www.microsoft.com/typography/otspec/gpos.htm>
//
// =============================================================================


// -----------------------------------------------------------------------------
// Shared Tables: Value Record, Anchor Table, and Mark Array Table
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#sharedTables>
// -----------------------------------------------------------------------------

/// Value Record
///
/// <https://www.microsoft.com/typography/otspec/gsub.htm#valueRecord>
ValueRecord = struct {
    /// Horizontal adjustment for placement, in design units.
    x_placement : i16be,
    /// Vertical adjustment for placement, in design units.
    y_placement : i16be,
    /// Horizontal adjustment for advance, in design units — only used for
    /// horizontal layout.
    x_advance : i16be,
    /// Vertical adjustment for advance, in design units — only used for
    /// vertical layout.
    y_advance : i16be,
    /// Offset to Device table (non-variable font) / VariationIndex table
    /// (variable font) for horizontal placement, from beginning of positioning
    /// subtable (SimplePos, PairPos — may be NULL)
    x_pla_device_offset : Offset16,
    /// Offset to Device table (non-variable font) / VariationIndex table
    /// (variable font) for vertical placement, from beginning of positioning
    /// subtable (SimplePos, PairPos — may be NULL)
    y_pla_device_offset : Offset16,
    /// Offset to Device table (non-variable font) / VariationIndex table
    /// (variable font) for horizontal advance, from beginning of positioning
    /// subtable (SimplePos, PairPos — may be NULL)
    x_adv_device_offset : Offset16,
    /// Offset to Device table (non-variable font) / VariationIndex table
    /// (variable font) for vertical advance, from beginning of positioning
    /// subtable (SimplePos, PairPos — may be NULL)
    y_adv_device_offset : Offset16,
};


// -----------------------------------------------------------------------------
// Lookup Type 1: Single Adjustment Positioning Subtable
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#SP>
// -----------------------------------------------------------------------------

/// Single Adjustment Positioning Format 1: Single Positioning Value
///
/// <https://www.microsoft.com/typography/otspec/gsub.htm#SPF1>
SinglePosFormat1 = struct {
    /// Format identifier: format = 1
    pos_format : u16be,
    /// Offset to Coverage table, from beginning of SinglePos subtable.
    coverage_offset : Offset16,
    /// Defines the types of data in the ValueRecord.
    value_format : u16be,
    /// Defines positioning value(s) — applied to all glyphs in the Coverage table.
    value_record : ValueRecord,
};

/// Single Adjustment Positioning Format 2: Array of Positioning Values
///
/// <https://www.microsoft.com/typography/otspec/gsub.htm#SPF2>
SinglePosFormat2 = struct {
    /// Format identifier: format = 2
    pos_format : u16be,
    /// Offset to Coverage table, from beginning of SinglePos subtable.
    coverage_offset : Offset16,
    /// Defines the types of data in the ValueRecords.
    value_format : u16be,
    /// Number of ValueRecords — must equal glyphCount in the Coverage table.
    value_count : u16be,
    /// Array of ValueRecords — positioning values applied to glyphs.
    value_records : [ValueRecord; value_count],
};


// -----------------------------------------------------------------------------
// Lookup Type 2: Pair Adjustment Positioning Subtable
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#PP>
// -----------------------------------------------------------------------------

/// PairValueRecord
///
/// <https://www.microsoft.com/typography/otspec/gsub.htm#pairValueRec>
PairValueRecord = struct {
    /// Glyph ID of second glyph in the pair (first glyph is listed in the
    /// Coverage table).
    second_glyph : u16be,
    /// Positioning data for the first glyph in the pair.
    value_record1 : ValueRecord,
    /// Positioning data for the second glyph in the pair.
    value_record2 : ValueRecord,
};

/// PairSet Table
///
/// <https://www.microsoft.com/typography/otspec/gsub.htm#pairSetTbl>
PairSet = struct {
    /// Number of PairValueRecords
    pair_value_count : u16be,
    /// Array of PairValueRecords, ordered by glyph ID of the second glyph.
    pair_value_records : [PairValueRecord; pair_value_count],
};

/// Pair Adjustment Positioning Format 1: Adjustments for Glyph Pairs
///
/// <https://www.microsoft.com/typography/otspec/gsub.htm#PPF1>
PairPosFormat1 = struct {
    /// Format identifier: format = 1
    pos_format : u16be,
    /// Offset to Coverage table, from beginning of PairPos subtable.
    coverage_offset : Offset16,
    /// Defines the types of data in valueRecord1 — for the first glyph in the
    /// pair (may be zero).
    value_format1 : u16be,
    /// Defines the types of data in valueRecord2 — for the second glyph in the
    /// pair (may be zero).
    value_format2 : u16be,
    /// Number of PairSet tables
    pair_set_count : u16be,
    /// Array of offsets to PairSet tables. Offsets are from beginning of
    /// PairPos subtable, ordered by Coverage Index.
    pair_set_offsets : [Offset16; pair_set_count],
};

/// Class2Record
///
/// <https://www.microsoft.com/typography/otspec/gsub.htm#class2Rec>
Class2Record = struct {
    /// Positioning for first glyph — empty if valueFormat1 = 0.
    value_record1 : ValueRecord,
    /// Positioning for second glyph — empty if valueFormat2 = 0.
    value_record2 : ValueRecord,
};

// FIXME: dependent type abstraction
// /// Class1Record
// ///
// /// <https://www.microsoft.com/typography/otspec/gsub.htm#class1Rec>
// Class1Record(class2Count : u16) = struct {
//     /// Array of Class2 records, ordered by classes in classDef2.
//     class2_records : [Class2Record; class2Count]
// };

/// Pair Adjustment Positioning Format 2: Class Pair Adjustment
///
/// <https://www.microsoft.com/typography/otspec/gsub.htm#PPF2>
PairPosFormat2 = struct {
    /// Format identifier: format = 2
    pos_format : u16be,
    /// Offset to Coverage table, from beginning of PairPos subtable.
    coverage_offset : Offset16,
    /// ValueRecord definition — for the first glyph of the pair (may be zero).
    value_format1 : u16be,
    /// ValueRecord definition — for the second glyph of the pair (may be zero).
    value_format2 : u16be,
    /// Offset to ClassDef table, from beginning of PairPos subtable — for the
    /// first glyph of the pair.
    class_def1_offset : Offset16,
    /// Offset to ClassDef table, from beginning of PairPos subtable — for the
    /// second glyph of the pair.
    class_def2_offset : Offset16,
    /// Number of classes in classDef1 table — includes Class 0.
    class1_count : u16be,
    /// Number of classes in classDef2 table — includes Class 0.
    class2_count : u16be,
    // FIXME: dependent type abstraction
    // /// Array of Class1 records, ordered by classes in classDef1.
    // class1_records : [Class1Record(class2_count); class1_count],
    /// Array of Class1 records, ordered by classes in classDef1.
    class1_records : [[Class2Record; class2_count]; class1_count],
};


// -----------------------------------------------------------------------------
// Lookup Type 3: Cursive Attachment Positioning Subtable
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#CAP>
// -----------------------------------------------------------------------------

/// EntryExitRecord
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#entryExitRec>
EntryExitRecord = struct {
    /// Offset to entryAnchor table, from beginning of CursivePos subtable (may be NULL).
    entry_anchor_offset : Offset16,
    /// Offset to exitAnchor table, from beginning of CursivePos subtable (may be NULL).
    exit_anchor_offset : Offset16,
};

/// Cursive Attachment Positioning Format1: Cursive attachment
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#cursivePos_1>
CursivePosFormat1 = struct {
    /// Format identifier: format = 1
    pos_format : u16be,
    /// Offset to Coverage table, from beginning of CursivePos subtable.
    coverage_offset : Offset16,
    /// Number of EntryExit records
    entry_exit_count : u16be,
    /// Array of EntryExit records, in Coverage index order.
    entry_exit_record : [EntryExitRecord; entry_exit_count],
};


// -----------------------------------------------------------------------------
// Lookup Type 4: Mark-to-Base Attachment Positioning Subtable
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#MBP>
// -----------------------------------------------------------------------------

// TODO: dependent type abstraction
// /// BaseRecord
// ///
// /// <https://www.microsoft.com/typography/otspec/gpos.htm#baseRec>
// BaseRecord(mark_class_count : u16) = {
//     /// Array of offsets (one per mark class) to Anchor tables. Offsets
//     /// are from beginning of BaseArray table, ordered by class.
//     base_anchor_offset : [Offset16; mark_class_count],
// }
//
// /// BaseArray Table
// ///
// /// <https://www.microsoft.com/typography/otspec/gpos.htm#baseArrayTbl>
// BaseArray(mark_class_count : u16) = struct {
//     /// Number of BaseRecords
//     base_count : u16be,
//     /// Array of BaseRecords, in order of baseCoverage Index.
//     base_records : [BaseRecord(mark_class_count); base_count],
// };

/// Mark-to-Base Attachment Positioning Format 1: Mark-to-base Attachment Point
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#MBPF1>
MarkBasePosFormat1 = struct {
    /// Format identifier: format = 1
    pos_format : u16be,
    /// Offset to markCoverage table, from beginning of MarkBasePos subtable.
    mark_coverage_offset : Offset16,
    /// Offset to baseCoverage table, from beginning of MarkBasePos subtable.
    base_coverage_offset : Offset16,
    /// Number of classes defined for marks
    mark_class_count : u16be,
    /// Offset to MarkArray table, from beginning of MarkBasePos subtable.
    mark_array_offset : Offset16,
    /// Offset to BaseArray table, from beginning of MarkBasePos subtable.
    base_array_offset : Offset16,
};


// -----------------------------------------------------------------------------
// Lookup Type 5: Mark-to-Ligature Attachment Positioning Subtable
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#MLP>
// -----------------------------------------------------------------------------

// FIXME: dependent type abstraction
// /// ComponentRecord
// ///
// /// <https://www.microsoft.com/typography/otspec/gpos.htm#componentRec>
// ComponentRecord(mark_class_count : u16) = struct {
//     /// Array of offsets (one per class) to Anchor tables. Offsets are from
//     /// beginning of LigatureAttach table, ordered by class (may be NULL).
//     ligature_anchor_offsets : [Offset16; mark_class_count],
// };
//
// /// LigatureAttach Table
// ///
// /// <https://www.microsoft.com/typography/otspec/gpos.htm#ligatureAttachTbl>
// LigatureAttach(mark_class_count : u16) = struct {
//     /// Number of ComponentRecords in this ligature
//     component_count : u16be,
//     /// Array of Component records, ordered in writing direction.
//     component_records : [ComponentRecord; component_count],
// };

/// LigatureArray Table
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#ligatureArrayTbl>
LigatureArray = struct {
// FIXME: dependent type abstraction
// LigatureArray(mark_class_count : u16) = struct {
    /// Number of LigatureAttach table offsets
    ligature_count : u16be,
    /// Array of offsets to LigatureAttach tables. Offsets are from beginning of
    /// LigatureArray table, ordered by ligatureCoverage index.
    ligature_attach_offsets : [Offset16; ligature_count],
};

/// Mark-To-Ligature Attachment Positioning Format 1: Mark-to-Ligature Attachment
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#MLPF1>
MarkLigPosFormat1 = struct {
    /// Format identifier: format = 1
    pos_format : u16be,
    /// Offset to markCoverage table, from beginning of MarkLigPos subtable.
    mark_coverage_offset : Offset16,
    /// Offset to ligatureCoverage table, from beginning of MarkLigPos subtable.
    ligature_coverage_offset : Offset16,
    /// Number of defined mark classes
    mark_class_count : u16be,
    /// Offset to MarkArray table, from beginning of MarkLigPos subtable.
    mark_array_offset : Offset16,
    /// Offset to LigatureArray table, from beginning of MarkLigPos subtable.
    ligature_array_offset : Offset16,
};


// -----------------------------------------------------------------------------
// Lookup Type 6: Mark-to-Mark Attachment Positioning Subtable
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#MMP>
// -----------------------------------------------------------------------------

// FIXME: dependent type abstraction
// /// Mark2Record
// ///
// /// <https://www.microsoft.com/typography/otspec/gpos.htm#mark2Rec>
// Mark2Record(mark_class_count : u16) = struct {
//     /// Array of offsets (one per class) to Anchor tables. Offsets are
//     /// from beginning of Mark2Array table, in class order.
//     mark2_anchor_offsets : [Offset16; mark_class_count],
// };
//
// /// Mark2Array Table
// ///
// /// <https://www.microsoft.com/typography/otspec/gpos.htm#mark2ArrayTbl>
// Mark2Array(mark_class_count : u16) = struct {
//     /// Number of Mark2 records
//     mark2_count : u16be,
//     /// Array of Mark2Records, in Coverage order.
//     mark2_records : [Mark2Record(mark_class_count); mark2_count],
// };

/// Mark-to-Mark Attachment Positioning Format 1: Mark-to-Mark Attachment
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#MMPF1>Description
MarkMarkPosFormat1 = struct {
    /// Format identifier: format = 1
    pos_format : u16be,
    /// Offset to Combining Mark Coverage table, from beginning of MarkMarkPos subtable.
    mark1_coverage_offset : Offset16,
    /// Offset to Base Mark Coverage table, from beginning of MarkMarkPos subtable.
    mark2_coverage_offset : Offset16,
    /// Number of Combining Mark classes defined
    mark_class_count : u16be,
    /// Offset to MarkArray table for mark1, from beginning of MarkMarkPos subtable.
    mark1_array_offset : Offset16,
    /// Offset to Mark2Array table for mark2, from beginning of MarkMarkPos subtable.
    mark2_array_offset : Offset16,
};


// -----------------------------------------------------------------------------
// Lookup Type 7: Contextual Positioning Subtables
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#CP>
// -----------------------------------------------------------------------------

/// Position Lookup Record
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#posLookupRecord>
PosLookupRecord = struct {
    /// Index (zero-based) to input glyph sequence
    sequence_index : u16be,
    /// Index (zero-based) into the LookupList for the Lookup table to apply to
    /// that position in the glyph sequence.
    lookup_list_index : u16be,
};

/// PosRule Table
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#posRuleTbl>
PosRule = struct {
    /// Number of glyphs in the input glyph sequence
    glyph_count : u16be,
    /// Number of PosLookupRecords
    pos_count : u16be,
    /// Array of input glyph IDs — starting with the second glyph.
    input_sequence : [u16be; glyph_count - 1u16],
    /// Array of positioning lookups, in design order.
    pos_lookup_records : [PosLookupRecord; pos_count],
};

/// PosRuleSet Table
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#posRuleSetTbl>
PosRuleSet = struct {
    /// Number of PosRule tables
    pos_rule_count : u16be,
    /// Array of offsets to PosRule tables. Offsets are from beginning of
    /// PosRuleSet, ordered by preference.
    pos_rule_offsets : [Offset16; pos_rule_count],
};

/// Context Positioning Subtable Format 1: Simple Glyph Contexts
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#CPF1>
ContextPosFormat1 = struct {
    /// Format identifier: format = 1
    pos_format : u16be,
    /// Offset to Coverage table, from beginning of ContextPos subtable.
    coverage_offset : Offset16,
    /// Number of PosRuleSet tables
    pos_rule_set_count : u16be,
    /// Array of offsets to PosRuleSet tables. Offsets are from beginning of
    /// ContextPos subtable, ordered by Coverage Index.
    pos_rule_set_offsets : [Offset16; pos_rule_set_count],
};

/// PosClassRule Table
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#posClassRuleTbl>
PosClassRule = struct {
    /// Number of glyphs to be matched
    glyph_count : u16be,
    /// Number of PosLookupRecords
    pos_count : u16be,
    /// Array of classes to be matched to the input glyph sequence, beginning
    /// with the second glyph position.
    classes : [u16be; glyph_count - 1u16],
    /// Array of PosLookupRecords, in design order.
    pos_lookup_records : [PosLookupRecord; pos_count],
};

/// PosClassSet Table
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#posClassSetTbl>
PosClassSet = struct {
    /// Number of PosClassRule tables
    pos_class_rule_count : u16be,
    /// Array of offsets to PosClassRule tables. Offsets are from beginning of
    /// PosClassSet, ordered by preference.
    pos_class_rule_offsets : [Offset16; pos_class_rule_count],
};

/// Context Positioning Subtable Format 2: Class-based Glyph Contexts
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#CPF2>
ContextPosFormat2 = struct {
    /// Format identifier: format = 2
    pos_format : u16be,
    /// Offset to Coverage table, from beginning of ContextPos subtable.
    coverage_offset : Offset16,
    /// Offset to ClassDef table, from beginning of ContextPos subtable.
    class_def_offset : Offset16,
    /// Number of PosClassSet tables
    pos_class_set_count : u16be,
    /// Array of offsets to PosClassSet tables. Offsets are from beginning of
    /// ContextPos subtable, ordered by class (may be NULL).
    pos_class_set_offsets : [Offset16; pos_class_set_count],
};

/// Context Positioning Subtable Format 3: Coverage-based Glyph Contexts
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#CPF3>
ContextPosFormat3 = struct {
    /// Format identifier: format = 3
    pos_format : u16be,
    /// Number of glyphs in the input sequence
    glyph_count : u16be,
    /// Number of PosLookupRecords
    pos_count : u16be,
    /// Array of offsets to Coverage tables, from beginning of ContextPos subtable.
    coverage_offsets : [Offset16; glyph_count],
    /// Array of PosLookupRecords, in design order.
    pos_lookup_records : [PosLookupRecord; pos_count],
};


// -----------------------------------------------------------------------------
// LookupType 8: Chaining Contextual Positioning Subtable
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#CCP>
// -----------------------------------------------------------------------------

/// ChainPosRule Table
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#chainPosRuleTbl>
ChainPosRule = struct {
    /// Total number of glyphs in the backtrack sequence.
    backtrack_glyph_count : u16be,
    /// Array of backtrack glyph IDs.
    backtrack_sequence : [u16be; backtrack_glyph_count],
    /// Total number of glyphs in the input sequence — includes the first glyph.
    input_glyph_count : u16be,
    /// Array of input glyph IDs — start with second glyph.
    input_sequence : [u16be; input_glyph_count - 1u16],
    /// Total number of glyphs in the lookahead sequence.
    lookahead_glyph_count : u16be,
    /// Array of lookahead glyph IDs.
    lookahead_sequence : [u16be; lookahead_glyph_count],
    /// Number of PosLookupRecords
    pos_count : u16be,
    /// Array of PosLookupRecords, in design order.
    pos_lookup_records : [PosLookupRecord; pos_count],
};

/// ChainPosRuleSet Table
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#chainPosRuleSetTbl>
ChainPosRuleSet = struct {
    /// Number of ChainPosRule tables
    chain_pos_rule_count : u16be,
    /// Array of offsets to ChainPosRule tables. Offsets are from beginning of
    /// ChainPosRuleSet, ordered by preference.
    chain_pos_rule_offsets : [Offset16; chain_pos_rule_count],
};

/// Chaining Context Positioning Format 1: Simple Glyph Contexts
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#CCPF1>
ChainContextPosFormat1 = struct {
    /// Format identifier: format = 1
    pos_format : u16be,
    /// Offset to Coverage table, from beginning of ChainContextPos subtable.
    coverage_offset : Offset16,
    /// Number of ChainPosRuleSet tables
    chain_pos_rule_set_count : u16be,
    /// Array of offsets to ChainPosRuleSet tables. Offsets are from beginning
    /// of ChainContextPos subtable, ordered by Coverage Index.
    chain_pos_rule_set_offsets : [Offset16; chain_pos_rule_set_count],
};

/// ChainPosClassRule Table
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#chainPosClassRuleTbl>
ChainPosClassRule = struct {
    /// Total number of glyphs in the backtrack sequence.
    backtrack_glyph_count : u16be,
    /// Array of backtrack-sequence classes.
    backtrack_sequence : [u16be; backtrack_glyph_count],
    /// Total number of classes in the input sequence — includes the first class.
    input_glyph_count : u16be,
    /// Array of input classes to be matched to the input glyph sequence,
    /// beginning with the second glyph position.
    input_sequence : [u16be; input_glyph_count - 1u16],
    /// Total number of classes in the lookahead sequence.
    lookahead_glyph_count : u16be,
    /// Array of lookahead-sequence classes.
    lookahead_sequence : [u16be; lookahead_glyph_count],
    /// Number of PosLookupRecords
    pos_count : u16be,
    /// Array of PosLookupRecords, in design order.
    pos_lookup_records : [PosLookupRecord; pos_count],
};

/// ChainPosClassSet Table
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#chainPosClassSetTbl>
ChainPosClassSet = struct {
    /// Number of ChainPosClassRule tables
    chain_pos_class_rule_count : u16be,
    /// Array of offsets to ChainPosClassRule tables. Offsets are from beginning
    /// of ChainPosClassSet, ordered by preference.
    chain_pos_class_rule_offsets : [Offset16; chain_pos_class_rule_count],
};

/// Chaining Context Positioning Format 2: Class-based Glyph Contexts
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#CCPF2>
ChainContextPosFormat2 = struct {
    /// Format identifier: format = 2
    pos_format : u16be,
    /// Offset to Coverage table, from beginning of ChainContextPos subtable.
    coverage_offset : Offset16,
    /// Offset to ClassDef table containing backtrack sequence context, from
    /// beginning of ChainContextPos subtable.
    backtrack_class_def_offset : Offset16,
    /// Offset to ClassDef table containing input sequence context, from
    /// beginning of ChainContextPos subtable.
    input_class_def_offset : Offset16,
    /// Offset to ClassDef table containing lookahead sequence context, from
    /// beginning of ChainContextPos subtable.
    lookahead_class_def_offset : Offset16,
    /// Number of ChainPosClassSet tables
    chain_pos_class_set_count : u16be,
    /// Array of offsets to ChainPosClassSet tables. Offsets are from beginning
    /// of ChainContextPos subtable, ordered by input class (may be NULL).
    chain_pos_class_set_offsets : [Offset16; chain_pos_class_set_count],
};

/// Chaining Context Positioning Format 3: Coverage-based Glyph Contexts
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#CCPF3>
ChainContextPosFormat3 = struct {
    /// Format identifier: format = 3
    pos_format : u16be,
    /// Number of glyphs in the backtrack sequence
    backtrack_glyph_count : u16be,
    /// Array of offsets to coverage tables for the backtrack sequence, in
    /// glyph sequence order.
    backtrack_coverage_offsets : [Offset16; backtrack_glyph_count],
    /// Number of glyphs in input sequence
    input_glyph_count : u16be,
    /// Array of offsets to coverage tables for the input sequence, in
    /// glyph sequence order.
    input_coverage_offsets : [Offset16; input_glyph_count],
    /// Number of glyphs in lookahead sequence
    lookahead_glyph_count : u16be,
    /// Array of offsets to coverage tables for the lookahead sequence, in
    /// glyph sequence order.
    lookahead_coverage_offsets : [Offset16; lookahead_glyph_count],
    /// Number of PosLookupRecords
    pos_count : u16be,
    /// Array of PosLookupRecords, in design order.
    pos_lookup_records : [PosLookupRecord; pos_count],
};


// -----------------------------------------------------------------------------
// LookupType 9: Extension Positioning
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#EP>
// -----------------------------------------------------------------------------

/// Extension Positioning Subtable Format 1
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#EPF1>
ExtensionPosFormat1 = struct {
    /// Format identifier: format = 1
    pos_format : u16be,
    /// Lookup type of subtable referenced by extensionOffset (i.e. the
    /// extension subtable).
    extension_lookup_type : u16be,
    /// Offset to the extension subtable, of lookup type extensionLookupType,
    /// relative to the start of the ExtensionPosFormat1 subtable.
    extension_offset : Offset32,
};


// -----------------------------------------------------------------------------
//
// GPOS Header
//
// <https://www.microsoft.com/typography/otspec/gpos.htm>
//
// -----------------------------------------------------------------------------

/// GPOS Header
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#header>
GPos = struct {
    /// Major version of the GPOS table
    major_version : u16be,
    /// Minor version of the GPOS table
    minor_version : u16be,

    // FIXME: Proper version switching

    // GPOS Header, Version 1.0
    // <https://www.microsoft.com/typography/otspec/gpos.htm#gposHeader_10>

    /// Offset to `ScriptList` table, from beginning of GPOS table
    script_list_offset : Offset16,
    /// Offset to `FeatureList` table, from beginning of GPOS table
    feature_list_offset : Offset16,
    /// Offset to `LookupList` table, from beginning of GPOS table
    lookup_list_offset : Offset16,

    // GPOS Header, Version 1.1
    // <https://www.microsoft.com/typography/otspec/gpos.htm#gposHeader_11>

    /// Offset to `FeatureVariations` table, from beginning of GPOS table (may be NULL)
    feature_variations_offset : Offset32,
};



// =============================================================================
//
// GSUB — Glyph Substitution Table
//
// <https://www.microsoft.com/typography/otspec/gsub.htm>
//
// =============================================================================

// TODO

/// GSUB Header
///
/// <https://www.microsoft.com/typography/otspec/gsub.htm#header>
GSub = struct {
    /// Major version of the GSUB table
    major_version : u16be,
    /// Minor version of the GSUB table
    minor_version : u16be,

    // FIXME: Proper version switching

    // GSUB Header, Version 1.0
    // <https://www.microsoft.com/typography/otspec/gsub.htm#gsubHeader_10>

    /// Offset to `ScriptList` table, from beginning of GSUB table
    script_list_offset : Offset16,
    /// Offset to `FeatureList` table, from beginning of GSUB table
    feature_list_offset : Offset16,
    /// Offset to `LookupList` table, from beginning of GSUB table
    lookup_list_offset : Offset16,

    // GSUB Header, Version 1.1
    // <https://www.microsoft.com/typography/otspec/gsub.htm#gsubHeader_11>

    /// Offset to `FeatureVariations` table, from beginning of GSUB table (may be NULL)
    feature_variations_offset : Offset32,
};



// =============================================================================
//
// JSTF — Justification Table
//
// <https://www.microsoft.com/typography/otspec/jstf.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// MATH - The mathematical typesetting table
//
// <https://www.microsoft.com/typography/otspec/math.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// avar — Axis Variations Table
//
// <https://www.microsoft.com/typography/otspec/avar.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6avar.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// cvar — CVT Variations Table
//
// <https://www.microsoft.com/typography/otspec/cvar.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6cvar.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// fvar — Font Variations Table
//
// <https://www.microsoft.com/typography/otspec/fvar.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6fvar.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// gvar — Glyph Variations Table
//
// <https://www.microsoft.com/typography/otspec/gvar.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6gvar.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// HVAR — Horizontal Metrics Variations Table
//
// <https://www.microsoft.com/typography/otspec/hvar.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// MVAR — Metrics Variations Table
//
// <https://www.microsoft.com/typography/otspec/mvar.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// STAT — Style Attributes Table
//
// <https://www.microsoft.com/typography/otspec/stat.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// VVAR — Vertical Metrics Variations Table
//
// <https://www.microsoft.com/typography/otspec/vvar.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// COLR - Color Table
//
// <https://www.microsoft.com/typography/otspec/colr.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// CPAL - Color Palette Table
//
// <https://www.microsoft.com/typography/otspec/cpal.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// DSIG - Digital Signature Table
//
// <https://www.microsoft.com/typography/otspec/dsig.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// hdmx - Horizontal Device Metrics
//
// <https://www.microsoft.com/typography/otspec/hdmx.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// kern - Kerning
//
// <https://www.microsoft.com/typography/otspec/kern.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6kern.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// LTSH - Linear Threshold
//
// <https://www.microsoft.com/typography/otspec/ltsh.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// MERG — Merge Table
//
// <https://www.microsoft.com/typography/otspec/merg.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// meta — Metadata Table
//
// <https://www.microsoft.com/typography/otspec/meta.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6meta.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// STAT — Style Attributes Table
//
// <https://www.microsoft.com/typography/otspec/stat.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// PCLT - PCL 5 Table
//
// <https://www.microsoft.com/typography/otspec/pclt.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// VDMX - Vertical Device Metrics
//
// <https://www.microsoft.com/typography/otspec/vdmx.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// vhea — Vertical Header Table
//
// <https://www.microsoft.com/typography/otspec/vhea.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6vhea.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// vmtx - Vertical Metrics Table
//
// <https://www.microsoft.com/typography/otspec/vmtx.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6vmtx.html>
//
// =============================================================================

// TODO
