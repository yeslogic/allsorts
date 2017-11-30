/// Specifies a particular encoding and the offset to the corresponding subtable
///
/// <https://www.microsoft.com/typography/otspec/cmap.htm#encodings>
EncodingRecord = struct {
    /// Platform ID.
    platform_id: u16be,
    /// Platform-specific encoding ID.
    encoding_id: u16be,
    /// Byte offset from beginning of table to the subtable for this encoding.
    subtable_offset: u32be,
};

/// <https://www.microsoft.com/typography/otspec/cmap.htm#cmapHeader>
CMap = struct {
    /// Table version number (0)
    version: u16be, // TODO: where x => x == 0
    /// Number of encoding records that follow
    num_tables: u16be,
    /// A list of encoding records
    encoding_records: [EncodingRecord; num_tables],
};
