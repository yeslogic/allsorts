//! Big5 encoding.

use encoding_rs::{EncoderResult, BIG5};

pub fn unicode_to_big5(u: char) -> Option<u16> {
    let mut encoder = BIG5.new_encoder();
    let src: &mut [u8] = &mut [0, 0, 0, 0];
    let dst: &mut [u8] = &mut [0, 0];
    let (res, _read, written) =
        encoder.encode_from_utf8_without_replacement(u.encode_utf8(src), dst, true);
    match res {
        EncoderResult::InputEmpty => {
            match written {
                1 => Some(u16::from(dst[0])),
                2 => Some((u16::from(dst[0]) << 8) | u16::from(dst[1])),
                _ => None, // should not happen
            }
        }
        EncoderResult::OutputFull => None, // should not happen
        EncoderResult::Unmappable(_) => None,
    }
}
