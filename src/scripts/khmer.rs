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
