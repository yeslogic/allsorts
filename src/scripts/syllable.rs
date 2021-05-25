pub trait SyllableChar {
    fn char(&self) -> char;
}

pub fn match_unit<T: SyllableChar>(_cs: &[T]) -> Option<usize> {
    Some(0)
}

pub fn match_one<T: SyllableChar>(cs: &[T], f: impl FnOnce(char) -> bool) -> Option<usize> {
    match cs.get(0) {
        Some(c) if f(c.char()) => Some(1),
        _ => None,
    }
}

pub fn match_nonempty<T: SyllableChar>(
    cs: &[T],
    f: impl FnOnce(&[T]) -> Option<usize>,
) -> Option<usize> {
    f(cs).filter(|&n| n > 0)
}

pub fn match_optional<T: SyllableChar>(
    cs: &[T],
    f: impl FnOnce(&[T]) -> Option<usize>,
) -> Option<usize> {
    f(cs).or(Some(0))
}

pub fn match_optional_seq<T: SyllableChar>(
    cs: &[T],
    f: impl FnOnce(&[T]) -> Option<usize>,
    g: impl Fn(&[T]) -> Option<usize>,
) -> Option<usize> {
    match_either(cs, &g, |cs| match_seq(cs, f, &g))
}

pub fn match_repeat_num<T: SyllableChar>(
    mut cs: &[T],
    num: usize,
    f: impl Fn(&[T]) -> Option<usize>,
) -> Option<usize> {
    let mut total = 0;
    for _ in 0..num {
        let n = f(cs)?;
        total += n;
        cs = &cs[n..];
    }
    Some(total)
}

pub fn match_repeat_upto<T: SyllableChar>(
    cs: &[T],
    max: usize,
    f: impl Fn(&[T]) -> Option<usize>,
    g: impl Fn(&[T]) -> Option<usize>,
) -> Option<usize> {
    for i in (0..=max).rev() {
        if let Some(nf) = match_repeat_num(cs, i, &f) {
            if let Some(ng) = g(&cs[nf..]) {
                return Some(nf + ng);
            }
        }
    }
    None
}

pub fn match_seq<T: SyllableChar>(
    cs: &[T],
    f1: impl FnOnce(&[T]) -> Option<usize>,
    f2: impl FnOnce(&[T]) -> Option<usize>,
) -> Option<usize> {
    let n1 = f1(cs)?;
    let n2 = f2(&cs[n1..])?;
    Some(n1 + n2)
}

pub fn match_either<T: SyllableChar>(
    cs: &[T],
    f1: impl FnOnce(&[T]) -> Option<usize>,
    f2: impl FnOnce(&[T]) -> Option<usize>,
) -> Option<usize> {
    let n1 = f1(cs);
    let n2 = f2(cs);
    std::cmp::max(n1, n2)
}

pub fn match_either_seq<T: SyllableChar>(
    cs: &[T],
    f1: impl FnOnce(&[T]) -> Option<usize>,
    f2: impl FnOnce(&[T]) -> Option<usize>,
    g: impl Fn(&[T]) -> Option<usize>,
) -> Option<usize> {
    let n1 = match_seq(cs, f1, &g);
    let n2 = match_seq(cs, f2, &g);
    std::cmp::max(n1, n2)
}
