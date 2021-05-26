pub trait SyllableChar {
    fn char(&self) -> char;
}

pub fn match_unit<T: SyllableChar>() -> impl Fn(&[T]) -> Option<usize> {
    |_cs: &[T]| Some(0)
}

pub fn match_one<T: SyllableChar>(f: impl Fn(char) -> bool) -> impl Fn(&[T]) -> Option<usize> {
    move |cs: &[T]| match cs.get(0) {
        Some(c) if f(c.char()) => Some(1),
        _ => None,
    }
}

pub fn match_nonempty<T: SyllableChar>(
    f: impl Fn(&[T]) -> Option<usize>,
) -> impl Fn(&[T]) -> Option<usize> {
    move |cs: &[T]| f(cs).filter(|&n| n > 0)
}

pub fn match_optional<T: SyllableChar>(
    f: impl Fn(&[T]) -> Option<usize>,
) -> impl Fn(&[T]) -> Option<usize> {
    move |cs: &[T]| f(cs).or(Some(0))
}

pub fn match_optional_seq<T: SyllableChar>(
    f: impl Fn(&[T]) -> Option<usize>,
    g: impl Fn(&[T]) -> Option<usize>,
) -> impl Fn(&[T]) -> Option<usize> {
    move |cs: &[T]| match_either(&g, match_seq(&f, &g))(cs)
}

pub fn match_repeat_num<T: SyllableChar>(
    num: usize,
    f: impl Fn(&[T]) -> Option<usize>,
) -> impl Fn(&[T]) -> Option<usize> {
    move |mut cs: &[T]| {
        let mut total = 0;
        for _ in 0..num {
            let n = f(cs)?;
            total += n;
            cs = &cs[n..];
        }
        Some(total)
    }
}

pub fn match_repeat_upto<T: SyllableChar>(
    max: usize,
    f: impl Fn(&[T]) -> Option<usize>,
    g: impl Fn(&[T]) -> Option<usize>,
) -> impl Fn(&[T]) -> Option<usize> {
    move |cs: &[T]| {
        for i in (0..=max).rev() {
            if let Some(nf) = match_repeat_num(i, &f)(cs) {
                if let Some(ng) = g(&cs[nf..]) {
                    return Some(nf + ng);
                }
            }
        }
        None
    }
}

pub fn match_seq<T: SyllableChar>(
    f1: impl Fn(&[T]) -> Option<usize>,
    f2: impl Fn(&[T]) -> Option<usize>,
) -> impl Fn(&[T]) -> Option<usize> {
    move |cs: &[T]| {
        let n1 = f1(cs)?;
        let n2 = f2(&cs[n1..])?;
        Some(n1 + n2)
    }
}

pub fn match_either<T: SyllableChar>(
    f1: impl Fn(&[T]) -> Option<usize>,
    f2: impl Fn(&[T]) -> Option<usize>,
) -> impl Fn(&[T]) -> Option<usize> {
    move |cs: &[T]| {
        let n1 = f1(cs);
        let n2 = f2(cs);
        std::cmp::max(n1, n2)
    }
}

pub fn match_either_seq<T: SyllableChar>(
    f1: impl Fn(&[T]) -> Option<usize>,
    f2: impl Fn(&[T]) -> Option<usize>,
    g: impl Fn(&[T]) -> Option<usize>,
) -> impl Fn(&[T]) -> Option<usize> {
    move |cs: &[T]| {
        let n1 = match_seq(&f1, &g)(cs);
        let n2 = match_seq(&f2, &g)(cs);
        std::cmp::max(n1, n2)
    }
}
