// This file is derived from ttf-parser, licenced under Apache-2.0.
// https://github.com/RazrFalcon/ttf-parser/blob/439aaaebd50eb8aed66302e3c1b51fae047f85b2/src/tables/cff/argstack.rs

use super::CFFError;

pub struct ArgumentsStack<'a> {
    pub data: &'a mut [f32],
    pub len: usize,
    pub max_len: usize,
}

impl<'a> ArgumentsStack<'a> {
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn push(&mut self, n: f32) -> Result<(), CFFError> {
        if self.len == self.max_len {
            Err(CFFError::ArgumentsStackLimitReached)
        } else {
            self.data[self.len] = n;
            self.len += 1;
            Ok(())
        }
    }

    pub fn at(&self, index: usize) -> f32 {
        self.data[index]
    }

    pub fn pop(&mut self) -> f32 {
        debug_assert!(!self.is_empty());
        self.len -= 1;
        self.data[self.len]
    }

    pub fn reverse(&mut self) {
        if self.is_empty() {
            return;
        }

        // Reverse only the actual data and not the whole stack.
        let (first, _) = self.data.split_at_mut(self.len);
        first.reverse();
    }

    pub fn clear(&mut self) {
        self.len = 0;
    }
}

impl core::fmt::Debug for ArgumentsStack<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_list().entries(&self.data[..self.len]).finish()
    }
}
