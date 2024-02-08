// This file is derived from ttf-parser, licenced under Apache-2.0.
// https://github.com/RazrFalcon/ttf-parser/blob/439aaaebd50eb8aed66302e3c1b51fae047f85b2/src/tables/cff/charstring.rs

use crate::binary::read::ReadCtxt;
use crate::cff::charstring::{ArgumentsStack, IsEven};
use crate::cff::outline::Builder;
use crate::cff::CFFError;
use crate::outline::OutlineSink;

pub(crate) struct CharStringParser<'a, B>
where
    B: OutlineSink,
{
    pub stack: ArgumentsStack<'a, f32>,
    pub builder: &'a mut Builder<'a, B>,
    pub x: f32,
    pub y: f32,
    // Used to track if a moveto operator has been encountered before other path building operators.
    // Adobe Technical Note #5177 - The Type 2 Charstring Format:
    // > Every character path and subpath must begin with one of the moveto operators. If the
    // > current path is open when a moveto operator is encountered, the path is closed before
    // > performing the moveto operation.
    pub has_move_to: bool,
    // Used to determine what point a moveto operator is relative to.
    // Adobe Technical Note #5177 - The Type 2 Charstring Format:
    // > For the initial moveto operators in a charstring, the arguments are relative to the (0, 0)
    // > point in the character’s coordinate system; subsequent moveto operators’ arguments are
    // > relative to the current point.
    pub is_first_move_to: bool,
}

impl<B: OutlineSink> CharStringParser<'_, B> {
    pub fn parse_move_to(&mut self, offset: usize) -> Result<(), CFFError> {
        // dx1 dy1

        if self.stack.len() != offset + 2 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        if self.is_first_move_to {
            self.is_first_move_to = false;
        } else {
            self.builder.close();
        }

        self.has_move_to = true;

        self.x += self.stack.at(offset + 0);
        self.y += self.stack.at(offset + 1);
        self.builder.move_to(self.x, self.y);

        self.stack.clear();
        Ok(())
    }

    pub fn parse_horizontal_move_to(&mut self, offset: usize) -> Result<(), CFFError> {
        // dx1

        if self.stack.len() != offset + 1 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        if self.is_first_move_to {
            self.is_first_move_to = false;
        } else {
            self.builder.close();
        }

        self.has_move_to = true;

        self.x += self.stack.at(offset);
        self.builder.move_to(self.x, self.y);

        self.stack.clear();
        Ok(())
    }

    pub fn parse_vertical_move_to(&mut self, offset: usize) -> Result<(), CFFError> {
        // dy1

        if self.stack.len() != offset + 1 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        if self.is_first_move_to {
            self.is_first_move_to = false;
        } else {
            self.builder.close();
        }

        self.has_move_to = true;

        self.y += self.stack.at(offset);
        self.builder.move_to(self.x, self.y);

        self.stack.clear();
        Ok(())
    }

    pub fn parse_line_to(&mut self) -> Result<(), CFFError> {
        // {dxa dya}+

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        if self.stack.len().is_odd() {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        let mut i = 0;
        while i < self.stack.len() {
            self.x += self.stack.at(i + 0);
            self.y += self.stack.at(i + 1);
            self.builder.line_to(self.x, self.y);
            i += 2;
        }

        self.stack.clear();
        Ok(())
    }

    pub fn parse_horizontal_line_to(&mut self) -> Result<(), CFFError> {
        // dx1 {dya dxb}*
        //     {dxa dyb}+

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        if self.stack.is_empty() {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        let mut i = 0;
        while i < self.stack.len() {
            self.x += self.stack.at(i);
            i += 1;
            self.builder.line_to(self.x, self.y);

            if i == self.stack.len() {
                break;
            }

            self.y += self.stack.at(i);
            i += 1;
            self.builder.line_to(self.x, self.y);
        }

        self.stack.clear();
        Ok(())
    }

    pub fn parse_vertical_line_to(&mut self) -> Result<(), CFFError> {
        // dy1 {dxa dyb}*
        //     {dya dxb}+

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        if self.stack.is_empty() {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        let mut i = 0;
        while i < self.stack.len() {
            self.y += self.stack.at(i);
            i += 1;
            self.builder.line_to(self.x, self.y);

            if i == self.stack.len() {
                break;
            }

            self.x += self.stack.at(i);
            i += 1;
            self.builder.line_to(self.x, self.y);
        }

        self.stack.clear();
        Ok(())
    }

    pub fn parse_curve_to(&mut self) -> Result<(), CFFError> {
        // {dxa dya dxb dyb dxc dyc}+

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        if self.stack.len() % 6 != 0 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        let mut i = 0;
        while i < self.stack.len() {
            let x1 = self.x + self.stack.at(i + 0);
            let y1 = self.y + self.stack.at(i + 1);
            let x2 = x1 + self.stack.at(i + 2);
            let y2 = y1 + self.stack.at(i + 3);
            self.x = x2 + self.stack.at(i + 4);
            self.y = y2 + self.stack.at(i + 5);

            self.builder.curve_to(x1, y1, x2, y2, self.x, self.y);
            i += 6;
        }

        self.stack.clear();
        Ok(())
    }

    pub fn parse_curve_line(&mut self) -> Result<(), CFFError> {
        // {dxa dya dxb dyb dxc dyc}+ dxd dyd

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        if self.stack.len() < 8 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        if (self.stack.len() - 2) % 6 != 0 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        let mut i = 0;
        while i < self.stack.len() - 2 {
            let x1 = self.x + self.stack.at(i + 0);
            let y1 = self.y + self.stack.at(i + 1);
            let x2 = x1 + self.stack.at(i + 2);
            let y2 = y1 + self.stack.at(i + 3);
            self.x = x2 + self.stack.at(i + 4);
            self.y = y2 + self.stack.at(i + 5);

            self.builder.curve_to(x1, y1, x2, y2, self.x, self.y);
            i += 6;
        }

        self.x += self.stack.at(i + 0);
        self.y += self.stack.at(i + 1);
        self.builder.line_to(self.x, self.y);

        self.stack.clear();
        Ok(())
    }

    pub fn parse_line_curve(&mut self) -> Result<(), CFFError> {
        // {dxa dya}+ dxb dyb dxc dyc dxd dyd

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        if self.stack.len() < 8 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        if (self.stack.len() - 6).is_odd() {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        let mut i = 0;
        while i < self.stack.len() - 6 {
            self.x += self.stack.at(i + 0);
            self.y += self.stack.at(i + 1);

            self.builder.line_to(self.x, self.y);
            i += 2;
        }

        let x1 = self.x + self.stack.at(i + 0);
        let y1 = self.y + self.stack.at(i + 1);
        let x2 = x1 + self.stack.at(i + 2);
        let y2 = y1 + self.stack.at(i + 3);
        self.x = x2 + self.stack.at(i + 4);
        self.y = y2 + self.stack.at(i + 5);
        self.builder.curve_to(x1, y1, x2, y2, self.x, self.y);

        self.stack.clear();
        Ok(())
    }

    pub fn parse_hh_curve_to(&mut self) -> Result<(), CFFError> {
        // dy1? {dxa dxb dyb dxc}+

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        let mut i = 0;

        // The odd argument count indicates an Y position.
        if self.stack.len().is_odd() {
            self.y += self.stack.at(0);
            i += 1;
        }

        if (self.stack.len() - i) % 4 != 0 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        while i < self.stack.len() {
            let x1 = self.x + self.stack.at(i + 0);
            let y1 = self.y;
            let x2 = x1 + self.stack.at(i + 1);
            let y2 = y1 + self.stack.at(i + 2);
            self.x = x2 + self.stack.at(i + 3);
            self.y = y2;

            self.builder.curve_to(x1, y1, x2, y2, self.x, self.y);
            i += 4;
        }

        self.stack.clear();
        Ok(())
    }

    pub fn parse_vv_curve_to(&mut self) -> Result<(), CFFError> {
        // dx1? {dya dxb dyb dyc}+

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        let mut i = 0;

        // The odd argument count indicates an X position.
        if self.stack.len().is_odd() {
            self.x += self.stack.at(0);
            i += 1;
        }

        if (self.stack.len() - i) % 4 != 0 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        while i < self.stack.len() {
            let x1 = self.x;
            let y1 = self.y + self.stack.at(i + 0);
            let x2 = x1 + self.stack.at(i + 1);
            let y2 = y1 + self.stack.at(i + 2);
            self.x = x2;
            self.y = y2 + self.stack.at(i + 3);

            self.builder.curve_to(x1, y1, x2, y2, self.x, self.y);
            i += 4;
        }

        self.stack.clear();
        Ok(())
    }

    pub fn parse_hv_curve_to(&mut self) -> Result<(), CFFError> {
        // dx1 dx2 dy2 dy3 {dya dxb dyb dxc dxd dxe dye dyf}* dxf?
        //                 {dxa dxb dyb dyc dyd dxe dye dxf}+ dyf?

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        if self.stack.len() < 4 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        self.stack.reverse();
        while !self.stack.is_empty() {
            if self.stack.len() < 4 {
                return Err(CFFError::InvalidArgumentsStackLength);
            }

            let x1 = self.x + self.stack.pop();
            let y1 = self.y;
            let x2 = x1 + self.stack.pop();
            let y2 = y1 + self.stack.pop();
            self.y = y2 + self.stack.pop();
            self.x = x2
                + if self.stack.len() == 1 {
                    self.stack.pop()
                } else {
                    0.0
                };
            self.builder.curve_to(x1, y1, x2, y2, self.x, self.y);
            if self.stack.is_empty() {
                break;
            }

            if self.stack.len() < 4 {
                return Err(CFFError::InvalidArgumentsStackLength);
            }

            let x1 = self.x;
            let y1 = self.y + self.stack.pop();
            let x2 = x1 + self.stack.pop();
            let y2 = y1 + self.stack.pop();
            self.x = x2 + self.stack.pop();
            self.y = y2
                + if self.stack.len() == 1 {
                    self.stack.pop()
                } else {
                    0.0
                };
            self.builder.curve_to(x1, y1, x2, y2, self.x, self.y);
        }

        debug_assert!(self.stack.is_empty());
        Ok(())
    }

    pub fn parse_vh_curve_to(&mut self) -> Result<(), CFFError> {
        // dy1 dx2 dy2 dx3 {dxa dxb dyb dyc dyd dxe dye dxf}* dyf?
        //                 {dya dxb dyb dxc dxd dxe dye dyf}+ dxf?

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        if self.stack.len() < 4 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        self.stack.reverse();
        while !self.stack.is_empty() {
            if self.stack.len() < 4 {
                return Err(CFFError::InvalidArgumentsStackLength);
            }

            let x1 = self.x;
            let y1 = self.y + self.stack.pop();
            let x2 = x1 + self.stack.pop();
            let y2 = y1 + self.stack.pop();
            self.x = x2 + self.stack.pop();
            self.y = y2
                + if self.stack.len() == 1 {
                    self.stack.pop()
                } else {
                    0.0
                };
            self.builder.curve_to(x1, y1, x2, y2, self.x, self.y);
            if self.stack.is_empty() {
                break;
            }

            if self.stack.len() < 4 {
                return Err(CFFError::InvalidArgumentsStackLength);
            }

            let x1 = self.x + self.stack.pop();
            let y1 = self.y;
            let x2 = x1 + self.stack.pop();
            let y2 = y1 + self.stack.pop();
            self.y = y2 + self.stack.pop();
            self.x = x2
                + if self.stack.len() == 1 {
                    self.stack.pop()
                } else {
                    0.0
                };
            self.builder.curve_to(x1, y1, x2, y2, self.x, self.y);
        }

        debug_assert!(self.stack.is_empty());
        Ok(())
    }

    pub fn parse_flex(&mut self) -> Result<(), CFFError> {
        // dx1 dy1 dx2 dy2 dx3 dy3 dx4 dy4 dx5 dy5 dx6 dy6 fd

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        if self.stack.len() != 13 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        let dx1 = self.x + self.stack.at(0);
        let dy1 = self.y + self.stack.at(1);
        let dx2 = dx1 + self.stack.at(2);
        let dy2 = dy1 + self.stack.at(3);
        let dx3 = dx2 + self.stack.at(4);
        let dy3 = dy2 + self.stack.at(5);
        let dx4 = dx3 + self.stack.at(6);
        let dy4 = dy3 + self.stack.at(7);
        let dx5 = dx4 + self.stack.at(8);
        let dy5 = dy4 + self.stack.at(9);
        self.x = dx5 + self.stack.at(10);
        self.y = dy5 + self.stack.at(11);
        self.builder.curve_to(dx1, dy1, dx2, dy2, dx3, dy3);
        self.builder.curve_to(dx4, dy4, dx5, dy5, self.x, self.y);

        self.stack.clear();
        Ok(())
    }

    pub fn parse_flex1(&mut self) -> Result<(), CFFError> {
        // dx1 dy1 dx2 dy2 dx3 dy3 dx4 dy4 dx5 dy5 d6

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        if self.stack.len() != 11 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        let dx1 = self.x + self.stack.at(0);
        let dy1 = self.y + self.stack.at(1);
        let dx2 = dx1 + self.stack.at(2);
        let dy2 = dy1 + self.stack.at(3);
        let dx3 = dx2 + self.stack.at(4);
        let dy3 = dy2 + self.stack.at(5);
        let dx4 = dx3 + self.stack.at(6);
        let dy4 = dy3 + self.stack.at(7);
        let dx5 = dx4 + self.stack.at(8);
        let dy5 = dy4 + self.stack.at(9);

        if (dx5 - self.x).abs() > (dy5 - self.y).abs() {
            self.x = dx5 + self.stack.at(10);
        } else {
            self.y = dy5 + self.stack.at(10);
        }

        self.builder.curve_to(dx1, dy1, dx2, dy2, dx3, dy3);
        self.builder.curve_to(dx4, dy4, dx5, dy5, self.x, self.y);

        self.stack.clear();
        Ok(())
    }

    pub fn parse_hflex(&mut self) -> Result<(), CFFError> {
        // dx1 dx2 dy2 dx3 dx4 dx5 dx6

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        if self.stack.len() != 7 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        let dx1 = self.x + self.stack.at(0);
        let dy1 = self.y;
        let dx2 = dx1 + self.stack.at(1);
        let dy2 = dy1 + self.stack.at(2);
        let dx3 = dx2 + self.stack.at(3);
        let dy3 = dy2;
        let dx4 = dx3 + self.stack.at(4);
        let dy4 = dy2;
        let dx5 = dx4 + self.stack.at(5);
        let dy5 = self.y;
        self.x = dx5 + self.stack.at(6);
        self.builder.curve_to(dx1, dy1, dx2, dy2, dx3, dy3);
        self.builder.curve_to(dx4, dy4, dx5, dy5, self.x, self.y);

        self.stack.clear();
        Ok(())
    }

    pub fn parse_hflex1(&mut self) -> Result<(), CFFError> {
        // dx1 dy1 dx2 dy2 dx3 dx4 dx5 dy5 dx6

        if !self.has_move_to {
            return Err(CFFError::MissingMoveTo);
        }

        if self.stack.len() != 9 {
            return Err(CFFError::InvalidArgumentsStackLength);
        }

        let dx1 = self.x + self.stack.at(0);
        let dy1 = self.y + self.stack.at(1);
        let dx2 = dx1 + self.stack.at(2);
        let dy2 = dy1 + self.stack.at(3);
        let dx3 = dx2 + self.stack.at(4);
        let dy3 = dy2;
        let dx4 = dx3 + self.stack.at(5);
        let dy4 = dy2;
        let dx5 = dx4 + self.stack.at(6);
        let dy5 = dy4 + self.stack.at(7);
        self.x = dx5 + self.stack.at(8);
        self.builder.curve_to(dx1, dy1, dx2, dy2, dx3, dy3);
        self.builder.curve_to(dx4, dy4, dx5, dy5, self.x, self.y);

        self.stack.clear();
        Ok(())
    }

    pub fn parse_int1(&mut self, op: u8) -> Result<(), CFFError> {
        self.stack.push(crate::cff::charstring::parse_int1(op)?)?;
        Ok(())
    }

    pub fn parse_int2(&mut self, op: u8, s: &mut ReadCtxt<'_>) -> Result<(), CFFError> {
        self.stack
            .push(crate::cff::charstring::parse_int2(op, s)?)?;
        Ok(())
    }

    pub fn parse_int3(&mut self, op: u8, s: &mut ReadCtxt<'_>) -> Result<(), CFFError> {
        self.stack
            .push(crate::cff::charstring::parse_int3(op, s)?)?;
        Ok(())
    }

    pub fn parse_fixed(&mut self, s: &mut ReadCtxt<'_>) -> Result<(), CFFError> {
        self.stack.push(crate::cff::charstring::parse_fixed(s)?)?;
        Ok(())
    }
}
