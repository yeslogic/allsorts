#![allow(missing_docs)]

//! Parse binary data
//!
//! The is module provides the basis for all font parsing in Allsorts. The parsing approach
//! is inspired by the paper,
//! [The next 700 data description languages](https://collaborate.princeton.edu/en/publications/the-next-700-data-description-languages) by Kathleen Fisher, Yitzhak Mandelbaum, David P. Walker.

use crate::binary::{I16Be, I32Be, I64Be, U16Be, U24Be, U32Be, I8, U8};
use crate::error::ParseError;
use crate::layout::{LayoutCache, LayoutTableType};
use crate::size;
use std::borrow::Cow;
use std::cmp;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;
use std::rc::Rc;

#[derive(Debug, Copy, Clone)]
pub struct ReadEof {}

pub struct ReadBuf<'a> {
    data: Cow<'a, [u8]>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReadScope<'a> {
    base: usize,
    data: &'a [u8],
}

pub struct ReadScopeOwned {
    base: usize,
    data: Box<[u8]>,
}

impl ReadScopeOwned {
    pub fn new(scope: ReadScope<'_>) -> ReadScopeOwned {
        ReadScopeOwned {
            base: scope.base,
            data: Box::from(scope.data),
        }
    }

    pub fn scope(&self) -> ReadScope<'_> {
        ReadScope {
            base: self.base,
            data: &self.data,
        }
    }
}

#[derive(Clone)]
pub struct ReadCtxt<'a> {
    scope: ReadScope<'a>,
    offset: usize,
}

pub struct ReadCache<T> {
    map: HashMap<usize, Rc<T>>,
}

pub trait ReadBinary<'a> {
    type HostType: Sized; // default = Self

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType, ParseError>;
}

pub trait ReadBinaryDep<'a> {
    type Args: Clone;
    type HostType: Sized; // default = Self

    fn read_dep(ctxt: &mut ReadCtxt<'a>, args: Self::Args) -> Result<Self::HostType, ParseError>;
}

pub trait ReadFixedSizeDep<'a>: ReadBinaryDep<'a> {
    /// The number of bytes consumed by `ReadBinaryDep::read`.
    fn size(args: Self::Args) -> usize;
}

/// Read will always succeed if sufficient bytes are available.
pub trait ReadUnchecked<'a> {
    type HostType: Sized; // default = Self

    /// The number of bytes consumed by `read_unchecked`.
    const SIZE: usize;

    /// Must read exactly `SIZE` bytes.
    /// Unsafe as it avoids prohibitively expensive per-byte bounds checking.
    unsafe fn read_unchecked(ctxt: &mut ReadCtxt<'a>) -> Self::HostType;
}

pub trait ReadFrom<'a> {
    type ReadType: ReadUnchecked<'a>;
    fn from(value: <Self::ReadType as ReadUnchecked<'a>>::HostType) -> Self;
}

impl<'a, T> ReadUnchecked<'a> for T
where
    T: ReadFrom<'a>,
{
    type HostType = T;

    const SIZE: usize = T::ReadType::SIZE;

    unsafe fn read_unchecked(ctxt: &mut ReadCtxt<'a>) -> Self::HostType {
        let t = T::ReadType::read_unchecked(ctxt);
        T::from(t)
    }
}

impl<'a, T> ReadBinary<'a> for T
where
    T: ReadUnchecked<'a>,
{
    type HostType = T::HostType;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType, ParseError> {
        ctxt.check_avail(T::SIZE)?;
        Ok(unsafe { T::read_unchecked(ctxt) })
        // Safe because we have `SIZE` bytes available.
    }
}

impl<'a, T> ReadBinaryDep<'a> for T
where
    T: ReadBinary<'a>,
{
    type Args = ();
    type HostType = T::HostType;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, (): Self::Args) -> Result<Self::HostType, ParseError> {
        T::read(ctxt)
    }
}

impl<'a, T> ReadFixedSizeDep<'a> for T
where
    T: ReadUnchecked<'a>,
{
    fn size((): ()) -> usize {
        T::SIZE
    }
}

pub trait CheckIndex {
    fn check_index(&self, index: usize) -> Result<(), ParseError>;
}

#[derive(Clone)]
pub struct ReadArray<'a, T: ReadFixedSizeDep<'a>> {
    scope: ReadScope<'a>,
    length: usize,
    args: T::Args,
}

pub struct ReadArrayIter<'a, T: ReadUnchecked<'a>> {
    ctxt: ReadCtxt<'a>,
    length: usize,
    phantom: PhantomData<T>,
}

pub struct ReadArrayDepIter<'a, 'b, T: ReadFixedSizeDep<'a>> {
    array: &'b ReadArray<'a, T>,
    index: usize,
}

#[derive(Clone)]
pub enum ReadArrayCow<'a, T>
where
    T: ReadUnchecked<'a>,
{
    Owned(Vec<T::HostType>),
    Borrowed(ReadArray<'a, T>),
}

pub struct ReadArrayCowIter<'a, 'b, T: ReadUnchecked<'a>> {
    array: &'b ReadArrayCow<'a, T>,
    index: usize,
}

impl<'a, T: ReadUnchecked<'a>> ReadArrayCow<'a, T> {
    pub fn len(&self) -> usize {
        match self {
            ReadArrayCow::Borrowed(array) => array.len(),
            ReadArrayCow::Owned(vec) => vec.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            ReadArrayCow::Borrowed(array) => array.is_empty(),
            ReadArrayCow::Owned(vec) => vec.is_empty(),
        }
    }

    pub fn read_item(&self, index: usize) -> Result<T::HostType, ParseError>
    where
        T::HostType: Copy,
    {
        match self {
            ReadArrayCow::Borrowed(array) => array.read_item(index),
            ReadArrayCow::Owned(vec) => Ok(vec[index]),
        }
    }

    pub fn get_item(&self, index: usize) -> <T as ReadUnchecked<'a>>::HostType
    where
        T: ReadUnchecked<'a>,
        <T as ReadUnchecked<'a>>::HostType: Copy,
    {
        match self {
            ReadArrayCow::Borrowed(array) => array.get_item(index),
            ReadArrayCow::Owned(vec) => vec[index],
        }
    }

    // subarray and iter_res are not yet implemented

    pub fn iter<'b>(&'b self) -> ReadArrayCowIter<'a, 'b, T> {
        ReadArrayCowIter {
            array: self,
            index: 0,
        }
    }
}

impl<'a, T: ReadUnchecked<'a>> CheckIndex for ReadArrayCow<'a, T> {
    fn check_index(&self, index: usize) -> Result<(), ParseError> {
        if index < self.len() {
            Ok(())
        } else {
            Err(ParseError::BadIndex)
        }
    }
}

impl<'a> ReadScope<'a> {
    pub fn new(data: &'a [u8]) -> ReadScope<'a> {
        let base = 0;
        ReadScope { base, data }
    }

    pub fn data(&self) -> &'a [u8] {
        self.data
    }

    pub fn offset(&self, offset: usize) -> ReadScope<'a> {
        let base = self.base + offset;
        let data = self.data.get(offset..).unwrap_or(&[]);
        ReadScope { base, data }
    }

    pub fn offset_length(&self, offset: usize, length: usize) -> Result<ReadScope<'a>, ParseError> {
        if offset < self.data.len() || length == 0 {
            let data = &self.data[offset..];
            if length <= data.len() {
                let base = self.base + offset;
                let data = &data[0..length];
                Ok(ReadScope { base, data })
            } else {
                Err(ParseError::BadEof)
            }
        } else {
            Err(ParseError::BadOffset)
        }
    }

    pub fn ctxt(&self) -> ReadCtxt<'a> {
        ReadCtxt::new(self.clone())
    }

    pub fn read<T: ReadBinaryDep<'a, Args = ()>>(&self) -> Result<T::HostType, ParseError> {
        self.ctxt().read::<T>()
    }

    pub fn read_dep<T: ReadBinaryDep<'a>>(&self, args: T::Args) -> Result<T::HostType, ParseError> {
        self.ctxt().read_dep::<T>(args)
    }

    pub fn read_cache<'b, T>(
        &self,
        cache: &mut ReadCache<T::HostType>,
    ) -> Result<Rc<T::HostType>, ParseError>
    where
        T: 'static + ReadBinaryDep<'a, Args = ()>,
    {
        match cache.map.entry(self.base) {
            Entry::Vacant(entry) => {
                let t = Rc::new(self.read::<T>()?);
                Ok(Rc::clone(entry.insert(t)))
            }
            Entry::Occupied(entry) => Ok(Rc::clone(entry.get())),
        }
    }

    pub fn read_cache_state<'b, T, Table>(
        &self,
        cache: &mut ReadCache<T::HostType>,
        state: LayoutCache<Table>,
    ) -> Result<Rc<T::HostType>, ParseError>
    where
        T: 'static + ReadBinaryDep<'a, Args = LayoutCache<Table>>,
        Table: LayoutTableType,
    {
        match cache.map.entry(self.base) {
            Entry::Vacant(entry) => {
                let t = Rc::new(self.read_dep::<T>(state)?);
                Ok(Rc::clone(entry.insert(t)))
            }
            Entry::Occupied(entry) => Ok(Rc::clone(entry.get())),
        }
    }
}

impl<T> ReadCache<T> {
    pub fn new() -> Self {
        let map = HashMap::new();
        ReadCache { map }
    }
}

impl<'a> ReadCtxt<'a> {
    /// ReadCtxt is constructed by calling `ReadScope::ctxt`.
    fn new(scope: ReadScope<'a>) -> ReadCtxt<'a> {
        ReadCtxt { scope, offset: 0 }
    }

    pub fn check(&self, cond: bool) -> Result<(), ParseError> {
        match cond {
            true => Ok(()),
            false => Err(ParseError::BadValue),
        }
    }

    /// Check a condition, returning `ParseError::BadVersion` if `false`.
    ///
    /// Intended for use in checking versions read from data. Example:
    ///
    /// ```
    /// use allsorts::binary::read::ReadScope;
    /// use allsorts::error::ParseError;
    ///
    /// let scope = ReadScope::new(&[0, 2]);
    /// let mut ctxt = scope.ctxt();
    /// let major_version = ctxt.read_u16be().expect("unable to read version");
    ///
    /// assert!(ctxt.check_version(major_version == 2).is_ok());
    /// assert_eq!(ctxt.check_version(major_version == 1), Err(ParseError::BadVersion));
    /// ```
    pub fn check_version(&self, cond: bool) -> Result<(), ParseError> {
        match cond {
            true => Ok(()),
            false => Err(ParseError::BadVersion),
        }
    }

    pub fn scope(&self) -> ReadScope<'a> {
        self.scope.offset(self.offset)
    }

    pub fn read<T: ReadBinaryDep<'a, Args = ()>>(&mut self) -> Result<T::HostType, ParseError> {
        T::read_dep(self, ())
    }

    pub fn read_dep<T: ReadBinaryDep<'a>>(
        &mut self,
        args: T::Args,
    ) -> Result<T::HostType, ParseError> {
        T::read_dep(self, args)
    }

    pub fn bytes_available(&self) -> bool {
        self.offset < self.scope.data.len()
    }

    fn check_avail(&self, length: usize) -> Result<(), ReadEof> {
        match self.offset.checked_add(length) {
            Some(endpos) if endpos <= self.scope.data.len() => Ok(()),
            _ => Err(ReadEof {}),
        }
    }

    unsafe fn read_unchecked_u8(&mut self) -> u8 {
        let byte = *self.scope.data.get_unchecked(self.offset);
        self.offset += 1;
        byte
    }

    unsafe fn read_unchecked_i8(&mut self) -> i8 {
        self.read_unchecked_u8() as i8
    }

    unsafe fn read_unchecked_u16be(&mut self) -> u16 {
        let hi = u16::from(*self.scope.data.get_unchecked(self.offset));
        let lo = u16::from(*self.scope.data.get_unchecked(self.offset + 1));
        self.offset += 2;
        (hi << 8) | lo
    }

    unsafe fn read_unchecked_i16be(&mut self) -> i16 {
        self.read_unchecked_u16be() as i16
    }

    unsafe fn read_unchecked_u24be(&mut self) -> u32 {
        let b0 = u32::from(*self.scope.data.get_unchecked(self.offset));
        let b1 = u32::from(*self.scope.data.get_unchecked(self.offset + 1));
        let b2 = u32::from(*self.scope.data.get_unchecked(self.offset + 2));
        self.offset += 3;
        (b0 << 16) | (b1 << 8) | b2
    }

    unsafe fn read_unchecked_u32be(&mut self) -> u32 {
        let b0 = u32::from(*self.scope.data.get_unchecked(self.offset));
        let b1 = u32::from(*self.scope.data.get_unchecked(self.offset + 1));
        let b2 = u32::from(*self.scope.data.get_unchecked(self.offset + 2));
        let b3 = u32::from(*self.scope.data.get_unchecked(self.offset + 3));
        self.offset += 4;
        (b0 << 24) | (b1 << 16) | (b2 << 8) | b3
    }

    unsafe fn read_unchecked_i32be(&mut self) -> i32 {
        self.read_unchecked_u32be() as i32
    }

    unsafe fn read_unchecked_u64be(&mut self) -> u64 {
        let hi = u64::from(self.read_unchecked_u32be());
        let lo = u64::from(self.read_unchecked_u32be());
        (hi << 32) | lo
    }

    unsafe fn read_unchecked_i64be(&mut self) -> i64 {
        self.read_unchecked_u64be() as i64
    }

    pub fn read_u8(&mut self) -> Result<u8, ReadEof> {
        self.check_avail(1)?;
        Ok(unsafe { self.read_unchecked_u8() })
        // Safe because we have 1 byte available.
    }

    pub fn read_i8(&mut self) -> Result<i8, ReadEof> {
        self.check_avail(1)?;
        Ok(unsafe { self.read_unchecked_i8() })
        // Safe because we have 1 byte available.
    }

    pub fn read_u16be(&mut self) -> Result<u16, ReadEof> {
        self.check_avail(2)?;
        Ok(unsafe { self.read_unchecked_u16be() })
        // Safe because we have 2 bytes available.
    }

    pub fn read_i16be(&mut self) -> Result<i16, ReadEof> {
        self.check_avail(2)?;
        Ok(unsafe { self.read_unchecked_i16be() })
        // Safe because we have 2 bytes available.
    }

    pub fn read_u32be(&mut self) -> Result<u32, ReadEof> {
        self.check_avail(4)?;
        Ok(unsafe { self.read_unchecked_u32be() })
        // Safe because we have 4 bytes available.
    }

    pub fn read_i32be(&mut self) -> Result<i32, ReadEof> {
        self.check_avail(4)?;
        Ok(unsafe { self.read_unchecked_i32be() })
        // Safe because we have 4 bytes available.
    }

    pub fn read_u64be(&mut self) -> Result<u64, ReadEof> {
        self.check_avail(8)?;
        Ok(unsafe { self.read_unchecked_u64be() })
        // Safe because we have 8 bytes available.
    }

    pub fn read_i64be(&mut self) -> Result<i64, ReadEof> {
        self.check_avail(8)?;
        Ok(unsafe { self.read_unchecked_i64be() })
        // Safe because we have 8 bytes available.
    }

    pub fn read_array<T: ReadUnchecked<'a>>(
        &mut self,
        length: usize,
    ) -> Result<ReadArray<'a, T>, ParseError> {
        let scope = self.read_scope(length * T::SIZE)?;
        let args = ();
        Ok(ReadArray {
            scope,
            length,
            args,
        })
    }

    pub fn read_array_upto_hack<T: ReadUnchecked<'a>>(
        &mut self,
        length: usize,
    ) -> Result<ReadArray<'a, T>, ParseError> {
        let start_pos = self.offset;
        let buf_size = self.scope.data.len();
        let avail_bytes = cmp::max(0, buf_size - start_pos);
        let max_length = avail_bytes / T::SIZE;
        let length = cmp::min(length, max_length);
        self.read_array(length)
    }

    /// Read up to and including the supplied nibble.
    pub fn read_until_nibble(&mut self, nibble: u8) -> Result<&'a [u8], ReadEof> {
        let end = self.scope.data[self.offset..]
            .iter()
            .position(|&b| (b >> 4) == nibble || (b & 0xF) == nibble)
            .ok_or(ReadEof {})?;
        self.read_slice(end + 1)
    }

    pub fn read_array_dep<T: ReadFixedSizeDep<'a>>(
        &mut self,
        length: usize,
        args: T::Args,
    ) -> Result<ReadArray<'a, T>, ParseError> {
        let scope = self.read_scope(length * T::size(args.clone()))?;
        Ok(ReadArray {
            scope,
            length,
            args: args.clone(),
        })
    }

    pub fn read_scope(&mut self, length: usize) -> Result<ReadScope<'a>, ReadEof> {
        if let Ok(scope) = self.scope.offset_length(self.offset, length) {
            self.offset += length;
            Ok(scope)
        } else {
            Err(ReadEof {})
        }
    }

    pub fn read_slice(&mut self, length: usize) -> Result<&'a [u8], ReadEof> {
        let scope = self.read_scope(length)?;
        Ok(scope.data)
    }
}

impl<'a> ReadBuf<'a> {
    pub fn scope(&'a self) -> ReadScope<'a> {
        ReadScope::new(&*self.data)
    }

    pub fn into_data(self) -> Cow<'a, [u8]> {
        self.data
    }
}

impl<'a> From<&'a [u8]> for ReadBuf<'a> {
    fn from(data: &'a [u8]) -> ReadBuf<'a> {
        ReadBuf {
            data: Cow::Borrowed(data),
        }
    }
}

impl<'a> From<Vec<u8>> for ReadBuf<'a> {
    fn from(data: Vec<u8>) -> ReadBuf<'a> {
        ReadBuf {
            data: Cow::Owned(data),
        }
    }
}

impl<'a, T: ReadFixedSizeDep<'a>> ReadArray<'a, T> {
    pub fn len(&self) -> usize {
        self.length
    }

    pub fn is_empty(&self) -> bool {
        self.length == 0
    }

    pub fn read_item(&self, index: usize) -> Result<T::HostType, ParseError> {
        if index < self.length {
            let size = T::size(self.args.clone());
            let offset = index * size;
            let scope = self.scope.offset_length(offset, size).unwrap();
            let mut ctxt = scope.ctxt();
            T::read_dep(&mut ctxt, self.args.clone())
        } else {
            panic!("ReadArray::read_item: index out of bounds");
        }
    }

    pub fn get_item(&self, index: usize) -> <T as ReadUnchecked<'a>>::HostType
    where
        T: ReadUnchecked<'a>,
    {
        if index < self.length {
            let offset = index * T::SIZE;
            let scope = self.scope.offset_length(offset, T::SIZE).unwrap();
            let mut ctxt = scope.ctxt();
            unsafe { T::read_unchecked(&mut ctxt) } // Safe because we have `SIZE` bytes available.
        } else {
            panic!("ReadArray::get_item: index out of bounds");
        }
    }

    pub fn subarray(&self, index: usize) -> Self {
        if index < self.length {
            let offset = index * T::size(self.args.clone());
            ReadArray {
                scope: self.scope.offset(offset),
                length: self.length - index,
                args: self.args.clone(),
            }
        } else {
            ReadArray {
                scope: ReadScope::new(&[]),
                length: 0,
                args: self.args.clone(),
            }
        }
    }

    pub fn to_vec(&self) -> Vec<<T as ReadUnchecked<'a>>::HostType>
    where
        T: ReadUnchecked<'a>,
    {
        let mut vec = Vec::with_capacity(self.length);
        for t in self.iter() {
            vec.push(t);
        }
        vec
    }

    pub fn read_to_vec(&self) -> Result<Vec<T::HostType>, ParseError> {
        let mut vec = Vec::with_capacity(self.length);
        for res in self.iter_res() {
            let t = res?;
            vec.push(t);
        }
        Ok(vec)
    }

    pub fn iter(&self) -> ReadArrayIter<'a, T>
    where
        T: ReadUnchecked<'a>,
    {
        ReadArrayIter {
            ctxt: self.scope.ctxt(),
            length: self.length,
            phantom: PhantomData,
        }
    }

    pub fn iter_res<'b>(&'b self) -> ReadArrayDepIter<'a, 'b, T> {
        ReadArrayDepIter {
            array: self,
            index: 0,
        }
    }
}

impl<'a, T: ReadFixedSizeDep<'a>> CheckIndex for ReadArray<'a, T> {
    fn check_index(&self, index: usize) -> Result<(), ParseError> {
        if index < self.len() {
            Ok(())
        } else {
            Err(ParseError::BadIndex)
        }
    }
}

impl<T> CheckIndex for Vec<T> {
    fn check_index(&self, index: usize) -> Result<(), ParseError> {
        if index < self.len() {
            Ok(())
        } else {
            Err(ParseError::BadIndex)
        }
    }
}

impl<'a, 'b, T: ReadUnchecked<'a>> IntoIterator for &'b ReadArray<'a, T> {
    type Item = T::HostType;
    type IntoIter = ReadArrayIter<'a, T>;
    fn into_iter(self) -> ReadArrayIter<'a, T> {
        self.iter()
    }
}

impl<'a, T: ReadUnchecked<'a>> Iterator for ReadArrayIter<'a, T> {
    type Item = T::HostType;

    fn next(&mut self) -> Option<T::HostType> {
        if self.length > 0 {
            self.length -= 1;
            Some(unsafe { T::read_unchecked(&mut self.ctxt) })
        // Safe because we have (at least) `SIZE` bytes available.
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.length, Some(self.length))
    }
}

impl<'a, T: ReadUnchecked<'a>> ExactSizeIterator for ReadArrayIter<'a, T> {}

impl<'a, 'b, T: ReadUnchecked<'a>> IntoIterator for &'b ReadArrayCow<'a, T>
where
    T::HostType: Copy,
{
    type Item = T::HostType;
    type IntoIter = ReadArrayCowIter<'a, 'b, T>;

    fn into_iter(self) -> ReadArrayCowIter<'a, 'b, T> {
        self.iter()
    }
}

impl<'a, 'b, T: ReadUnchecked<'a>> Iterator for ReadArrayCowIter<'a, 'b, T>
where
    T::HostType: Copy,
{
    type Item = T::HostType;

    fn next(&mut self) -> Option<T::HostType> {
        if self.index < self.array.len() {
            let item = self.array.get_item(self.index);
            self.index += 1;
            Some(item)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.index < self.array.len() {
            let length = self.array.len() - self.index;
            (length, Some(length))
        } else {
            (0, Some(0))
        }
    }
}

impl<'a, 'b, T: ReadFixedSizeDep<'a>> Iterator for ReadArrayDepIter<'a, 'b, T> {
    type Item = Result<T::HostType, ParseError>;

    fn next(&mut self) -> Option<Result<T::HostType, ParseError>> {
        if self.index < self.array.len() {
            let result = self.array.read_item(self.index);
            self.index += 1;
            Some(result)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.index < self.array.len() {
            let length = self.array.len() - self.index;
            (length, Some(length))
        } else {
            (0, Some(0))
        }
    }
}

impl<'a, T: ReadUnchecked<'a>> ReadArray<'a, T> {
    pub fn empty() -> ReadArray<'a, T> {
        ReadArray {
            scope: ReadScope::new(&[]),
            length: 0,
            args: (),
        }
    }
}

impl<'a> ReadUnchecked<'a> for U8 {
    type HostType = u8;

    const SIZE: usize = size::U8;

    unsafe fn read_unchecked(ctxt: &mut ReadCtxt<'a>) -> u8 {
        ctxt.read_unchecked_u8()
    }
}

impl<'a> ReadUnchecked<'a> for I8 {
    type HostType = i8;

    const SIZE: usize = size::I8;

    unsafe fn read_unchecked(ctxt: &mut ReadCtxt<'a>) -> i8 {
        ctxt.read_unchecked_i8()
    }
}

impl<'a> ReadUnchecked<'a> for U16Be {
    type HostType = u16;

    const SIZE: usize = size::U16;

    unsafe fn read_unchecked(ctxt: &mut ReadCtxt<'a>) -> u16 {
        ctxt.read_unchecked_u16be()
    }
}

impl<'a> ReadUnchecked<'a> for I16Be {
    type HostType = i16;

    const SIZE: usize = size::I16;

    unsafe fn read_unchecked(ctxt: &mut ReadCtxt<'a>) -> i16 {
        ctxt.read_unchecked_i16be()
    }
}

impl<'a> ReadUnchecked<'a> for U24Be {
    type HostType = u32;

    const SIZE: usize = size::U24;

    unsafe fn read_unchecked(ctxt: &mut ReadCtxt<'a>) -> u32 {
        ctxt.read_unchecked_u24be()
    }
}

impl<'a> ReadUnchecked<'a> for U32Be {
    type HostType = u32;

    const SIZE: usize = size::U32;

    unsafe fn read_unchecked(ctxt: &mut ReadCtxt<'a>) -> u32 {
        ctxt.read_unchecked_u32be()
    }
}

impl<'a> ReadUnchecked<'a> for I32Be {
    type HostType = i32;

    const SIZE: usize = size::I32;

    unsafe fn read_unchecked(ctxt: &mut ReadCtxt<'a>) -> i32 {
        ctxt.read_unchecked_i32be()
    }
}

impl<'a> ReadUnchecked<'a> for I64Be {
    type HostType = i64;

    const SIZE: usize = size::I64;

    unsafe fn read_unchecked(ctxt: &mut ReadCtxt<'a>) -> i64 {
        ctxt.read_unchecked_i64be()
    }
}

impl<'a, T1, T2> ReadUnchecked<'a> for (T1, T2)
where
    T1: ReadUnchecked<'a>,
    T2: ReadUnchecked<'a>,
{
    type HostType = (T1::HostType, T2::HostType);

    const SIZE: usize = T1::SIZE + T2::SIZE;

    unsafe fn read_unchecked(ctxt: &mut ReadCtxt<'a>) -> Self::HostType {
        let t1 = T1::read_unchecked(ctxt);
        let t2 = T2::read_unchecked(ctxt);
        (t1, t2)
    }
}

impl<'a, T1, T2, T3> ReadUnchecked<'a> for (T1, T2, T3)
where
    T1: ReadUnchecked<'a>,
    T2: ReadUnchecked<'a>,
    T3: ReadUnchecked<'a>,
{
    type HostType = (T1::HostType, T2::HostType, T3::HostType);

    const SIZE: usize = T1::SIZE + T2::SIZE + T3::SIZE;

    unsafe fn read_unchecked(ctxt: &mut ReadCtxt<'a>) -> Self::HostType {
        let t1 = T1::read_unchecked(ctxt);
        let t2 = T2::read_unchecked(ctxt);
        let t3 = T3::read_unchecked(ctxt);
        (t1, t2, t3)
    }
}

impl<'a, T> fmt::Debug for ReadArrayCow<'a, T>
where
    T: ReadUnchecked<'a>,
    <T as ReadUnchecked<'a>>::HostType: Copy + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<'a, T> fmt::Debug for ReadArray<'a, T>
where
    T: ReadUnchecked<'a>,
    <T as ReadUnchecked<'a>>::HostType: Copy + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.debug_list().entries(self.iter()).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_u24be() {
        let scope = ReadScope::new(&[1, 2, 3]);
        assert_eq!(scope.read::<U24Be>().unwrap(), 0x10203);
    }
}
