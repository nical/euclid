// Copyright 2013 The Servo Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! A group of side offsets, which correspond to top/left/bottom/right for borders, padding,
//! and margins in CSS.

use length::Length;
use num::Zero;
use core::fmt;
use core::ops::Add;
use core::marker::PhantomData;
use core::cmp::{Eq, PartialEq};
use core::hash::{Hash};
#[cfg(feature = "serde")]
use serde;

/// A group of 2D side offsets, which correspond to top/left/bottom/right for borders, padding,
/// and margins in CSS, optionally tagged with a unit.
#[repr(C)]
pub struct SideOffsets2D<T, U> {
    pub top: T,
    pub right: T,
    pub bottom: T,
    pub left: T,
    #[doc(hidden)]
    pub _unit: PhantomData<U>,
}

impl<T: Copy, U> Copy for SideOffsets2D<T, U> {}

impl<T: Clone, U> Clone for SideOffsets2D<T, U> {
    fn clone(&self) -> Self {
        SideOffsets2D {
            top: self.top.clone(),
            right: self.right.clone(),
            bottom: self.bottom.clone(),
            left: self.left.clone(),
            _unit: PhantomData,
        }
    }
}

#[cfg(feature = "serde")]
impl<'de, T, U> serde::Deserialize<'de> for SideOffsets2D<T, U>
    where T: serde::Deserialize<'de>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where D: serde::Deserializer<'de>
    {
        let (top, right, bottom, left) = try!(serde::Deserialize::deserialize(deserializer));
        Ok(SideOffsets2D { top, right, bottom, left, _unit: PhantomData })
    }
}

#[cfg(feature = "serde")]
impl<T, U> serde::Serialize for SideOffsets2D<T, U>
    where T: serde::Serialize
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer
    {
        (&self.top, &self.right, &self.bottom, &self.left).serialize(serializer)
    }
}

impl<T, U> Eq for SideOffsets2D<T, U> where T: Eq {}

impl<T, U> PartialEq for SideOffsets2D<T, U>
    where T: PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        self.top == other.top &&
            self.right == other.right &&
            self.bottom == other.bottom &&
            self.left == other.left
    }
}

impl<T, U> Hash for SideOffsets2D<T, U>
    where T: Hash
{
    fn hash<H: ::core::hash::Hasher>(&self, h: &mut H) {
        self.top.hash(h);
        self.right.hash(h);
        self.bottom.hash(h);
        self.left.hash(h);
    }
}

impl<T: fmt::Debug, U> fmt::Debug for SideOffsets2D<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({:?},{:?},{:?},{:?})",
            self.top, self.right, self.bottom, self.left
        )
    }
}

impl<T: Default, U> Default for SideOffsets2D<T, U> {
    fn default() -> Self {
        SideOffsets2D {
            top: Default::default(),
            right: Default::default(),
            bottom: Default::default(),
            left: Default::default(),
            _unit: PhantomData,
        }
    }
}

impl<T: Copy, U> SideOffsets2D<T, U> {
    /// Constructor taking a scalar for each side.
    pub fn new(top: T, right: T, bottom: T, left: T) -> Self {
        SideOffsets2D {
            top,
            right,
            bottom,
            left,
            _unit: PhantomData,
        }
    }

    /// Constructor taking a typed Length for each side.
    pub fn from_lengths(
        top: Length<T, U>,
        right: Length<T, U>,
        bottom: Length<T, U>,
        left: Length<T, U>,
    ) -> Self {
        SideOffsets2D::new(top.0, right.0, bottom.0, left.0)
    }

    /// Access self.top as a typed Length instead of a scalar value.
    pub fn top_typed(&self) -> Length<T, U> {
        Length::new(self.top)
    }

    /// Access self.right as a typed Length instead of a scalar value.
    pub fn right_typed(&self) -> Length<T, U> {
        Length::new(self.right)
    }

    /// Access self.bottom as a typed Length instead of a scalar value.
    pub fn bottom_typed(&self) -> Length<T, U> {
        Length::new(self.bottom)
    }

    /// Access self.left as a typed Length instead of a scalar value.
    pub fn left_typed(&self) -> Length<T, U> {
        Length::new(self.left)
    }

    /// Constructor setting the same value to all sides, taking a scalar value directly.
    pub fn new_all_same(all: T) -> Self {
        SideOffsets2D::new(all, all, all, all)
    }

    /// Constructor setting the same value to all sides, taking a typed Length.
    pub fn from_length_all_same(all: Length<T, U>) -> Self {
        SideOffsets2D::new_all_same(all.0)
    }
}

impl<T, U> SideOffsets2D<T, U>
where
    T: Add<T, Output = T> + Copy,
{
    pub fn horizontal(&self) -> T {
        self.left + self.right
    }

    pub fn vertical(&self) -> T {
        self.top + self.bottom
    }

    pub fn horizontal_typed(&self) -> Length<T, U> {
        Length::new(self.horizontal())
    }

    pub fn vertical_typed(&self) -> Length<T, U> {
        Length::new(self.vertical())
    }
}

impl<T, U> Add for SideOffsets2D<T, U>
where
    T: Copy + Add<T, Output = T>,
{
    type Output = Self;
    fn add(self, other: Self) -> Self {
        SideOffsets2D::new(
            self.top + other.top,
            self.right + other.right,
            self.bottom + other.bottom,
            self.left + other.left,
        )
    }
}

impl<T: Copy + Zero, U> SideOffsets2D<T, U> {
    /// Constructor, setting all sides to zero.
    pub fn zero() -> Self {
        SideOffsets2D::new(Zero::zero(), Zero::zero(), Zero::zero(), Zero::zero())
    }
}
