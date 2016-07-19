// Copyright 2013 The Servo Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use length::{ Length, Untyped };
use scale_factor::{ ScaleFactor };
use num::Zero;

use num_traits::NumCast;
use std::fmt;
use std::ops::{Mul, Div};
use std::marker::PhantomData;

define_vector! {
    #[derive(RustcDecodable, RustcEncodable)]
    pub struct Size2D<T, U> {
        pub width: T,
        pub height: T,
        _unit: PhantomData<U>,
    }
}

impl<T: Copy, U> Copy for Size2D<T, U> {}

impl<T: Clone, U> Clone for Size2D<T, U> {
    fn clone(&self) -> Size2D<T, U> {
        Size2D::new(self.width.clone(), self.height.clone())
    }
}

impl<T: PartialEq, U> PartialEq<Size2D<T, U>> for Size2D<T, U> {
    fn eq(&self, other: &Size2D<T, U>) -> bool {
        self.width.eq(&other.width) && self.height.eq(&other.height)
    }
}

impl<T: Eq, U> Eq for Size2D<T, U> {}

impl<T: fmt::Debug, U> fmt::Debug for Size2D<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}×{:?}", self.width, self.height)
    }
}

impl<T: fmt::Display, U> fmt::Display for Size2D<T, U> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "({}x{})", self.width, self.height)
    }
}

impl<T, U> Size2D<T, U> {
    pub fn new(width: T, height: T) -> Size2D<T, U> {
        Size2D {
            width: width,
            height: height,
            _unit: PhantomData,
        }
    }
}

impl<T:Copy + Clone + Mul<T, Output=U>, U> Size2D<T, U> {
    pub fn area(&self) -> U { self.width * self.height }
}

impl<T: Zero, U> Size2D<T, U> {
    pub fn zero() -> Size2D<T, U> {
        Size2D::new(
            Zero::zero(),
            Zero::zero(),
        )
    }
}

impl<T: Zero, U> Zero for Size2D<T, U> {
    fn zero() -> Size2D<T, U> {
        Size2D::new(
            Zero::zero(),
            Zero::zero(),
        )
    }
}

impl<T: Copy + Mul<T, Output=T>, U> Mul<T> for Size2D<T, U> {
    type Output = Size2D<T, U>;
    #[inline]
    fn mul(self, scale: T) -> Size2D<T, U> {
        Size2D::new(self.width * scale, self.height * scale)
    }
}

impl<T: Copy + Div<T, Output=T>, U> Div<T> for Size2D<T, U> {
    type Output = Size2D<T, U>;
    #[inline]
    fn div(self, scale: T) -> Size2D<T, U> {
        Size2D::new(self.width / scale, self.height / scale)
    }
}

impl<T: Copy + Mul<T, Output=T>, U1, U2> Mul<ScaleFactor<T, U1, U2>> for Size2D<T, U1> {
    type Output = Size2D<T, U2>;
    #[inline]
    fn mul(self, scale: ScaleFactor<T, U1, U2>) -> Size2D<T, U2> {
        Size2D::new(self.width * scale.get(), self.height * scale.get())
    }
}

impl<T: Copy + Div<T, Output=T>, U1, U2> Div<ScaleFactor<T, U1, U2>> for Size2D<T, U2> {
    type Output = Size2D<T, U1>;
    #[inline]
    fn div(self, scale: ScaleFactor<T, U1, U2>) -> Size2D<T, U1> {
        Size2D::new(self.width / scale.get(), self.height / scale.get())
    }
}

// TODO[nical]
//impl<Scale: Copy, T0: Mul<Scale, Output=T1>, T1: Clone> Mul<Scale> for Size2D<T0> {
//    type Output = Size2D<T1>;
//    #[inline]
//    fn mul(self, scale: Scale) -> Size2D<T1> {
//        Size2D::new(self.width * scale, self.height * scale)
//    }
//}
//
//impl<Scale: Copy, T0: Div<Scale, Output=T1>, T1: Clone> Div<Scale> for Size2D<T0> {
//    type Output = Size2D<T1>;
//    #[inline]
//    fn div(self, scale: Scale) -> Size2D<T1> {
//        Size2D::new(self.width / scale, self.height / scale)
//    }
//}

// Convenient aliases for Size2D with typed units

pub type TypedSize2D<Unit, T> = Size2D<T, Unit>;

impl<Unit, T: Clone> Size2D<T, Unit> {
    pub fn typed(width: T, height: T) -> TypedSize2D<Unit, T> {
        Size2D::new(width, height)
    }

    /// Drop the units, preserving only the numeric value.
    pub fn to_untyped(&self) -> Size2D<T, Unit> {
        Size2D::new(self.width.clone(), self.height.clone())
    }

    /// Tag a unitless value with units.
    pub fn from_untyped(p: &Size2D<T, Unit>) -> TypedSize2D<Unit, T> {
        Size2D::new(p.width.clone(), p.height.clone())
    }
}

impl<Unit, T0: NumCast + Clone> Size2D<T0, Unit> {
    /// Cast from one numeric representation to another, preserving the units.
    pub fn cast<T1: NumCast + Clone>(&self) -> Option<Size2D<T1, Unit>> {
        match (NumCast::from(self.width.clone()), NumCast::from(self.height.clone())) {
            (Some(w), Some(h)) => Some(Size2D::new(w, h)),
            _ => None
        }
    }
}

// Convenience functions for common casts
impl<Unit, T: NumCast + Clone> Size2D<T, Unit> {
    pub fn as_f32(&self) -> Size2D<f32, Unit> {
        self.cast().unwrap()
    }

    pub fn as_uint(&self) -> Size2D<usize, Unit> {
        self.cast().unwrap()
    }
}
