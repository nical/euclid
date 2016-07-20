// Copyright 2013 The Servo Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use num::{One, Zero};
use point::Point2D;
use rect::Rect;
use size::Size2D;
use std::ops::{Add, Mul, Sub};
use std::marker::PhantomData;

define_matrix! {
    pub struct Matrix2D<T, Src, Dst> {
        pub m11: T, pub m12: T,
        pub m21: T, pub m22: T,
        pub m31: T, pub m32: T,
        _units: PhantomData<(Src, Dst)>,
    }
}

impl<T:Add<T, Output=T> +
       Copy +
       Clone +
       Mul<T, Output=T> +
       One +
       PartialOrd +
       Sub<T, Output=T> +
       Zero, Src, Dst> Matrix2D<T, Src, Dst> {
    pub fn new(m11: T, m12: T, m21: T, m22: T, m31: T, m32: T) -> Matrix2D<T, Src, Dst> {
        Matrix2D {
            m11: m11, m12: m12,
            m21: m21, m22: m22,
            m31: m31, m32: m32,
            _units: PhantomData,
        }
    }

    pub fn mul(&self, m: &Matrix2D<T, Src, Dst>) -> Matrix2D<T, Src, Dst> {
        Matrix2D::new(m.m11*self.m11 + m.m12*self.m21,
                      m.m11*self.m12 + m.m12*self.m22,
                      m.m21*self.m11 + m.m22*self.m21,
                      m.m21*self.m12 + m.m22*self.m22,
                      m.m31*self.m11 + m.m32*self.m21 + self.m31,
                      m.m31*self.m12 + m.m32*self.m22 + self.m32)
    }

    pub fn translate(&self, x: T, y: T) -> Matrix2D<T, Src, Dst> {
         let (_0, _1): (T, T) = (Zero::zero(), One::one());
         let matrix = Matrix2D::new(_1, _0,
                                    _0, _1,
                                    x, y);
        self.mul(&matrix)
    }

    pub fn scale(&self, x: T, y: T) -> Matrix2D<T, Src, Dst> {
        Matrix2D::new(self.m11 * x, self.m12,
                      self.m21, self.m22 * y,
                      self.m31, self.m32)
    }

    pub fn identity() -> Matrix2D<T, Src, Dst> {
        let (_0, _1) = (Zero::zero(), One::one());
        Matrix2D::new(_1, _0,
                      _0, _1,
                      _0, _0)
    }

    pub fn to_array(&self) -> [T; 6] {
        [
            self.m11, self.m12,
            self.m21, self.m22,
            self.m31, self.m32
        ]
    }

    /// Returns the given point transformed by this matrix.
    #[inline]
    pub fn transform_point(&self, point: &Point2D<T, Src>) -> Point2D<T, Dst> {
        Point2D::new(point.x * self.m11 + point.y * self.m21 + self.m31,
                     point.x * self.m12 + point.y * self.m22 + self.m32)
    }

    /// Returns a rectangle that encompasses the result of transforming the given rectangle by this
    /// matrix.
    #[inline]
    pub fn transform_rect(&self, rect: &Rect<T, Src>) -> Rect<T, Dst> {
        let top_left = self.transform_point(&rect.origin);
        let top_right = self.transform_point(&rect.top_right());
        let bottom_left = self.transform_point(&rect.bottom_left());
        let bottom_right = self.transform_point(&rect.bottom_right());
        let (mut min_x, mut min_y) = (top_left.x, top_left.y);
        let (mut max_x, mut max_y) = (min_x, min_y);
        for point in &[top_right, bottom_left, bottom_right] {
            if point.x < min_x {
                min_x = point.x
            }
            if point.x > max_x {
                max_x = point.x
            }
            if point.y < min_y {
                min_y = point.y
            }
            if point.y > max_y {
                max_y = point.y
            }
        }
        Rect::new(Point2D::new(min_x, min_y),
                  Size2D::new(max_x - min_x, max_y - min_y))
    }
}
