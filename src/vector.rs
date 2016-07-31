// Copyright 2013 The Servo Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use super::UnknownUnit;
use length::Length;
use scale_factor::ScaleFactor;
use size::TypedSize2D;
use point::{TypedPoint2D, TypedPoint3D, TypedPoint4D};
use num::*;

use num_traits::{Float, NumCast};
use std::fmt;
use std::ops::{Add, Neg, Mul, Sub, Div};
use std::marker::PhantomData;
use std::cmp::{PartialEq, Eq};
use std::hash::{Hash, Hasher};

define_vector! {
    /// A 2d Vector tagged with a unit.
    #[derive(RustcDecodable, RustcEncodable)]
    pub struct TypedVector2D<T, U> {
        pub x: T,
        pub y: T,
    }
}

/// Default 2d vector type with no unit.
///
/// Vector2D provides the same methods as TypedVector2D.
pub type Vector2D<T> = TypedVector2D<T, UnknownUnit>;

impl<T: Copy, U> Copy for TypedVector2D<T, U> {}

impl<T: Copy, U> Clone for TypedVector2D<T, U> {
    fn clone(&self) -> TypedVector2D<T, U> { *self }
}

impl<T: PartialEq, U> PartialEq<TypedVector2D<T, U>> for TypedVector2D<T, U> {
    fn eq(&self, other: &TypedVector2D<T, U>) -> bool {
        self.x.eq(&other.x) && self.y.eq(&other.y)
    }
}

impl<T: Eq, U> Eq for TypedVector2D<T, U> {}

impl<T: Hash, U> Hash for TypedVector2D<T, U> {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.x.hash(h);
        self.y.hash(h);
    }
}

impl<T: Copy + Zero, U> TypedVector2D<T, U> {
    /// Constructor, setting all components to zero.
    #[inline]
    pub fn zero() -> TypedVector2D<T, U> {
        TypedVector2D::new(Zero::zero(), Zero::zero())
    }
}

impl<T: fmt::Debug, U> fmt::Debug for TypedVector2D<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:?},{:?})", self.x, self.y)
    }
}

impl<T: fmt::Display, U> fmt::Display for TypedVector2D<T, U> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "({},{})", self.x, self.y)
    }
}

impl<T: Copy, U> TypedVector2D<T, U> {
    /// Constructor taking scalar values directly.
    #[inline]
    pub fn new(x: T, y: T) -> TypedVector2D<T, U> {
        TypedVector2D { x: x, y: y, _unit: PhantomData }
    }

    /// Constructor taking properly typed Lengths instead of scalar values.
    #[inline]
    pub fn from_lengths(x: Length<T, U>, y: Length<T, U>) -> TypedVector2D<T, U> {
        TypedVector2D::new(x.0, y.0)
    }

    /// Returns self.x as a Length carrying the unit.
    #[inline]
    pub fn x_typed(&self) -> Length<T, U> { Length::new(self.x) }

    /// Returns self.y as a Length carrying the unit.
    #[inline]
    pub fn y_typed(&self) -> Length<T, U> { Length::new(self.y) }

    /// Drop the units, preserving only the numeric value.
    #[inline]
    pub fn to_untyped(&self) -> Vector2D<T> {
        TypedVector2D::new(self.x, self.y)
    }

    /// Tag a unitless value with units.
    #[inline]
    pub fn from_untyped(p: &Vector2D<T>) -> TypedVector2D<T, U> {
        TypedVector2D::new(p.x, p.y)
    }

    #[inline]
    pub fn to_array(&self) -> [T; 2] {
        [self.x, self.y]
    }

    /// Convert into a 2d point.
    ///
    /// This is equivalent to TypedPoint2D::origin + *self.
    #[inline]
    pub fn to_point(self) -> TypedPoint2D<T, U> {
        TypedPoint2D::new(self.x, self.y)
    }
}

impl<T, U> TypedVector2D<T, U>
where T: Copy + Mul<T, Output=T> + Add<T, Output=T> + Sub<T, Output=T> {
    /// Dot product.
    #[inline]
    pub fn dot(self, other: TypedVector2D<T, U>) -> T {
        self.x * other.x + self.y * other.y
    }

    /// Returns the norm of the cross product [self.x, self.y, 0] x [other.x, other.y, 0]..
    #[inline]
    pub fn cross(self, other: TypedVector2D<T, U>) -> T {
        self.x * other.y - self.y * other.x
    }
}

impl<T: Copy + Add<T, Output=T>, U> Add for TypedVector2D<T, U> {
    type Output = TypedVector2D<T, U>;
    fn add(self, other: TypedVector2D<T, U>) -> TypedVector2D<T, U> {
        TypedVector2D::new(self.x + other.x, self.y + other.y)
    }
}

impl<T: Copy + Add<T, Output=T>, U> Add<TypedSize2D<T, U>> for TypedVector2D<T, U> {
    type Output = TypedVector2D<T, U>;
    fn add(self, other: TypedSize2D<T, U>) -> TypedVector2D<T, U> {
        TypedVector2D::new(self.x + other.width, self.y + other.height)
    }
}

impl<T: Copy + Add<T, Output=T>, U> TypedVector2D<T, U> {
    pub fn add_size(&self, other: &TypedSize2D<T, U>) -> TypedVector2D<T, U> {
        TypedVector2D::new(self.x + other.width, self.y + other.height)
    }
}

impl<T: Copy + Sub<T, Output=T>, U> Sub for TypedVector2D<T, U> {
    type Output = TypedVector2D<T, U>;
    fn sub(self, other: TypedVector2D<T, U>) -> TypedVector2D<T, U> {
        TypedVector2D::new(self.x - other.x, self.y - other.y)
    }
}

impl <T: Copy + Neg<Output=T>, U> Neg for TypedVector2D<T, U> {
    type Output = TypedVector2D<T, U>;
    #[inline]
    fn neg(self) -> TypedVector2D<T, U> {
        TypedVector2D::new(-self.x, -self.y)
    }
}

impl<T: Float, U> TypedVector2D<T, U> {
    pub fn min(self, other: TypedVector2D<T, U>) -> TypedVector2D<T, U> {
         TypedVector2D::new(self.x.min(other.x), self.y.min(other.y))
    }

    pub fn max(self, other: TypedVector2D<T, U>) -> TypedVector2D<T, U> {
        TypedVector2D::new(self.x.max(other.x), self.y.max(other.y))
    }
}

impl<T: Copy + Mul<T, Output=T>, U> Mul<T> for TypedVector2D<T, U> {
    type Output = TypedVector2D<T, U>;
    #[inline]
    fn mul(self, scale: T) -> TypedVector2D<T, U> {
        TypedVector2D::new(self.x * scale, self.y * scale)
    }
}

impl<T: Copy + Div<T, Output=T>, U> Div<T> for TypedVector2D<T, U> {
    type Output = TypedVector2D<T, U>;
    #[inline]
    fn div(self, scale: T) -> TypedVector2D<T, U> {
        TypedVector2D::new(self.x / scale, self.y / scale)
    }
}

impl<T: Copy + Mul<T, Output=T>, U1, U2> Mul<ScaleFactor<T, U1, U2>> for TypedVector2D<T, U1> {
    type Output = TypedVector2D<T, U2>;
    #[inline]
    fn mul(self, scale: ScaleFactor<T, U1, U2>) -> TypedVector2D<T, U2> {
        TypedVector2D::new(self.x * scale.get(), self.y * scale.get())
    }
}

impl<T: Copy + Div<T, Output=T>, U1, U2> Div<ScaleFactor<T, U1, U2>> for TypedVector2D<T, U2> {
    type Output = TypedVector2D<T, U1>;
    #[inline]
    fn div(self, scale: ScaleFactor<T, U1, U2>) -> TypedVector2D<T, U1> {
        TypedVector2D::new(self.x / scale.get(), self.y / scale.get())
    }
}

impl<T: Round, U> TypedVector2D<T, U> {
    /// Rounds each component to the nearest integer value.
    ///
    /// This behavior is preserved for negative values (unlike the basic cast).
    /// For example { -0.1, -0.8 }.round() == { 0.0, -1.0 }
    pub fn round(&self) -> Self {
        TypedVector2D::new(self.x.round(), self.y.round())
    }
}

impl<T: Ceil, U> TypedVector2D<T, U> {
    /// Rounds each component to the smallest integer equal or greater than the orginal value.
    ///
    /// This behavior is preserved for negative values (unlike the basic cast).
    /// For example { -0.1, -0.8 }.ceil() == { 0.0, 0.0 }.
    pub fn ceil(&self) -> Self {
        TypedVector2D::new(self.x.ceil(), self.y.ceil())
    }
}

impl<T: Floor, U> TypedVector2D<T, U> {
    /// Rounds each component to the biggest integer equal or lower than the orginal value.
    ///
    /// This behavior is preserved for negative values (unlike the basic cast).
    /// For example { -0.1, -0.8 }.floor() == { -1.0, -1.0 }.
    pub fn floor(&self) -> Self {
        TypedVector2D::new(self.x.floor(), self.y.floor())
    }
}

impl<T: NumCast + Copy, U> TypedVector2D<T, U> {
    /// Cast from one numeric representation to another, preserving the units.
    ///
    /// When casting from floating point to integer coordinates, the decimals are truncated
    /// as one would expect from a simple cast, but this behavior does not always marke sense
    /// geometrically. Consider using round(), ceil or floor() before casting.
    pub fn cast<NewT: NumCast + Copy>(&self) -> Option<TypedVector2D<NewT, U>> {
        match (NumCast::from(self.x), NumCast::from(self.y)) {
            (Some(x), Some(y)) => Some(TypedVector2D::new(x, y)),
            _ => None
        }
    }

    // Convenience functions for common casts

    /// Cast into an f32 vector.
    pub fn to_f32(&self) -> TypedVector2D<f32, U> {
        self.cast().unwrap()
    }

    /// Cast into an usize vector, truncating decimals if any.
    ///
    /// When casting from floating point vectors, it is worth considering whether
    /// to round(), ceil() or floor() before the cast in order to obtain the desired
    /// conversion behavior.
    pub fn to_uint(&self) -> TypedVector2D<usize, U> {
        self.cast().unwrap()
    }

    /// Cast into an i32 vector, truncating decimals if any.
    ///
    /// When casting from floating point vectors, it is worth considering whether
    /// to round(), ceil() or floor() before the cast in order to obtain the desired
    /// conversion behavior.
    pub fn to_i32(&self) -> TypedVector2D<i32, U> {
        self.cast().unwrap()
    }

    /// Cast into an i64 vector, truncating decimals if any.
    ///
    /// When casting from floating point vectors, it is worth considering whether
    /// to round(), ceil() or floor() before the cast in order to obtain the desired
    /// conversion behavior.
    pub fn to_i64(&self) -> TypedVector2D<i64, U> {
        self.cast().unwrap()
    }
}

define_vector! {
    /// A 3d Vector tagged with a unit.
    #[derive(RustcDecodable, RustcEncodable)]
    pub struct TypedVector3D<T, U> {
        pub x: T,
        pub y: T,
        pub z: T,
    }
}

/// Default 3d vector type with no unit.
///
/// Vector3D provides the same methods as TypedVector3D.
pub type Vector3D<T> = TypedVector3D<T, UnknownUnit>;

impl<T: Hash, U> Hash for TypedVector3D<T, U> {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.x.hash(h);
        self.y.hash(h);
        self.z.hash(h);
    }
}

impl<T: Copy + Zero, U> TypedVector3D<T, U> {
    /// Constructor, setting all copmonents to zero.
    #[inline]
    pub fn zero() -> TypedVector3D<T, U> {
        TypedVector3D::new(Zero::zero(), Zero::zero(), Zero::zero())
    }

    #[inline]
    pub fn to_3d(&self) -> TypedVector3D<T, U> {
        TypedVector3D::new(self.x, self.y, Zero::zero())
    }
}

impl<T: Copy, U> Copy for TypedVector3D<T, U> {}

impl<T: Copy, U> Clone for TypedVector3D<T, U> {
    fn clone(&self) -> TypedVector3D<T, U> { *self }
}

impl<T: PartialEq, U> PartialEq<TypedVector3D<T, U>> for TypedVector3D<T, U> {
    fn eq(&self, other: &TypedVector3D<T, U>) -> bool {
        self.x.eq(&other.x) && self.y.eq(&other.y) && self.z.eq(&other.z)
    }
}

impl<T: Eq, U> Eq for TypedVector3D<T, U> {}

impl<T: fmt::Debug, U> fmt::Debug for TypedVector3D<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:?},{:?},{:?})", self.x, self.y, self.z)
    }
}

impl<T: fmt::Display, U> fmt::Display for TypedVector3D<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({},{},{})", self.x, self.y, self.z)
    }
}

impl<T: Copy, U> TypedVector3D<T, U> {
    /// Constructor taking scalar values directly.
    #[inline]
    pub fn new(x: T, y: T, z: T) -> TypedVector3D<T, U> {
        TypedVector3D { x: x, y: y, z: z, _unit: PhantomData }
    }

    /// Constructor taking properly typed Lengths instead of scalar values.
    #[inline]
    pub fn from_lengths(x: Length<T, U>, y: Length<T, U>, z: Length<T, U>) -> TypedVector3D<T, U> {
        TypedVector3D::new(x.0, y.0, z.0)
    }

    /// Returns self.x as a Length carrying the unit.
    #[inline]
    pub fn x_typed(&self) -> Length<T, U> { Length::new(self.x) }

    /// Returns self.y as a Length carrying the unit.
    #[inline]
    pub fn y_typed(&self) -> Length<T, U> { Length::new(self.y) }

    /// Returns self.z as a Length carrying the unit.
    #[inline]
    pub fn z_typed(&self) -> Length<T, U> { Length::new(self.z) }

    #[inline]
    pub fn to_array(&self) -> [T; 3] { [self.x, self.y, self.z] }

    /// Convert into a 3d point.
    ///
    /// This is equivalent to TypedPoint3D::origin + *self.
    #[inline]
    pub fn to_point(self) -> TypedPoint3D<T, U> {
        TypedPoint3D::new(self.x, self.y, self.z)
    }

    /// Drop the units, preserving only the numeric value.
    #[inline]
    pub fn to_untyped(&self) -> Vector3D<T> {
        TypedVector3D::new(self.x, self.y, self.z)
    }

    /// Tag a unitless value with units.
    #[inline]
    pub fn from_untyped(p: &Vector3D<T>) -> TypedVector3D<T, U> {
        TypedVector3D::new(p.x, p.y, p.z)
    }
}

impl<T: Mul<T, Output=T> +
        Add<T, Output=T> +
        Sub<T, Output=T> +
        Copy, U> TypedVector3D<T, U> {

    // Dot product.
    #[inline]
    pub fn dot(self, other: TypedVector3D<T, U>) -> T {
        self.x * other.x +
        self.y * other.y +
        self.z * other.z
    }

    // Cross product.
    #[inline]
    pub fn cross(self, other: TypedVector3D<T, U>) -> TypedVector3D<T, U> {
        TypedVector3D::new(self.y * other.z - self.z * other.y,
                          self.z * other.x - self.x * other.z,
                          self.x * other.y - self.y * other.x)
    }
}

impl<T: Copy + Add<T, Output=T>, U> Add for TypedVector3D<T, U> {
    type Output = TypedVector3D<T, U>;
    fn add(self, other: TypedVector3D<T, U>) -> TypedVector3D<T, U> {
        TypedVector3D::new(self.x + other.x,
                          self.y + other.y,
                          self.z + other.z)
    }
}

impl<T: Copy + Sub<T, Output=T>, U> Sub for TypedVector3D<T, U> {
    type Output = TypedVector3D<T, U>;
    fn sub(self, other: TypedVector3D<T, U>) -> TypedVector3D<T, U> {
        TypedVector3D::new(self.x - other.x,
                          self.y - other.y,
                          self.z - other.z)
    }
}

impl <T: Copy + Neg<Output=T>, U> Neg for TypedVector3D<T, U> {
    type Output = TypedVector3D<T, U>;
    #[inline]
    fn neg(self) -> TypedVector3D<T, U> {
        TypedVector3D::new(-self.x, -self.y, -self.z)
    }
}

impl<T: Float, U> TypedVector3D<T, U> {
    pub fn min(self, other: TypedVector3D<T, U>) -> TypedVector3D<T, U> {
         TypedVector3D::new(self.x.min(other.x),
                           self.y.min(other.y),
                           self.z.min(other.z))
    }

    pub fn max(self, other: TypedVector3D<T, U>) -> TypedVector3D<T, U> {
        TypedVector3D::new(self.x.max(other.x), self.y.max(other.y),
                     self.z.max(other.z))
    }
}

impl<T: Round, U> TypedVector3D<T, U> {
    /// Rounds each component to the nearest integer value.
    ///
    /// This behavior is preserved for negative values (unlike the basic cast).
    pub fn round(&self) -> Self {
        TypedVector3D::new(self.x.round(), self.y.round(), self.z.round())
    }
}

impl<T: Ceil, U> TypedVector3D<T, U> {
    /// Rounds each component to the smallest integer equal or greater than the orginal value.
    ///
    /// This behavior is preserved for negative values (unlike the basic cast).
    pub fn ceil(&self) -> Self {
        TypedVector3D::new(self.x.ceil(), self.y.ceil(), self.z.ceil())
    }
}

impl<T: Floor, U> TypedVector3D<T, U> {
    /// Rounds each component to the biggest integer equal or lower than the orginal value.
    ///
    /// This behavior is preserved for negative values (unlike the basic cast).
    pub fn floor(&self) -> Self {
        TypedVector3D::new(self.x.floor(), self.y.floor(), self.z.floor())
    }
}

impl<T: NumCast + Copy, U> TypedVector3D<T, U> {
    /// Cast from one numeric representation to another, preserving the units.
    ///
    /// When casting from floating point to integer coordinates, the decimals are truncated
    /// as one would expect from a simple cast, but this behavior does not always marke sense
    /// geometrically. Consider using round(), ceil or floor() before casting.
    pub fn cast<NewT: NumCast + Copy>(&self) -> Option<TypedVector3D<NewT, U>> {
        match (NumCast::from(self.x),
               NumCast::from(self.y),
               NumCast::from(self.z)) {
            (Some(x), Some(y), Some(z)) => Some(TypedVector3D::new(x, y, z)),
            _ => None
        }
    }

    // Convenience functions for common casts

    /// Cast into an f32 vector.
    pub fn to_f32(&self) -> TypedVector3D<f32, U> {
        self.cast().unwrap()
    }

    /// Cast into an usize vector, truncating decimals if any.
    ///
    /// When casting from floating point vectors, it is worth considering whether
    /// to round(), ceil() or floor() before the cast in order to obtain the desired
    /// conversion behavior.
    pub fn to_uint(&self) -> TypedVector3D<usize, U> {
        self.cast().unwrap()
    }

    /// Cast into an i32 vector, truncating decimals if any.
    ///
    /// When casting from floating point vectors, it is worth considering whether
    /// to round(), ceil() or floor() before the cast in order to obtain the desired
    /// conversion behavior.
    pub fn to_i32(&self) -> TypedVector3D<i32, U> {
        self.cast().unwrap()
    }

    /// Cast into an i64 vector, truncating decimals if any.
    ///
    /// When casting from floating point vectors, it is worth considering whether
    /// to round(), ceil() or floor() before the cast in order to obtain the desired
    /// conversion behavior.
    pub fn to_i64(&self) -> TypedVector3D<i64, U> {
        self.cast().unwrap()
    }
}

define_vector! {
    /// A 4d Vector tagged with a unit.
    #[derive(RustcDecodable, RustcEncodable)]
    pub struct TypedVector4D<T, U> {
        pub x: T,
        pub y: T,
        pub z: T,
        pub w: T,
    }
}

/// Default 4d vector with no unit.
///
/// Vector4D provides the same methods as TypedVector4D.
pub type Vector4D<T> = TypedVector4D<T, UnknownUnit>;

impl<T: Copy, U> Copy for TypedVector4D<T, U> {}

impl<T: Copy, U> Clone for TypedVector4D<T, U> {
    fn clone(&self) -> TypedVector4D<T, U> { *self }
}

impl<T: PartialEq, U> PartialEq<TypedVector4D<T, U>> for TypedVector4D<T, U> {
    fn eq(&self, other: &TypedVector4D<T, U>) -> bool {
        self.x.eq(&other.x) && self.y.eq(&other.y) && self.z.eq(&other.z) && self.w.eq(&other.w)
    }
}

impl<T: Eq, U> Eq for TypedVector4D<T, U> {}

impl<T: Hash, U> Hash for TypedVector4D<T, U> {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.x.hash(h);
        self.y.hash(h);
        self.z.hash(h);
        self.w.hash(h);
    }
}

impl<T: Copy + Zero, U> TypedVector4D<T, U> {
    /// Constructor, setting all copmonents to zero.
    #[inline]
    pub fn zero() -> TypedVector4D<T, U> {
        TypedVector4D::new(Zero::zero(), Zero::zero(), Zero::zero(), Zero::zero())
    }
}

impl<T: fmt::Debug, U> fmt::Debug for TypedVector4D<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:?},{:?},{:?},{:?})", self.x, self.y, self.z, self.w)
    }
}

impl<T: fmt::Display, U> fmt::Display for TypedVector4D<T, U> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "({},{},{},{})", self.x, self.y, self.z, self.w)
    }
}

impl<T: Copy, U> TypedVector4D<T, U> {
    /// Constructor taking scalar values directly.
    #[inline]
    pub fn new(x: T, y: T, z: T, w: T) -> TypedVector4D<T, U> {
        TypedVector4D { x: x, y: y, z: z, w: w, _unit: PhantomData }
    }

    /// Constructor taking properly typed Lengths instead of scalar values.
    #[inline]
    pub fn from_lengths(x: Length<T, U>,
                        y: Length<T, U>,
                        z: Length<T, U>,
                        w: Length<T, U>) -> TypedVector4D<T, U> {
        TypedVector4D::new(x.0, y.0, z.0, w.0)
    }

    /// Returns self.x as a Length carrying the unit.
    #[inline]
    pub fn x_typed(&self) -> Length<T, U> { Length::new(self.x) }

    /// Returns self.y as a Length carrying the unit.
    #[inline]
    pub fn y_typed(&self) -> Length<T, U> { Length::new(self.y) }

    /// Returns self.z as a Length carrying the unit.
    #[inline]
    pub fn z_typed(&self) -> Length<T, U> { Length::new(self.z) }

    /// Returns self.w as a Length carrying the unit.
    #[inline]
    pub fn w_typed(&self) -> Length<T, U> { Length::new(self.w) }

    /// Drop the units, preserving only the numeric value.
    #[inline]
    pub fn to_untyped(&self) -> Vector4D<T> {
        TypedVector4D::new(self.x, self.y, self.z, self.w)
    }

    /// Tag a unitless value with units.
    #[inline]
    pub fn from_untyped(p: &Vector4D<T>) -> TypedVector4D<T, U> {
        TypedVector4D::new(p.x, p.y, p.z, p.w)
    }

    #[inline]
    pub fn to_array(&self) -> [T; 4] {
        [self.x, self.y, self.z, self.w]
    }

    /// Convert into a 2d vector.
    #[inline]
    pub fn to_2d(self) -> TypedVector2D<T, U> {
        TypedVector2D::new(self.x, self.y)
    }

    /// Convert into a 3d vector.
    #[inline]
    pub fn to_3d(self) -> TypedVector3D<T, U> {
        TypedVector3D::new(self.x, self.y, self.z)
    }
}

impl<T: Copy + One, U> TypedVector4D<T, U> {
    /// Convert into a 4d point.
    #[inline]
    pub fn to_point(self) -> TypedPoint4D<T, U> {
        TypedPoint4D::new(self.x, self.y, self.z, One::one())
    }
}


impl<T: Copy + Add<T, Output=T>, U> Add for TypedVector4D<T, U> {
    type Output = TypedVector4D<T, U>;
    fn add(self, other: TypedVector4D<T, U>) -> TypedVector4D<T, U> {
        TypedVector4D::new(self.x + other.x,
                          self.y + other.y,
                          self.z + other.z,
                          self.w + other.w)
    }
}

impl<T: Copy + Sub<T, Output=T>, U> Sub for TypedVector4D<T, U> {
    type Output = TypedVector4D<T, U>;
    fn sub(self, other: TypedVector4D<T, U>) -> TypedVector4D<T, U> {
        TypedVector4D::new(self.x - other.x,
                          self.y - other.y,
                          self.z - other.z,
                          self.w - other.w)
    }
}

impl <T: Copy + Neg<Output=T>, U> Neg for TypedVector4D<T, U> {
    type Output = TypedVector4D<T, U>;
    #[inline]
    fn neg(self) -> TypedVector4D<T, U> {
        TypedVector4D::new(-self.x, -self.y, -self.z, -self.w)
    }
}

impl<T: Float, U> TypedVector4D<T, U> {
    pub fn min(self, other: TypedVector4D<T, U>) -> TypedVector4D<T, U> {
         TypedVector4D::new(self.x.min(other.x), self.y.min(other.y),
                           self.z.min(other.z), self.w.min(other.w))
    }

    pub fn max(self, other: TypedVector4D<T, U>) -> TypedVector4D<T, U> {
        TypedVector4D::new(self.x.max(other.x), self.y.max(other.y),
                          self.z.max(other.z), self.w.max(other.w))
    }
}

impl<T: Round, U> TypedVector4D<T, U> {
    /// Rounds each component to the nearest integer value.
    ///
    /// This behavior is preserved for negative values (unlike the basic cast).
    pub fn round(&self) -> Self {
        TypedVector4D::new(self.x.round(), self.y.round(), self.z.round(), self.w.round())
    }
}

impl<T: Ceil, U> TypedVector4D<T, U> {
    /// Rounds each component to the smallest integer equal or greater than the orginal value.
    ///
    /// This behavior is preserved for negative values (unlike the basic cast).
    pub fn ceil(&self) -> Self {
        TypedVector4D::new(self.x.ceil(), self.y.ceil(), self.z.ceil(), self.w.ceil())
    }
}

impl<T: Floor, U> TypedVector4D<T, U> {
    /// Rounds each component to the biggest integer equal or lower than the orginal value.
    ///
    /// This behavior is preserved for negative values (unlike the basic cast).
    pub fn floor(&self) -> Self {
        TypedVector4D::new(self.x.floor(), self.y.floor(), self.z.floor(), self.w.floor())
    }
}

impl<T: NumCast + Copy, U> TypedVector4D<T, U> {
    /// Cast from one numeric representation to another, preserving the units.
    ///
    /// When casting from floating point to integer coordinates, the decimals are truncated
    /// as one would expect from a simple cast, but this behavior does not always marke sense
    /// geometrically. Consider using round(), ceil or floor() before casting.
    pub fn cast<NewT: NumCast + Copy>(&self) -> Option<TypedVector4D<NewT, U>> {
        match (NumCast::from(self.x),
               NumCast::from(self.y),
               NumCast::from(self.z),
               NumCast::from(self.w)) {
            (Some(x), Some(y), Some(z), Some(w)) => Some(TypedVector4D::new(x, y, z, w)),
            _ => None
        }
    }

    // Convenience functions for common casts

    /// Cast into an f32 vector.
    pub fn to_f32(&self) -> TypedVector4D<f32, U> {
        self.cast().unwrap()
    }

    /// Cast into an usize vector, truncating decimals if any.
    ///
    /// When casting from floating point vectors, it is worth considering whether
    /// to round(), ceil() or floor() before the cast in order to obtain the desired
    /// conversion behavior.
    pub fn to_uint(&self) -> TypedVector4D<usize, U> {
        self.cast().unwrap()
    }

    /// Cast into an i32 vector, truncating decimals if any.
    ///
    /// When casting from floating point vectors, it is worth considering whether
    /// to round(), ceil() or floor() before the cast in order to obtain the desired
    /// conversion behavior.
    pub fn to_i32(&self) -> TypedVector4D<i32, U> {
        self.cast().unwrap()
    }

    /// Cast into an i64 vector, truncating decimals if any.
    ///
    /// When casting from floating point vectors, it is worth considering whether
    /// to round(), ceil() or floor() before the cast in order to obtain the desired
    /// conversion behavior.
    pub fn to_i64(&self) -> TypedVector4D<i64, U> {
        self.cast().unwrap()
    }
}

#[cfg(test)]
mod vector2d {
    use super::Vector2D;

    #[test]
    pub fn test_scalar_mul() {
        let p1: Vector2D<f32> = Vector2D::new(3.0, 5.0);

        let result = p1 * 5.0;

        assert_eq!(result, Vector2D::new(15.0, 25.0));
    }

    #[test]
    pub fn test_dot() {
        let p1: Vector2D<f32> = Vector2D::new(2.0, 7.0);
        let p2: Vector2D<f32> = Vector2D::new(13.0, 11.0);
        assert_eq!(p1.dot(p2), 103.0);
    }

    #[test]
    pub fn test_cross() {
        let p1: Vector2D<f32> = Vector2D::new(4.0, 7.0);
        let p2: Vector2D<f32> = Vector2D::new(13.0, 8.0);
        let r = p1.cross(p2);
        assert_eq!(r, -59.0);
    }

    #[test]
    pub fn test_min() {
        let p1 = Vector2D::new(1.0, 3.0);
        let p2 = Vector2D::new(2.0, 2.0);

        let result = p1.min(p2);

        assert_eq!(result, Vector2D::new(1.0, 2.0));
    }

    #[test]
    pub fn test_max() {
        let p1 = Vector2D::new(1.0, 3.0);
        let p2 = Vector2D::new(2.0, 2.0);

        let result = p1.max(p2);

        assert_eq!(result, Vector2D::new(2.0, 3.0));
    }
}

#[cfg(test)]
mod typedvector2d {
    use super::TypedVector2D;
    use scale_factor::ScaleFactor;

    pub enum Mm {}
    pub enum Cm {}

    pub type Vector2DMm<T> = TypedVector2D<T, Mm>;
    pub type Vector2DCm<T> = TypedVector2D<T, Cm>;

    #[test]
    pub fn test_add() {
        let p1 = Vector2DMm::new(1.0, 2.0);
        let p2 = Vector2DMm::new(3.0, 4.0);

        let result = p1 + p2;

        assert_eq!(result, Vector2DMm::new(4.0, 6.0));
    }

    #[test]
    pub fn test_scalar_mul() {
        let p1 = Vector2DMm::new(1.0, 2.0);
        let cm_per_mm: ScaleFactor<f32, Mm, Cm> = ScaleFactor::new(0.1);

        let result = p1 * cm_per_mm;

        assert_eq!(result, Vector2DCm::new(0.1, 0.2));
    }
}

#[cfg(test)]
mod vector3d {
    use super::Vector3D;

    #[test]
    pub fn test_dot() {
        let p1 = Vector3D::new(7.0, 21.0, 32.0);
        let p2 = Vector3D::new(43.0, 5.0, 16.0);
        assert_eq!(p1.dot(p2), 918.0);
    }

    #[test]
    pub fn test_cross() {
        let p1 = Vector3D::new(4.0, 7.0, 9.0);
        let p2 = Vector3D::new(13.0, 8.0, 3.0);
        let p3 = p1.cross(p2);
        assert_eq!(p3, Vector3D::new(-51.0, 105.0, -59.0));
    }

    #[test]
    pub fn test_min() {
        let p1 = Vector3D::new(1.0, 3.0, 5.0);
        let p2 = Vector3D::new(2.0, 2.0, -1.0);

        let result = p1.min(p2);

        assert_eq!(result, Vector3D::new(1.0, 2.0, -1.0));
    }

    #[test]
    pub fn test_max() {
        let p1 = Vector3D::new(1.0, 3.0, 5.0);
        let p2 = Vector3D::new(2.0, 2.0, -1.0);

        let result = p1.max(p2);

        assert_eq!(result, Vector3D::new(2.0, 3.0, 5.0));
    }
}

#[cfg(test)]
mod vector4d {
    use super::Vector4D;

    #[test]
    pub fn test_add() {
        let p1 = Vector4D::new(7.0, 21.0, 32.0, 1.0);
        let p2 = Vector4D::new(43.0, 5.0, 16.0, 2.0);

        let result = p1 + p2;

        assert_eq!(result, Vector4D::new(50.0, 26.0, 48.0, 3.0));
    }

    #[test]
    pub fn test_sub() {
        let p1 = Vector4D::new(7.0, 21.0, 32.0, 1.0);
        let p2 = Vector4D::new(43.0, 5.0, 16.0, 2.0);

        let result = p1 - p2;

        assert_eq!(result, Vector4D::new(-36.0, 16.0, 16.0, -1.0));
    }

    #[test]
    pub fn test_min() {
        let p1 = Vector4D::new(1.0, 3.0, 5.0, 7.0);
        let p2 = Vector4D::new(2.0, 2.0, -1.0, 10.0);

        let result = p1.min(p2);

        assert_eq!(result, Vector4D::new(1.0, 2.0, -1.0, 7.0));
    }

    #[test]
    pub fn test_max() {
        let p1 = Vector4D::new(1.0, 3.0, 5.0, 7.0);
        let p2 = Vector4D::new(2.0, 2.0, -1.0, 10.0);

        let result = p1.max(p2);

        assert_eq!(result, Vector4D::new(2.0, 3.0, 5.0, 10.0));
    }
}
