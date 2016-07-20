// Copyright 2013 The Servo Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use length::{ Length, Unit, Untyped };
use scale_factor::ScaleFactor;
use num::Zero;
use point::Point2D;
use size::Size2D;

use heapsize::HeapSizeOf;
use num_traits::NumCast;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::cmp::PartialOrd;
use std::fmt;
use std::ops::{Add, Sub, Mul, Div};
use std::marker::PhantomData;

#[derive(RustcDecodable, RustcEncodable)]
#[cfg_attr(feature = "plugins", derive(HeapSizeOf, Deserialize, Serialize))]
pub struct Rect<T, U = Untyped> {
    pub origin: Point2D<T, U>,
    pub size: Size2D<T, U>,
    _unit: PhantomData<U>,
}

impl<T: HeapSizeOf, U> HeapSizeOf for Rect<T, U> {
    fn heap_size_of_children(&self) -> usize {
        self.origin.heap_size_of_children() + self.size.heap_size_of_children()
    }
}

impl<T: Clone + Deserialize, U> Deserialize for Rect<T, U> {
    fn deserialize<D>(deserializer: &mut D) -> Result<Self, D::Error>
        where D: Deserializer
    {
        let (origin, size) = try!(Deserialize::deserialize(deserializer));
        Ok(Rect {
            origin: origin,
            size: size,
        })
    }
}

impl<T: Serialize, U> Serialize for Rect<T> {
    fn serialize<S>(&self, serializer: &mut S) -> Result<(), S::Error>
        where S: Serializer
    {
        (&self.origin, &self.size).serialize(serializer)
    }
}

impl<T: Copy, U> Copy for Rect<T, U> {}

impl<T: Clone, U> Clone for Rect<T, U> {
    fn clone(&self) -> Rect<T, U> {
        Rect::new(self.origin.clone(), self.size.clone())
    }
}

impl<T: PartialEq, U> PartialEq<Rect<T, U>> for Rect<T, U> {
    fn eq(&self, other: &Rect<T, U>) -> bool {
        self.origin.eq(&other.origin) && self.size.eq(&other.size)
    }
}

impl<T: Eq, U> Eq for Rect<T, U> {}

impl<T: fmt::Debug, U> fmt::Debug for Rect<T, U> {
   fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Rect({:?} at {:?})", self.size, self.origin)
    }
}

impl<T: fmt::Display, U> fmt::Display for Rect<T, U> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "Rect({} at {})", self.size.to_string(), self.origin.to_string())
    }
}

impl<T, U> Rect<T, U> {
    pub fn new(origin: Point2D<T, U>, size: Size2D<T, U>) -> Rect<T, U> {
        Rect {
            origin: origin,
            size: size,
            _unit: PhantomData,
        }
    }
}

impl<T: Copy + Clone + PartialOrd + Add<T, Output=T> + Sub<T, Output=T>, U> Rect<T, U> {
    #[inline]
    pub fn intersects(&self, other: &Rect<T, U>) -> bool {
        self.origin.x < other.origin.x + other.size.width &&
       other.origin.x <  self.origin.x + self.size.width &&
        self.origin.y < other.origin.y + other.size.height &&
       other.origin.y <  self.origin.y + self.size.height
    }

    #[inline]
    pub fn max_x(&self) -> T {
        self.origin.x + self.size.width
    }

    #[inline]
    pub fn min_x(&self) -> T {
        self.origin.x
    }

    #[inline]
    pub fn max_y(&self) -> T {
        self.origin.y + self.size.height
    }

    #[inline]
    pub fn min_y(&self) -> T {
        self.origin.y
    }

    #[inline]
    pub fn intersection(&self, other: &Rect<T, U>) -> Option<Rect<T, U>> {
        if !self.intersects(other) {
            return None;
        }

        let upper_left = Point2D::new(max(self.min_x(), other.min_x()),
                                      max(self.min_y(), other.min_y()));
        let lower_right_x = min(self.max_x(), other.max_x());
        let lower_right_y = min(self.max_y(), other.max_y());

        Some(Rect::new(upper_left.clone(), Size2D::new(lower_right_x - upper_left.x,
                                                       lower_right_y - upper_left.y)))
    }

    #[inline]
    pub fn translate(&self, other: &Point2D<T, U>) -> Rect<T, U> {
        Rect::new(
            Point2D::new(self.origin.x + other.x, self.origin.y + other.y),
            self.size
        )
    }

    #[inline]
    pub fn contains(&self, other: &Point2D<T, U>) -> bool {
        self.origin.x <= other.x && other.x < self.origin.x + self.size.width &&
        self.origin.y <= other.y && other.y < self.origin.y + self.size.height
    }

    #[inline]
    pub fn inflate(&self, width: T, height: T) -> Rect<T, U> {
        Rect::new(
            Point2D::new(self.origin.x - width, self.origin.y - height),
            Size2D::new(self.size.width + width + width, self.size.height + height + height),
        )
    }

    #[inline]
    pub fn top_right(&self) -> Point2D<T, U> {
        Point2D::new(self.max_x(), self.origin.y)
    }

    #[inline]
    pub fn bottom_left(&self) -> Point2D<T, U> {
        Point2D::new(self.origin.x.clone(), self.max_y())
    }

    #[inline]
    pub fn bottom_right(&self) -> Point2D<T, U> {
        Point2D::new(self.max_x(), self.max_y())
    }

    #[inline]
    pub fn translate_by_size(&self, size: &Size2D<T, U>) -> Rect<T, U> {
        self.translate(&Point2D::new(size.width, size.height))
    }
}

impl<T: Copy + Clone + PartialOrd + Add<T, Output=T> + Sub<T, Output=T> + Zero, U> Rect<T, U> {
    #[inline]
    pub fn union(&self, other: &Rect<T, U>) -> Rect<T, U> {
        if self.size == Zero::zero() {
            return other.clone();
        }
        if other.size == Zero::zero() {
            return self.clone();
        }

        let upper_left = Point2D::new(min(self.min_x(), other.min_x()),
                                      min(self.min_y(), other.min_y()));

        let lower_right_x = max(self.max_x(), other.max_x());
        let lower_right_y = max(self.max_y(), other.max_y());

        Rect::new(
            upper_left,
            Size2D::new(lower_right_x - upper_left.x, lower_right_y - upper_left.y)
        )
    }
}

impl<T, U> Rect<T, U> {
    #[inline]
    pub fn scale<Scale: Copy>(&self, x: Scale, y: Scale) -> Rect<T, U>
        where T: Copy + Clone + Mul<Scale, Output=T> {
        Rect::new(
            Point2D::new(self.origin.x * x, self.origin.y * y),
            Size2D::new(self.size.width * x, self.size.height * y)
        )
    }
}

impl<T: PartialEq + Zero, U> Rect<T, U> {
    pub fn zero() -> Rect<T, U> {
        Rect::new(
            Point2D::zero(),
            Size2D::zero(),
        )
    }

    pub fn is_empty(&self) -> bool {
        self.size.width == Zero::zero() || self.size.height == Zero::zero()
    }
}


pub fn min<T:Clone + PartialOrd>(x: T, y: T) -> T {
    if x <= y { x } else { y }
}

pub fn max<T:Clone + PartialOrd>(x: T, y: T) -> T {
    if x >= y { x } else { y }
}

impl<T: Copy + Mul<T, Output=T>, U> Mul<T> for Rect<T, U> {
    type Output = Rect<T, U>;
    #[inline]
    fn mul(self, scale: T) -> Rect<T, U> {
        Rect::new(self.origin * scale, self.size * scale)
    }
}

impl<T: Copy + Div<T, Output=T>, U> Div<T> for Rect<T, U> {
    type Output = Rect<T, U>;
    #[inline]
    fn div(self, scale: T) -> Rect<T, U> {
        Rect::new(self.origin / scale, self.size / scale)
    }
}

impl<T: Copy + Mul<T, Output=T>, U1, U2> Mul<ScaleFactor<T, U1, U2>> for Rect<T, U1> {
    type Output = Rect<T, U2>;
    #[inline]
    fn mul(self, scale: ScaleFactor<T, U1, U2>) -> Rect<T, U2> {
        Rect::new(self.origin * scale.clone(), self.size * scale.clone())
    }
}

impl<T: Copy + Div<T, Output=T>, U1, U2> Div<ScaleFactor<T, U1, U2>> for Rect<T, U2> {
    type Output = Rect<T, U1>;
    #[inline]
    fn div(self, scale: ScaleFactor<T, U1, U2>) -> Rect<T, U1> {
        Rect::new(self.origin / scale.clone(), self.size / scale.clone())
    }
}

// Convenient aliases for Rect with typed units
pub type TypedRect<Unit, T> = Rect<T, Unit>;

impl<Unit:Clone, T: Clone> Rect<T, Unit> {
    /// Drop the units, preserving only the numeric value.
    pub fn to_untyped(&self) -> Rect<T> {
        Rect::new(self.origin.to_untyped(), self.size.to_untyped())
    }

    /// Tag a unitless value with units.
    pub fn from_untyped(r: &Rect<T>) -> TypedRect<Unit, T> {
        Rect::new(Point2D::from_untyped(&r.origin), Size2D::from_untyped(&r.size))
    }
}

impl<Unit: Clone, T0: NumCast + Clone> Rect<T0, Unit> {
    /// Cast from one numeric representation to another, preserving the units.
    pub fn cast<T1: NumCast + Clone>(&self) -> Option<Rect<T1, Unit>> {
        match (self.origin.cast(), self.size.cast()) {
            (Some(origin), Some(size)) => Some(Rect::new(origin, size)),
            _ => None
        }
    }
}

// Convenience functions for common casts
impl<Unit: Clone, T: NumCast + Clone> Rect<T, Unit> {
    pub fn as_f32(&self) -> Rect<f32, Unit> {
        self.cast().unwrap()
    }

    pub fn as_uint(&self) -> Rect<usize, Unit> {
        self.cast().unwrap()
    }
}


#[cfg(test)]
mod tests {
    use point::Point2D;
    use size::Size2D;
    use super::*;

    #[test]
    fn test_min_max() {
        assert!(min(0u32, 1u32) == 0u32);
        assert!(min(-1.0f32, 0.0f32) == -1.0f32);

        assert!(max(0u32, 1u32) == 1u32);
        assert!(max(-1.0f32, 0.0f32) == 0.0f32);
    }

    #[test]
    fn test_translate() {
        let p = Rect::new(Point2D::new(0u32, 0u32), Size2D::new(50u32, 40u32));
        let pp = p.translate(&Point2D::new(10,15));

        assert!(pp.size.width == 50);
        assert!(pp.size.height == 40);
        assert!(pp.origin.x == 10);
        assert!(pp.origin.y == 15);


        let r = Rect::new(Point2D::new(-10, -5), Size2D::new(50, 40));
        let rr = r.translate(&Point2D::new(0,-10));

        assert!(rr.size.width == 50);
        assert!(rr.size.height == 40);
        assert!(rr.origin.x == -10);
        assert!(rr.origin.y == -15);
    }

    #[test]
    fn test_translate_by_size() {
        let p = Rect::new(Point2D::new(0u32, 0u32), Size2D::new(50u32, 40u32));
        let pp = p.translate_by_size(&Size2D::new(10,15));

        assert!(pp.size.width == 50);
        assert!(pp.size.height == 40);
        assert!(pp.origin.x == 10);
        assert!(pp.origin.y == 15);


        let r = Rect::new(Point2D::new(-10, -5), Size2D::new(50, 40));
        let rr = r.translate_by_size(&Size2D::new(0,-10));

        assert!(rr.size.width == 50);
        assert!(rr.size.height == 40);
        assert!(rr.origin.x == -10);
        assert!(rr.origin.y == -15);
    }

    #[test]
    fn test_union() {
        let p = Rect::new(Point2D::new(0, 0), Size2D::new(50, 40));
        let q = Rect::new(Point2D::new(20,20), Size2D::new(5, 5));
        let r = Rect::new(Point2D::new(-15, -30), Size2D::new(200, 15));
        let s = Rect::new(Point2D::new(20, -15), Size2D::new(250, 200));

        let pq = p.union(&q);
        assert!(pq.origin == Point2D::new(0, 0));
        assert!(pq.size == Size2D::new(50, 40));

        let pr = p.union(&r);
        assert!(pr.origin == Point2D::new(-15, -30));
        assert!(pr.size == Size2D::new(200, 70));

        let ps = p.union(&s);
        assert!(ps.origin == Point2D::new(0, -15));
        assert!(ps.size == Size2D::new(270, 200));

    }

    #[test]
    fn test_intersection() {
        let p = Rect::new(Point2D::new(0, 0), Size2D::new(10, 20));
        let q = Rect::new(Point2D::new(5, 15), Size2D::new(10, 10));
        let r = Rect::new(Point2D::new(-5, -5), Size2D::new(8, 8));

        let pq = p.intersection(&q);
        assert!(pq.is_some());
        let pq = pq.unwrap();
        assert!(pq.origin == Point2D::new(5, 15));
        assert!(pq.size == Size2D::new(5, 5));

        let pr = p.intersection(&r);
        assert!(pr.is_some());
        let pr = pr.unwrap();
        assert!(pr.origin == Point2D::new(0, 0));
        assert!(pr.size == Size2D::new(3, 3));

        let qr = q.intersection(&r);
        assert!(qr.is_none());
    }

    #[test]
    fn test_contains() {
        let r = Rect::new(Point2D::new(-20, 15), Size2D::new(100, 200));

        assert!(r.contains(&Point2D::new(0, 50)));
        assert!(r.contains(&Point2D::new(-10, 200)));

        // The `contains` method is inclusive of the top/left edges, but not the
        // bottom/right edges.
        assert!(r.contains(&Point2D::new(-20, 15)));
        assert!(!r.contains(&Point2D::new(80, 15)));
        assert!(!r.contains(&Point2D::new(80, 215)));
        assert!(!r.contains(&Point2D::new(-20, 215)));

        // Points beyond the top-left corner.
        assert!(!r.contains(&Point2D::new(-25, 15)));
        assert!(!r.contains(&Point2D::new(-15, 10)));

        // Points beyond the top-right corner.
        assert!(!r.contains(&Point2D::new(85, 20)));
        assert!(!r.contains(&Point2D::new(75, 10)));

        // Points beyond the bottom-right corner.
        assert!(!r.contains(&Point2D::new(85, 210)));
        assert!(!r.contains(&Point2D::new(75, 220)));

        // Points beyond the bottom-left corner.
        assert!(!r.contains(&Point2D::new(-25, 210)));
        assert!(!r.contains(&Point2D::new(-15, 220)));
    }

    #[test]
    fn test_scale() {
        let p = Rect::new(Point2D::new(0u32, 0u32), Size2D::new(50u32, 40u32));
        let pp = p.scale(10, 15);

        assert!(pp.size.width == 500);
        assert!(pp.size.height == 600);
        assert!(pp.origin.x == 0);
        assert!(pp.origin.y == 0);

        let r = Rect::new(Point2D::new(-10, -5), Size2D::new(50, 40));
        let rr = r.scale(1, 20);

        assert!(rr.size.width == 50);
        assert!(rr.size.height == 800);
        assert!(rr.origin.x == -10);
        assert!(rr.origin.y == -100);
    }

    #[test]
    fn test_inflate() {
        let p = Rect::new(Point2D::new(0, 0), Size2D::new(10, 10));
        let pp = p.inflate(10, 20);

        assert!(pp.size.width == 30);
        assert!(pp.size.height == 50);
        assert!(pp.origin.x == -10);
        assert!(pp.origin.y == -20);

        let r = Rect::new(Point2D::new(0, 0), Size2D::new(10, 20));
        let rr = r.inflate(-2, -5);

        assert!(rr.size.width == 6);
        assert!(rr.size.height == 10);
        assert!(rr.origin.x == 2);
        assert!(rr.origin.y == 5);
    }

    #[test]
    fn test_min_max_x_y() {
        let p = Rect::new(Point2D::new(0u32, 0u32), Size2D::new(50u32, 40u32));
        assert!(p.max_y() == 40);
        assert!(p.min_y() == 0);
        assert!(p.max_x() == 50);
        assert!(p.min_x() == 0);

        let r = Rect::new(Point2D::new(-10, -5), Size2D::new(50, 40));
        assert!(r.max_y() == 35);
        assert!(r.min_y() == -5);
        assert!(r.max_x() == 40);
        assert!(r.min_x() == -10);
    }

    #[test]
    fn test_is_empty() {
        assert!(Rect::new(Point2D::new(0u32, 0u32), Size2D::new(0u32, 0u32)).is_empty());
        assert!(Rect::new(Point2D::new(0u32, 0u32), Size2D::new(10u32, 0u32)).is_empty());
        assert!(Rect::new(Point2D::new(0u32, 0u32), Size2D::new(0u32, 10u32)).is_empty());
        assert!(!Rect::new(Point2D::new(0u32, 0u32), Size2D::new(1u32, 1u32)).is_empty());
        assert!(Rect::new(Point2D::new(10u32, 10u32), Size2D::new(0u32, 0u32)).is_empty());
        assert!(Rect::new(Point2D::new(10u32, 10u32), Size2D::new(10u32, 0u32)).is_empty());
        assert!(Rect::new(Point2D::new(10u32, 10u32), Size2D::new(0u32, 10u32)).is_empty());
        assert!(!Rect::new(Point2D::new(10u32, 10u32), Size2D::new(1u32, 1u32)).is_empty());
    }

}
