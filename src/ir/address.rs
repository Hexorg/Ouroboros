use std::ops::{Add, Sub};

use nodit::DiscreteFinite;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Address(pub u64);

impl std::fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("0x{:x}", self.0))
    }
}

impl std::fmt::Debug for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("0x{:x}", self.0))
    }
}

impl Into<Address> for u64 {
    fn into(self) -> Address {
        Address(self)
    }
}

impl Into<Address> for u32 {
    fn into(self) -> Address {
        Address(self as u64)
    }
}

impl Into<Address> for i32 {
    fn into(self) -> Address {
        Address(self as u64)
    }
}

impl Into<Address> for usize {
    fn into(self) -> Address {
        Address(self as u64)
    }
}

impl Default for Address {
    fn default() -> Self {
        Address::NULL
    }
}

impl Address {
    pub const NULL: Self = Self(0);
}

impl DiscreteFinite for Address {
    const MIN: Self = Address(0);

    const MAX: Self = Address(u64::MAX);

    fn up(self) -> Option<Self>
    where
        Self: Sized,
    {
        if self != Self::MAX {
            Some(Address(self.0 + 1))
        } else {
            None
        }
    }

    fn down(self) -> Option<Self>
    where
        Self: Sized,
    {
        if self != Self::MIN {
            Some(Address(self.0 - 1))
        } else {
            None
        }
    }
}

impl Add for Address {
    type Output = Address;

    fn add(self, rhs: Self) -> Self::Output {
        Address(self.0 + rhs.0)
    }
}

impl Sub for Address {
    type Output = Address;

    fn sub(self, rhs: Self) -> Self::Output {
        Address(self.0 - rhs.0)
    }
}
