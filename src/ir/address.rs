use std::ops::{Add, Range, Sub};

use nodit::{DiscreteFinite, PointType};
use petgraph::csr::IndexType;


#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Address(pub u64);

pub struct AddressIterator(Address, Address);

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

impl Iterator for AddressIterator {
    type Item = Address;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 != self.1 {
            self.0.0 += 1;
            Some(self.0)
        } else {
            None
        }
    }
}

impl Address {
    pub const NULL:Self = Self(0);

    fn test() {
    let test = 0..10;
    for i in test {

    }
    }
}

impl DiscreteFinite for Address {
    
    const MIN: Self = Address(0);
    
    const MAX: Self = Address(u64::MAX);
    
    fn up(self) -> Option<Self>
        where
            Self: Sized {
        if self != Self::MAX {
            Some(Address(self.0+1))
        } else {
            None
        }
    }
    
    fn down(self) -> Option<Self>
        where
            Self: Sized {
        if self != Self::MIN {
            Some(Address(self.0-1))
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