use crate::ir::address::Address;
use nodit::interval::ie;
use nodit::{Interval, NoditMap};

pub struct Section {
    pub name: String,
    pub virtual_size: usize,
    pub virtual_address: Address,
    pub size_of_raw_data: u32,
    pub pointer_to_raw_data: u32,
    pub pointer_to_relocations: u32,
    pub pointer_to_linenumbers: u32,
    pub number_of_relocations: u16,
    pub number_of_linenumbers: u16,
    pub characteristics: u32,
}

impl From<&goblin::pe::section_table::SectionTable> for Section {
    fn from(value: &goblin::pe::section_table::SectionTable) -> Self {
        let name = value.real_name.as_ref().cloned().unwrap_or_else(|| {
            String::from_utf8_lossy(&value.name)
                .trim_end_matches('\0')
                .to_string()
        });
        let mut section = Section::new(name, value.virtual_address, value.virtual_size as usize);
        section.characteristics = value.characteristics;
        section.number_of_linenumbers = value.number_of_linenumbers;
        section.number_of_relocations = value.number_of_relocations;
        section.size_of_raw_data = value.size_of_raw_data;
        section.pointer_to_raw_data = value.pointer_to_raw_data;
        section.pointer_to_relocations = value.pointer_to_relocations;
        section.pointer_to_linenumbers = value.pointer_to_linenumbers;
        section.virtual_address = value.virtual_address.into();
        section.virtual_size = value.virtual_size as usize;
        section
    }
}

impl Section {
    pub fn new<A: Into<Address>>(name: String, address: A, virtual_size: usize) -> Self {
        let virtual_address = address.into();
        Section {
            name: name,
            virtual_size,
            virtual_address,
            size_of_raw_data: u32::default(),
            pointer_to_raw_data: u32::default(),
            pointer_to_relocations: u32::default(),
            pointer_to_linenumbers: u32::default(),
            number_of_relocations: u16::default(),
            number_of_linenumbers: u16::default(),
            characteristics: u32::default(),
        }
    }
}

pub struct Navigation {
    /// Sections of the loaded binary file
    pub sections: Vec<Section>,
    /// Map of function interval to function start address
    pub function_span: NoditMap<Address, Interval<Address>, Address>,
}

impl Navigation {
    pub fn new() -> Self {
        Navigation {
            sections: Vec::new(),
            function_span: NoditMap::new(),
        }
    }
}
