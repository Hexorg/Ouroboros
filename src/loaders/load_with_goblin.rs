use crate::memory::{navigation::Section, Memory};
use crate::memory::{LiteralKind, LiteralState};
use crate::tab_viewer::TabSignals;
use goblin::{error, Object};
use std::borrow::Cow;
use std::env;
use std::fs;
use std::path::Path;

pub fn load<'s>(
    bytes: &'s [u8],
    memory: &mut Memory,
    signals: &mut TabSignals,
) -> Result<(), super::LoaderError> {
    let o = Object::parse(&bytes)?;
    match o {
        Object::Elf(elf) => {
            for entry in elf.dynsyms.iter() {
                let name = elf.dynstrtab.get_at(entry.st_name);
                let sh = elf.dynstrtab.get_at(entry.st_shndx);
                println!(
                    "{} ({}): {:?}",
                    name.unwrap_or("No name"),
                    sh.unwrap_or("NOSH"),
                    entry
                );
            }
        }
        Object::PE(pe) => {
            // Load all sections into memory
            // TODO: Figure out header image size
            memory
                .navigation
                .sections
                .push(Section::new("Headers".into(), pe.image_base, 1024));
            for section in pe.sections {
                if let Some(data) = section.data(bytes)? {
                    if memory.navigation.sections.len() == 1 {
                        let bytes = &bytes[0..section.pointer_to_raw_data as usize];
                        let literal = LiteralState::from_bytes(pe.image_base, bytes.to_vec());
                        memory.navigation.sections[0].virtual_size =
                            section.pointer_to_raw_data as usize;
                        memory
                            .literal
                            .insert_strict(literal.get_interval(), literal)
                            .unwrap();
                    }
                    let mut section = Section::from(&section);
                    // if section.virtual_address.0 as u32 == section.pointer_to_raw_data {
                    section.virtual_address.0 += pe.image_base;
                    // }
                    let literal =
                        LiteralState::from_bytes(section.virtual_address, data.into_owned());
                    memory.navigation.sections.push(section);
                    memory
                        .literal
                        .insert_strict(literal.get_interval(), literal)
                        .unwrap();
                } else {
                    return Err(super::LoaderError::MalformedFile(
                        "Section offsets are outside of file content size.".into(),
                    ));
                }
            }
            // pe.image_base
            // for entry in pe.imports {
            //     println!("{entry:?}")
            // }
            // if let Some(data) = pe.import_data {
            //     for entry in data.import_data {
            //         println!("{entry:?}")
            //     }
            // }
            println!("Entry point: 0x{:x}", pe.entry as u64 + pe.image_base);
            signals.define_function(pe.entry as u64 + pe.image_base);
        }
        Object::TE(te) => todo!(),
        Object::COFF(coff) => todo!(),
        Object::Mach(mach) => todo!(),
        Object::Archive(archive) => todo!(),
        Object::Unknown(_) => todo!(),
        _ => todo!(),
    }
    Ok(())
}
