use thiserror::Error;

use goblin;
use std::{error::Error, fmt::Display, fs::File, io, io::Read, path::Path};

use crate::{
    memory::{LiteralState, Memory},
    tab_viewer::TabSignals,
};

#[derive(Error, Debug)]
pub enum LoaderError {
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),
    #[error("Goblin error: {0}")]
    Parse(#[from] goblin::error::Error),
    #[error("Malformed file: {0}")]
    MalformedFile(String),
}

mod load_with_goblin;

pub fn load<P>(path: P, memory: &mut Memory, signals: &mut TabSignals) -> Result<(), LoaderError>
where
    P: AsRef<Path>,
{
    let mut file = File::open(path)?;
    let mut buf = Vec::new();
    file.read_to_end(&mut buf)?;
    load_with_goblin::load(&buf, memory, signals)?;

    signals.announce_new_file();
    Ok(())
}
