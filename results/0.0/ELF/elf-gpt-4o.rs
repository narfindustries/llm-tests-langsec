// Cargo.toml
// [dependencies]
// nom = "7.1"
// clap = { version = "4.0", features = ["derive"] }

use clap::Parser;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::count,
    number::complete::{le_u16, le_u32, le_u64, le_u8},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

#[derive(Parser)]
struct Cli {
    /// The path to the ELF file to read
    path: PathBuf,
}

#[derive(Debug)]
struct ElfHeader {
    e_ident: [u8; 16],
    e_type: u16,
    e_machine: u16,
    e_version: u32,
    e_entry: u64,
    e_phoff: u64,
    e_shoff: u64,
    e_flags: u32,
    e_ehsize: u16,
    e_phentsize: u16,
    e_phnum: u16,
    e_shentsize: u16,
    e_shnum: u16,
    e_shstrndx: u16,
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, e_ident) = map(take(16usize), |bytes: &[u8]| {
        let mut arr = [0u8; 16];
        arr.copy_from_slice(bytes);
        arr
    })(input)?;

    let (input, (e_type, e_machine, e_version, e_entry, e_phoff, e_shoff, e_flags, e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx)) =
        tuple((
            le_u16, le_u16, le_u32, le_u64, le_u64, le_u64, le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u16,
        ))(input)?;

    Ok((
        input,
        ElfHeader {
            e_ident,
            e_type,
            e_machine,
            e_version,
            e_entry,
            e_phoff,
            e_shoff,
            e_flags,
            e_ehsize,
            e_phentsize,
            e_phnum,
            e_shentsize,
            e_shnum,
            e_shstrndx,
        },
    ))
}

fn main() {
    let args = Cli::parse();

    let mut file = File::open(args.path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_elf_header(&buffer) {
        Ok((_, elf_header)) => {
            println!("{:?}", elf_header);
        }
        Err(e) => {
            eprintln!("Failed to parse ELF header: {:?}", e);
        }
    }
}