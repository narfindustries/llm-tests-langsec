use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use std::env;
use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u64, u8},
    combinator::{map, map_res},
    sequence::{tuple},
    multi::count,
};

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

fn parse_ident(input: &[u8]) -> IResult<&[u8], [u8; 16]> {
    map(take(16usize), |bytes: &[u8]| {
        let mut ident = [0; 16];
        ident.copy_from_slice(bytes);
        ident
    })(input)
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    map(tuple((
        parse_ident,
        le_u16, le_u16, le_u32,
        map(le_u64, |v| if v == 0 { 0 } else { v }),
        le_u64, le_u64,
        le_u32, le_u16,
        le_u16, le_u16,
        le_u16, le_u16,
        le_u16
    )), |(
        e_ident, e_type, e_machine, e_version, e_entry,
        e_phoff, e_shoff, e_flags, e_ehsize,
        e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx
    )| ElfHeader {
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
    })(input)
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <elf-file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = read_file(filename).expect("Failed to read file");

    match parse_elf_header(&data) {
        Ok((_, elf_header)) => {
            println!("{:?}", elf_header);
        },
        Err(e) => {
            eprintln!("Failed to parse ELF header: {:?}", e);
        }
    }
}