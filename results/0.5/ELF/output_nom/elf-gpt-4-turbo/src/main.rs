use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

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
    map(
        tuple((
            tag(b"\x7fELF"),
            take(12usize),
            le_u16,
            le_u16,
            le_u32,
            le_u64,
            le_u64,
            le_u64,
            le_u32,
            le_u16,
            le_u16,
            le_u16,
            le_u16,
            le_u16,
            le_u16,
        )),
        |(magic, rest_ident, e_type, e_machine, e_version, e_entry, e_phoff, e_shoff, e_flags, e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx)| {
            let mut e_ident = [0u8; 16];
            e_ident[..4].copy_from_slice(magic);
            e_ident[4..].copy_from_slice(rest_ident);
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
            }
        },
    )(input)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ELF file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_elf_header(&buffer) {
        Ok((_, header)) => {
            println!("{:#?}", header);
        }
        Err(e) => {
            eprintln!("Failed to parse ELF header: {:?}", e);
        }
    }

    Ok(())
}