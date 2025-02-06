use nom::{
    bytes::complete::{tag, take},
    multi::count,
    number::complete::{le_u16, le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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
    let (input, e_ident) = take(16usize)(input)?;
    let e_ident = e_ident.try_into().unwrap();

    let (input, (e_type, e_machine, e_version)) = tuple((le_u16, le_u16, le_u32))(input)?;
    let (input, (e_entry, e_phoff, e_shoff)) = tuple((le_u64, le_u64, le_u64))(input)?;
    let (input, (e_flags, e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx)) = 
        tuple((le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u16))(input)?;

    Ok((input, ElfHeader {
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
    }))
}

fn validate_elf_magic(header: &ElfHeader) -> bool {
    header.e_ident[0..4] == [0x7F, b'E', b'L', b'F']
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <elf_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_elf_header(&buffer) {
        Ok((_, header)) => {
            if validate_elf_magic(&header) {
                println!("Valid ELF Header: {:?}", header);
            } else {
                eprintln!("Invalid ELF magic number");
                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Failed to parse ELF header: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}