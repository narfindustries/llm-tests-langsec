use nom::{
    bytes::complete::{tag, take},
    multi::count,
    number::complete::{le_u16, le_u32, le_u64, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ElfHeader {
    ei_mag: [u8; 4],
    ei_class: u8,
    ei_data: u8,
    ei_version: u8,
    ei_osabi: u8,
    ei_abiversion: u8,
    ei_pad: [u8; 7],
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
    let (input, (
        ei_mag,
        ei_class,
        ei_data,
        ei_version,
        ei_osabi,
        ei_abiversion,
        ei_pad,
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
        e_shstrndx
    )) = tuple((
        take(4usize),
        le_u8,
        le_u8,
        le_u8,
        le_u8,
        le_u8,
        take(7usize),
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
        le_u16
    ))(input)?;

    Ok((input, ElfHeader {
        ei_mag: ei_mag.try_into().unwrap(),
        ei_class,
        ei_data,
        ei_version,
        ei_osabi,
        ei_abiversion,
        ei_pad: ei_pad.try_into().unwrap(),
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
    header.ei_mag == [0x7F, b'E', b'L', b'F']
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
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}