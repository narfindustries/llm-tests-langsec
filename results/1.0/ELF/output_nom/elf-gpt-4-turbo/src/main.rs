use nom::{
    bytes::complete::{take},
    number::complete::{le_u16, le_u32, le_u64},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct ELFHeader {
    e_ident: Vec<u8>,           // ELF identification
    e_type: u16,                // Object file type
    e_machine: u16,             // Machine type
    e_version: u32,             // Object file version
    e_entry: u64,               // Entry point address
    e_phoff: u64,               // Program header offset
    e_shoff: u64,               // Section header offset
    e_flags: u32,               // Processor-specific flags
    e_ehsize: u16,              // ELF header size
    e_phentsize: u16,           // Size of program header entry
    e_phnum: u16,               // Number of program header entries
    e_shentsize: u16,           // Size of section header entry
    e_shnum: u16,               // Number of section header entries
    e_shstrndx: u16,            // Section name string table index
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ELFHeader> {
    let (input, e_ident) = take(16usize)(input)?;
    let (input, e_type) = le_u16(input)?;
    let (input, e_machine) = le_u16(input)?;
    let (input, e_version) = le_u32(input)?;
    let (input, e_entry) = le_u64(input)?;
    let (input, e_phoff) = le_u64(input)?;
    let (input, e_shoff) = le_u64(input)?;
    let (input, e_flags) = le_u32(input)?;
    let (input, e_ehsize) = le_u16(input)?;
    let (input, e_phentsize) = le_u16(input)?;
    let (input, e_phnum) = le_u16(input)?;
    let (input, e_shentsize) = le_u16(input)?;
    let (input, e_shnum) = le_u16(input)?;
    let (input, e_shstrndx) = le_u16(input)?;

    Ok((input, ELFHeader {
        e_ident: e_ident.to_vec(),
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

fn main() -> Result<(), io::Error> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "Usage: elf_parser <file>"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_elf_header(&buffer) {
        Ok((_rest, header)) => {
            println!("{:?}", header);
        }
        Err(err) => {
            println!("Error parsing ELF header: {:?}", err);
        }
    }

    Ok(())
}