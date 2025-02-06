use nom::{
    bytes::complete::{take, tag},
    number::complete::{le_u16, le_u32, le_u64},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct ElfHeader {
    e_ident: Vec<u8>,     // ELF Identification
    e_type: u16,          // Object file type
    e_machine: u16,       // Machine type
    e_version: u32,       // Object file version
    e_entry: u64,         // Entry point address
    e_phoff: u64,         // Program header offset
    e_shoff: u64,         // Section header offset
    e_flags: u32,         // Processor-specific flags
    e_ehsize: u16,        // ELF header size
    e_phentsize: u16,     // Size of program header entry
    e_phnum: u16,         // Number of program header entries
    e_shentsize: u16,     // Size of section header entry
    e_shnum: u16,         // Number of section header entries
    e_shstrndx: u16,      // Section header string table index
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, e_ident) = tag(b"\x7FELF")(input)?;
    let (input, e_ident_rest) = take(12usize)(input)?;
    let mut e_ident_full = Vec::from(e_ident);
    e_ident_full.extend_from_slice(e_ident_rest);

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

    Ok((
        input,
        ElfHeader {
            e_ident: e_ident_full,
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

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ELF file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_elf_header(&buffer) {
        Ok((_, elf_header)) => {
            println!("{:#?}", elf_header);
        }
        Err(e) => {
            eprintln!("Failed to parse ELF header: {:?}", e);
        }
    }

    Ok(())
}