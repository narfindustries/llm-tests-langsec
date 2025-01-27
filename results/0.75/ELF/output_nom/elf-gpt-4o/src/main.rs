use nom::bytes::complete::{tag, take};
use nom::combinator::map_res;
use nom::error::ErrorKind;
use nom::multi::count;
use nom::number::complete::{be_u16, be_u32, be_u64, be_u8};
use nom::IResult;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::env;

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
    let (input, e_type) = be_u16(input)?;
    let (input, e_machine) = be_u16(input)?;
    let (input, e_version) = be_u32(input)?;
    let (input, e_entry) = be_u64(input)?;
    let (input, e_phoff) = be_u64(input)?;
    let (input, e_shoff) = be_u64(input)?;
    let (input, e_flags) = be_u32(input)?;
    let (input, e_ehsize) = be_u16(input)?;
    let (input, e_phentsize) = be_u16(input)?;
    let (input, e_phnum) = be_u16(input)?;
    let (input, e_shentsize) = be_u16(input)?;
    let (input, e_shnum) = be_u16(input)?;
    let (input, e_shstrndx) = be_u16(input)?;

    Ok((
        input,
        ElfHeader {
            e_ident: e_ident.try_into().expect("slice with incorrect length"),
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

fn read_file<P: AsRef<Path>>(path: P) -> std::io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <elf-file>", args[0]);
        return;
    }

    let filepath = &args[1];
    match read_file(filepath) {
        Ok(data) => match parse_elf_header(&data) {
            Ok((_, elf_header)) => println!("{:?}", elf_header),
            Err(e) => eprintln!("Failed to parse ELF header: {:?}", e),
        },
        Err(e) => eprintln!("Failed to read file: {:?}", e),
    }
}