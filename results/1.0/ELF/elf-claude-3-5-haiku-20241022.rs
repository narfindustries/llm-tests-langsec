use nom::{
    bytes::complete::{tag, take},
    multi::count,
    number::complete::{le_u16, le_u32, le_u64, le_u8},
    sequence::tuple,
    IResult, Parser,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ElfHeader {
    magic: [u8; 4],
    class: u8,
    data_encoding: u8,
    version: u8,
    os_abi: u8,
    abi_version: u8,
    padding: [u8; 7],
    file_type: u16,
    machine: u16,
    elf_version: u32,
    entry_point: u64,
    program_header_offset: u64,
    section_header_offset: u64,
    flags: u32,
    header_size: u16,
    program_header_entry_size: u16,
    program_header_count: u16,
    section_header_entry_size: u16,
    section_header_count: u16,
    section_header_string_index: u16,
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, magic) = take(4usize).parse(input)?;
    let (input, class) = le_u8.parse(input)?;
    let (input, data_encoding) = le_u8.parse(input)?;
    let (input, version) = le_u8.parse(input)?;
    let (input, os_abi) = le_u8.parse(input)?;
    let (input, abi_version) = le_u8.parse(input)?;
    let (input, padding) = take(7usize).parse(input)?;
    let (input, file_type) = le_u16.parse(input)?;
    let (input, machine) = le_u16.parse(input)?;
    let (input, elf_version) = le_u32.parse(input)?;
    let (input, entry_point) = le_u64.parse(input)?;
    let (input, program_header_offset) = le_u64.parse(input)?;
    let (input, section_header_offset) = le_u64.parse(input)?;
    let (input, flags) = le_u32.parse(input)?;
    let (input, header_size) = le_u16.parse(input)?;
    let (input, program_header_entry_size) = le_u16.parse(input)?;
    let (input, program_header_count) = le_u16.parse(input)?;
    let (input, section_header_entry_size) = le_u16.parse(input)?;
    let (input, section_header_count) = le_u16.parse(input)?;
    let (input, section_header_string_index) = le_u16.parse(input)?;

    Ok((input, ElfHeader {
        magic: magic.try_into().unwrap(),
        class,
        data_encoding,
        version,
        os_abi,
        abi_version,
        padding: padding.try_into().unwrap(),
        file_type,
        machine,
        elf_version,
        entry_point,
        program_header_offset,
        section_header_offset,
        flags,
        header_size,
        program_header_entry_size,
        program_header_count,
        section_header_entry_size,
        section_header_count,
        section_header_string_index,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <elf_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_elf_header(&buffer) {
        Ok((_, header)) => {
            println!("ELF Header parsed successfully: {:?}", header);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse ELF header: {:?}", e);
            Err(e.into())
        }
    }
}