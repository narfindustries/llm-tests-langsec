use nom::{
    bytes::complete::take,
    number::complete::{le_u16, le_u32, le_u64, le_u8},
    IResult,
};
use std::env;
use std::fs;
use std::error::Error;

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
    program_header_num: u16,
    section_header_entry_size: u16,
    section_header_num: u16,
    section_header_str_index: u16,
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, magic) = take(4usize)(input)?;
    let (input, class) = le_u8(input)?;
    let (input, data_encoding) = le_u8(input)?;
    let (input, version) = le_u8(input)?;
    let (input, os_abi) = le_u8(input)?;
    let (input, abi_version) = le_u8(input)?;
    let (input, padding) = take(7usize)(input)?;
    let (input, file_type) = le_u16(input)?;
    let (input, machine) = le_u16(input)?;
    let (input, elf_version) = le_u32(input)?;
    let (input, entry_point) = le_u64(input)?;
    let (input, program_header_offset) = le_u64(input)?;
    let (input, section_header_offset) = le_u64(input)?;
    let (input, flags) = le_u32(input)?;
    let (input, header_size) = le_u16(input)?;
    let (input, program_header_entry_size) = le_u16(input)?;
    let (input, program_header_num) = le_u16(input)?;
    let (input, section_header_entry_size) = le_u16(input)?;
    let (input, section_header_num) = le_u16(input)?;
    let (input, section_header_str_index) = le_u16(input)?;

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
        program_header_num,
        section_header_entry_size,
        section_header_num,
        section_header_str_index,
    }))
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <elf_file>", args[0]);
        std::process::exit(1);
    }

    let file_contents = fs::read(&args[1])?;
    match parse_elf_header(&file_contents) {
        Ok((_, header)) => {
            println!("ELF Header: {:?}", header);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse ELF header: {:?}", e);
            Err(Box::new(std::io::Error::new(std::io::ErrorKind::InvalidData, format!("{:?}", e))))
        }
    }
}