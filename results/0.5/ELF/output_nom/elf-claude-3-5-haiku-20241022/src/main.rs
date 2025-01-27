use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{le_u16, le_u32, le_u64, le_u8},
    sequence::{tuple, pair},
    IResult,
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

#[derive(Debug)]
struct ProgramHeader {
    segment_type: u32,
    flags: u32,
    offset: u64,
    virtual_address: u64,
    physical_address: u64,
    segment_file_size: u64,
    segment_memory_size: u64,
    alignment: u64,
}

#[derive(Debug)]
struct SectionHeader {
    name_index: u32,
    section_type: u32,
    flags: u64,
    virtual_address: u64,
    offset: u64,
    size: u64,
    link: u32,
    info: u32,
    address_alignment: u64,
    entry_size: u64,
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, (
        magic,
        class,
        data_encoding,
        version,
        os_abi,
        abi_version,
        padding,
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
        le_u16,
    ))(input)?;

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

fn parse_program_header(input: &[u8]) -> IResult<&[u8], ProgramHeader> {
    let (input, (
        segment_type,
        flags,
        offset,
        virtual_address,
        physical_address,
        segment_file_size,
        segment_memory_size,
        alignment,
    )) = tuple((
        le_u32,
        le_u32,
        le_u64,
        le_u64,
        le_u64,
        le_u64,
        le_u64,
        le_u64,
    ))(input)?;

    Ok((input, ProgramHeader {
        segment_type,
        flags,
        offset,
        virtual_address,
        physical_address,
        segment_file_size,
        segment_memory_size,
        alignment,
    }))
}

fn parse_section_header(input: &[u8]) -> IResult<&[u8], SectionHeader> {
    let (input, (
        name_index,
        section_type,
        flags,
        virtual_address,
        offset,
        size,
        link,
        info,
        address_alignment,
        entry_size,
    )) = tuple((
        le_u32,
        le_u32,
        le_u64,
        le_u64,
        le_u64,
        le_u64,
        le_u32,
        le_u32,
        le_u64,
        le_u64,
    ))(input)?;

    Ok((input, SectionHeader {
        name_index,
        section_type,
        flags,
        virtual_address,
        offset,
        size,
        link,
        info,
        address_alignment,
        entry_size,
    }))
}

fn parse_elf(input: &[u8]) -> IResult<&[u8], (ElfHeader, Vec<ProgramHeader>, Vec<SectionHeader>)> {
    let (input, header) = parse_elf_header(input)?;
    
    let (input, program_headers) = count(parse_program_header, header.program_header_count as usize)(input)?;
    
    let (input, section_headers) = count(parse_section_header, header.section_header_count as usize)(input)?;

    Ok((input, (header, program_headers, section_headers)))
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

    match parse_elf(&buffer) {
        Ok((_, (header, program_headers, section_headers))) => {
            println!("ELF Header: {:?}", header);
            println!("Program Headers: {:?}", program_headers);
            println!("Section Headers: {:?}", section_headers);
        },
        Err(e) => {
            eprintln!("Failed to parse ELF file: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}