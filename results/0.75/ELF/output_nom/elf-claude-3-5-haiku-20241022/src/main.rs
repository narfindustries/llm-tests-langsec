use nom::{
    bytes::complete::{take, take_until},
    multi::{count, many0},
    number::complete::{le_u16, le_u32, le_u64, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct ElfHeader {
    e_ident: ElfIdent,
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

#[derive(Debug)]
struct ElfIdent {
    ei_mag: [u8; 4],
    ei_class: u8,
    ei_data: u8,
    ei_version: u8,
    ei_osabi: u8,
    ei_abiversion: u8,
    ei_pad: [u8; 7],
}

#[derive(Debug)]
struct ProgramHeader {
    p_type: u32,
    p_flags: u32,
    p_offset: u64,
    p_vaddr: u64,
    p_paddr: u64,
    p_filesz: u64,
    p_memsz: u64,
    p_align: u64,
}

#[derive(Debug)]
struct SectionHeader {
    sh_name: u32,
    sh_type: u32,
    sh_flags: u64,
    sh_addr: u64,
    sh_offset: u64,
    sh_size: u64,
    sh_link: u32,
    sh_info: u32,
    sh_addralign: u64,
    sh_entsize: u64,
}

fn parse_elf_ident(input: &[u8]) -> IResult<&[u8], ElfIdent> {
    let (input, (ei_mag, ei_class, ei_data, ei_version, ei_osabi, ei_abiversion, ei_pad)) = tuple((
        take(4usize),
        le_u8,
        le_u8,
        le_u8,
        le_u8,
        le_u8,
        take(7usize),
    ))(input)?;

    Ok((input, ElfIdent {
        ei_mag: ei_mag.try_into().unwrap(),
        ei_class,
        ei_data,
        ei_version,
        ei_osabi,
        ei_abiversion,
        ei_pad: ei_pad.try_into().unwrap(),
    }))
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, e_ident) = parse_elf_ident(input)?;
    let (input, (e_type, e_machine, e_version, e_entry, e_phoff, e_shoff, e_flags, e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx)) = tuple((
        le_u16, le_u16, le_u32, le_u64, le_u64, le_u64, le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u16,
    ))(input)?;

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

fn parse_program_headers(input: &[u8], count: usize) -> IResult<&[u8], Vec<ProgramHeader>> {
    count(parse_program_header, count)(input)
}

fn parse_program_header(input: &[u8]) -> IResult<&[u8], ProgramHeader> {
    let (input, (p_type, p_flags, p_offset, p_vaddr, p_paddr, p_filesz, p_memsz, p_align)) = tuple((
        le_u32, le_u32, le_u64, le_u64, le_u64, le_u64, le_u64, le_u64,
    ))(input)?;

    Ok((input, ProgramHeader {
        p_type,
        p_flags,
        p_offset,
        p_vaddr,
        p_paddr,
        p_filesz,
        p_memsz,
        p_align,
    }))
}

fn parse_section_headers(input: &[u8], count: usize) -> IResult<&[u8], Vec<SectionHeader>> {
    count(parse_section_header, count)(input)
}

fn parse_section_header(input: &[u8]) -> IResult<&[u8], SectionHeader> {
    let (input, (sh_name, sh_type, sh_flags, sh_addr, sh_offset, sh_size, sh_link, sh_info, sh_addralign, sh_entsize)) = tuple((
        le_u32, le_u32, le_u64, le_u64, le_u64, le_u64, le_u32, le_u32, le_u64, le_u64,
    ))(input)?;

    Ok((input, SectionHeader {
        sh_name,
        sh_type,
        sh_flags,
        sh_addr,
        sh_offset,
        sh_size,
        sh_link,
        sh_info,
        sh_addralign,
        sh_entsize,
    }))
}

fn parse_elf(input: &[u8]) -> IResult<&[u8], (ElfHeader, Vec<ProgramHeader>, Vec<SectionHeader>)> {
    let (input, elf_header) = parse_elf_header(input)?;
    let (input, program_headers) = parse_program_headers(input, elf_header.e_phnum as usize)?;
    let (input, section_headers) = parse_section_headers(input, elf_header.e_shnum as usize)?;

    Ok((input, (elf_header, program_headers, section_headers)))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <elf_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_elf(&buffer) {
        Ok((_, (elf_header, program_headers, section_headers))) => {
            println!("ELF Header: {:?}", elf_header);
            println!("Program Headers: {:?}", program_headers);
            println!("Section Headers: {:?}", section_headers);
        }
        Err(err) => {
            eprintln!("Parsing error: {:?}", err);
            std::process::exit(1);
        }
    }

    Ok(())
}