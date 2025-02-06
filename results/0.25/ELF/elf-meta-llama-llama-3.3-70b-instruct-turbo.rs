use nom::{
    bytes::complete::{take},
    combinator::{map},
    multi::{count},
    number::complete::{be_u16, be_u32, be_u64, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
enum ElfClass {
    Elf32,
    Elf64,
}

#[derive(Debug)]
enum ElfData {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
enum OsAbi {
    SystemV,
    HpUx,
    NetBSD,
    Linux,
    Other(u8),
}

#[derive(Debug)]
enum ElfType {
    Rel,
    Exec,
    Dyn,
    Core,
}

#[derive(Debug)]
enum Machine {
    M32,
    Sparc,
    I386,
    Other(u16),
}

#[derive(Debug)]
struct ElfHeader {
    e_ident: [u8; 16],
    e_type: ElfType,
    e_machine: Machine,
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

fn parse_elf_class(input: &[u8]) -> IResult<&[u8], ElfClass> {
    map(be_u8, |x| match x {
        1 => ElfClass::Elf32,
        2 => ElfClass::Elf64,
        _ => panic!("Invalid ELF class"),
    })(input)
}

fn parse_elf_data(input: &[u8]) -> IResult<&[u8], ElfData> {
    map(be_u8, |x| match x {
        1 => ElfData::LittleEndian,
        2 => ElfData::BigEndian,
        _ => panic!("Invalid ELF data encoding"),
    })(input)
}

fn parse_os_abi(input: &[u8]) -> IResult<&[u8], OsAbi> {
    map(be_u8, |x| match x {
        0 => OsAbi::SystemV,
        1 => OsAbi::HpUx,
        2 => OsAbi::NetBSD,
        3 => OsAbi::Linux,
        x => OsAbi::Other(x),
    })(input)
}

fn parse_elf_type(input: &[u8]) -> IResult<&[u8], ElfType> {
    map(be_u16, |x| match x {
        1 => ElfType::Rel,
        2 => ElfType::Exec,
        3 => ElfType::Dyn,
        4 => ElfType::Core,
        _ => panic!("Invalid ELF type"),
    })(input)
}

fn parse_machine(input: &[u8]) -> IResult<&[u8], Machine> {
    map(be_u16, |x| match x {
        1 => Machine::M32,
        2 => Machine::Sparc,
        3 => Machine::I386,
        x => Machine::Other(x),
    })(input)
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, e_ident) = take(16usize)(input)?;
    let e_ident = {
        let mut arr = [0u8; 16];
        arr.copy_from_slice(e_ident);
        arr
    };
    let (input, e_type) = parse_elf_type(input)?;
    let (input, e_machine) = parse_machine(input)?;
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
        },
    ))
}

#[derive(Debug)]
enum ProgramHeaderType {
    Null,
    Load,
    Dynamic,
    Other(u32),
}

#[derive(Debug)]
struct ProgramHeader {
    p_type: ProgramHeaderType,
    p_offset: u64,
    p_vaddr: u64,
    p_paddr: u64,
    p_filesz: u64,
    p_memsz: u64,
    p_flags: u32,
    p_align: u64,
}

fn parse_program_header_type(input: &[u8]) -> IResult<&[u8], ProgramHeaderType> {
    map(be_u32, |x| match x {
        0 => ProgramHeaderType::Null,
        1 => ProgramHeaderType::Load,
        2 => ProgramHeaderType::Dynamic,
        x => ProgramHeaderType::Other(x),
    })(input)
}

fn parse_program_header(input: &[u8]) -> IResult<&[u8], ProgramHeader> {
    let (input, p_type) = parse_program_header_type(input)?;
    let (input, p_offset) = be_u64(input)?;
    let (input, p_vaddr) = be_u64(input)?;
    let (input, p_paddr) = be_u64(input)?;
    let (input, p_filesz) = be_u64(input)?;
    let (input, p_memsz) = be_u64(input)?;
    let (input, p_flags) = be_u32(input)?;
    let (input, p_align) = be_u64(input)?;
    Ok((
        input,
        ProgramHeader {
            p_type,
            p_offset,
            p_vaddr,
            p_paddr,
            p_filesz,
            p_memsz,
            p_flags,
            p_align,
        },
    ))
}

#[derive(Debug)]
enum SectionHeaderType {
    Null,
    Progbits,
    Symtab,
    Other(u32),
}

#[derive(Debug)]
struct SectionHeader {
    sh_name: u32,
    sh_type: SectionHeaderType,
    sh_flags: u64,
    sh_addr: u64,
    sh_offset: u64,
    sh_size: u64,
    sh_link: u32,
    sh_info: u32,
    sh_addralign: u64,
    sh_entsize: u64,
}

fn parse_section_header_type(input: &[u8]) -> IResult<&[u8], SectionHeaderType> {
    map(be_u32, |x| match x {
        0 => SectionHeaderType::Null,
        1 => SectionHeaderType::Progbits,
        2 => SectionHeaderType::Symtab,
        x => SectionHeaderType::Other(x),
    })(input)
}

fn parse_section_header(input: &[u8]) -> IResult<&[u8], SectionHeader> {
    let (input, sh_name) = be_u32(input)?;
    let (input, sh_type) = parse_section_header_type(input)?;
    let (input, sh_flags) = be_u64(input)?;
    let (input, sh_addr) = be_u64(input)?;
    let (input, sh_offset) = be_u64(input)?;
    let (input, sh_size) = be_u64(input)?;
    let (input, sh_link) = be_u32(input)?;
    let (input, sh_info) = be_u32(input)?;
    let (input, sh_addralign) = be_u64(input)?;
    let (input, sh_entsize) = be_u64(input)?;
    Ok((
        input,
        SectionHeader {
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
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <elf_file>", args[0]);
        return;
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).unwrap();
    let (_input, elf_header) = parse_elf_header(&buffer).unwrap();
    println!("ELF Header: {:?}", elf_header);
    let (_input, program_headers) = count(parse_program_header, elf_header.e_phnum as usize)(&buffer[elf_header.e_phoff as usize..]).unwrap();
    println!("Program Headers: {:?}", program_headers);
    let (_input, section_headers) = count(parse_section_header, elf_header.e_shnum as usize)(&buffer[elf_header.e_shoff as usize..]).unwrap();
    println!("Section Headers: {:?}", section_headers);
}