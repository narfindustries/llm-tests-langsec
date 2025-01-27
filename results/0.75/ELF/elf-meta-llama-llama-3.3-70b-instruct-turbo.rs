use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{length_data, take_till},
    number::complete::{be_u16, be_u32, be_u64},
    sequence::{tuple, preceded},
    IResult,
};
use std::{
    env, fs,
    io::{BufReader, Read},
    str,
};

#[derive(Debug)]
enum ElfClass {
    Elf32,
    Elf64,
}

#[derive(Debug)]
enum DataEncoding {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
enum OsAbi {
    SystemV,
    HpUx,
    NetBsd,
    Linux,
    Solaris,
    Aix,
    Irix,
    FreeBSD,
    Tru64,
    NovellModesto,
    OpenBsd,
    OpenUnix,
    Solaris8,
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
    NoMachine,
    M32,
    Sparc,
    I386,
    M68K,
    M88K,
    I860,
    Mips,
    S370,
    MipsRs3Le,
    Parisc,
}

#[derive(Debug)]
struct ElfHeader {
    magic: [u8; 4],
    class: ElfClass,
    data_encoding: DataEncoding,
    os_abi: OsAbi,
    abi_version: u8,
    pad: [u8; 7],
    elf_type: ElfType,
    machine: Machine,
    version: u32,
    entry_point: u64,
    ph_off: u64,
    sh_off: u64,
    flags: u32,
    eh_size: u16,
    ph_entsize: u16,
    ph_num: u16,
    sh_entsize: u16,
    sh_num: u16,
    sh_strndx: u16,
}

#[derive(Debug)]
enum SectionHeaderType {
    Null,
    ProgBits,
    SymTab,
    StrTab,
    Rela,
    Hash,
    Dynamic,
    Note,
    NoBits,
    Rel,
    ShLib,
    DynSym,
}

#[derive(Debug)]
struct SectionHeader {
    name: u32,
    type_: SectionHeaderType,
    flags: u64,
    addr: u64,
    offset: u64,
    size: u64,
    link: u32,
    info: u32,
    addralign: u64,
    entsize: u64,
}

fn parse_elf_class(input: &[u8]) -> IResult<&[u8], ElfClass> {
    map(be_u8, |x| match x {
        1 => ElfClass::Elf32,
        2 => ElfClass::Elf64,
        _ => panic!("Invalid ELF class"),
    })(input)
}

fn parse_data_encoding(input: &[u8]) -> IResult<&[u8], DataEncoding> {
    map(be_u8, |x| match x {
        1 => DataEncoding::LittleEndian,
        2 => DataEncoding::BigEndian,
        _ => panic!("Invalid data encoding"),
    })(input)
}

fn parse_os_abi(input: &[u8]) -> IResult<&[u8], OsAbi> {
    map(be_u8, |x| match x {
        0 => OsAbi::SystemV,
        1 => OsAbi::HpUx,
        2 => OsAbi::NetBsd,
        3 => OsAbi::Linux,
        4 => OsAbi::Solaris,
        5 => OsAbi::Aix,
        6 => OsAbi::Irix,
        7 => OsAbi::FreeBSD,
        8 => OsAbi::Tru64,
        9 => OsAbi::NovellModesto,
        10 => OsAbi::OpenBsd,
        11 => OsAbi::OpenUnix,
        12 => OsAbi::Solaris8,
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
        0 => Machine::NoMachine,
        1 => Machine::M32,
        2 => Machine::Sparc,
        3 => Machine::I386,
        4 => Machine::M68K,
        5 => Machine::M88K,
        6 => Machine::I860,
        7 => Machine::Mips,
        8 => Machine::S370,
        9 => Machine::MipsRs3Le,
        10 => Machine::Parisc,
        _ => panic!("Invalid machine"),
    })(input)
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, magic) = take(4u8)(input)?;
    let (input, class) = parse_elf_class(input)?;
    let (input, data_encoding) = parse_data_encoding(input)?;
    let (input, os_abi) = parse_os_abi(input)?;
    let (input, abi_version) = be_u8(input)?;
    let (input, pad) = take(7u8)(input)?;
    let (input, elf_type) = parse_elf_type(input)?;
    let (input, machine) = parse_machine(input)?;
    let (input, version) = be_u32(input)?;
    let (input, entry_point) = be_u64(input)?;
    let (input, ph_off) = be_u64(input)?;
    let (input, sh_off) = be_u64(input)?;
    let (input, flags) = be_u32(input)?;
    let (input, eh_size) = be_u16(input)?;
    let (input, ph_entsize) = be_u16(input)?;
    let (input, ph_num) = be_u16(input)?;
    let (input, sh_entsize) = be_u16(input)?;
    let (input, sh_num) = be_u16(input)?;
    let (input, sh_strndx) = be_u16(input)?;
    Ok((
        input,
        ElfHeader {
            magic,
            class,
            data_encoding,
            os_abi,
            abi_version,
            pad: *pad,
            elf_type,
            machine,
            version,
            entry_point,
            ph_off,
            sh_off,
            flags,
            eh_size,
            ph_entsize,
            ph_num,
            sh_entsize,
            sh_num,
            sh_strndx,
        },
    ))
}

fn parse_section_header(input: &[u8]) -> IResult<&[u8], SectionHeader> {
    let (input, name) = be_u32(input)?;
    let (input, type_) = map(be_u32, |x| match x {
        0 => SectionHeaderType::Null,
        1 => SectionHeaderType::ProgBits,
        2 => SectionHeaderType::SymTab,
        3 => SectionHeaderType::StrTab,
        4 => SectionHeaderType::Rela,
        5 => SectionHeaderType::Hash,
        6 => SectionHeaderType::Dynamic,
        7 => SectionHeaderType::Note,
        8 => SectionHeaderType::NoBits,
        9 => SectionHeaderType::Rel,
        10 => SectionHeaderType::ShLib,
        11 => SectionHeaderType::DynSym,
        _ => panic!("Invalid section header type"),
    })(input)?;
    let (input, flags) = be_u64(input)?;
    let (input, addr) = be_u64(input)?;
    let (input, offset) = be_u64(input)?;
    let (input, size) = be_u64(input)?;
    let (input, link) = be_u32(input)?;
    let (input, info) = be_u32(input)?;
    let (input, addralign) = be_u64(input)?;
    let (input, entsize) = be_u64(input)?;
    Ok((
        input,
        SectionHeader {
            name,
            type_,
            flags,
            addr,
            offset,
            size,
            link,
            info,
            addralign,
            entsize,
        },
    ))
}

fn parse_elf(input: &[u8]) -> IResult<&[u8], (ElfHeader, Vec<SectionHeader>)> {
    let (input, header) = parse_elf_header(input)?;
    let num_sections = header.sh_num as usize;
    let (input, sections) = length_data(take(num_sections * 64u8))(input)?;
    let mut section_headers = Vec::with_capacity(num_sections);
    let mut remaining = sections;
    for _ in 0..num_sections {
        let (input, section) = parse_section_header(remaining)?;
        section_headers.push(section);
        remaining = input;
    }
    Ok((input, (header, section_headers)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <elf_file>", args[0]);
        return;
    }
    let file = fs::File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    match parse_elf(&input) {
        Ok((remaining, (header, sections))) => {
            println!("ELF Header:");
            println!("  Magic: {:?}", header.magic);
            println!("  Class: {:?}", header.class);
            println!("  Data Encoding: {:?}", header.data_encoding);
            println!("  OS/ABI: {:?}", header.os_abi);
            println!("  ABI Version: {}", header.abi_version);
            println!("  Type: {:?}", header.elf_type);
            println!("  Machine: {:?}", header.machine);
            println!("  Version: {}", header.version);
            println!("  Entry Point: {}", header.entry_point);
            println!("  PH Off: {}", header.ph_off);
            println!("  SH Off: {}", header.sh_off);
            println!("  Flags: {}", header.flags);
            println!("  EH Size: {}", header.eh_size);
            println!("  PH Entsize: {}", header.ph_entsize);
            println!("  PH Num: {}", header.ph_num);
            println!("  SH Entsize: {}", header.sh_entsize);
            println!("  SH Num: {}", header.sh_num);
            println!("  SH Strndx: {}", header.sh_strndx);
            println!("Section Headers:");
            for (i, section) in sections.iter().enumerate() {
                println!("  Section {}: {:?}", i, section);
            }
        }
        Err(err) => {
            eprintln!("Error parsing ELF file: {:?}", err);
        }
    }
}