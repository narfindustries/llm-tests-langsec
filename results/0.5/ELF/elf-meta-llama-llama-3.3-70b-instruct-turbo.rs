use nom::{
    bytes::complete::{take},
    combinator::{map},
    multi::{many1},
    number::complete::{be_u16, be_u32, be_u64, be_u8},
    IResult,
};
use std::{env, fs};

#[derive(Debug)]
enum EiClass {
    Elf32,
    Elf64,
}

#[derive(Debug)]
enum EiData {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
enum EiOsabi {
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
    Other(u8),
}

#[derive(Debug)]
enum EiAbiversion {
    Current,
    Other(u8),
}

#[derive(Debug)]
struct EiIdent {
    ei_mag0: u8,
    ei_mag1: u8,
    ei_mag2: u8,
    ei_mag3: u8,
    ei_class: EiClass,
    ei_data: EiData,
    ei_version: u8,
    ei_osabi: EiOsabi,
    ei_abiversion: EiAbiversion,
    ei_pad: [u8; 7],
}

fn parse_ei_ident(input: &[u8]) -> IResult<&[u8], EiIdent> {
    let (input, ei_mag0) = be_u8(input)?;
    let (input, ei_mag1) = be_u8(input)?;
    let (input, ei_mag2) = be_u8(input)?;
    let (input, ei_mag3) = be_u8(input)?;
    let (input, ei_class) = map(be_u8, |x| match x {
        1 => EiClass::Elf32,
        2 => EiClass::Elf64,
        _ => panic!("Invalid EiClass"),
    })(input)?;
    let (input, ei_data) = map(be_u8, |x| match x {
        1 => EiData::LittleEndian,
        2 => EiData::BigEndian,
        _ => panic!("Invalid EiData"),
    })(input)?;
    let (input, ei_version) = be_u8(input)?;
    let (input, ei_osabi) = map(be_u8, |x| match x {
        0 => EiOsabi::SystemV,
        1 => EiOsabi::HpUx,
        2 => EiOsabi::NetBsd,
        3 => EiOsabi::Linux,
        6 => EiOsabi::Solaris,
        7 => EiOsabi::Aix,
        8 => EiOsabi::Irix,
        9 => EiOsabi::FreeBSD,
        10 => EiOsabi::Tru64,
        11 => EiOsabi::NovellModesto,
        12 => EiOsabi::OpenBsd,
        x => EiOsabi::Other(x),
    })(input)?;
    let (input, ei_abiversion) = map(be_u8, |x| match x {
        0 => EiAbiversion::Current,
        x => EiAbiversion::Other(x),
    })(input)?;
    let (input, ei_pad) = take(7u8)(input)?;
    Ok((
        input,
        EiIdent {
            ei_mag0,
            ei_mag1,
            ei_mag2,
            ei_mag3,
            ei_class,
            ei_data,
            ei_version,
            ei_osabi,
            ei_abiversion,
            ei_pad: ei_pad.try_into().expect("Invalid ei_pad"),
        },
    ))
}

#[derive(Debug)]
enum Et {
    Rel,
    Exec,
    Dyn,
    Core,
    Other(u16),
}

#[derive(Debug)]
struct ElfHeader32 {
    e_ident: EiIdent,
    e_type: Et,
    e_machine: u16,
    e_version: u32,
    e_entry: u32,
    e_phoff: u32,
    e_shoff: u32,
    e_flags: u32,
    e_ehsize: u16,
    e_phentsize: u16,
    e_phnum: u16,
    e_shentsize: u16,
    e_shnum: u16,
    e_shstrndx: u16,
}

fn parse_elf_header_32(input: &[u8]) -> IResult<&[u8], ElfHeader32> {
    let (input, e_ident) = parse_ei_ident(input)?;
    let (input, e_type) = map(be_u16, |x| match x {
        1 => Et::Rel,
        2 => Et::Exec,
        3 => Et::Dyn,
        4 => Et::Core,
        x => Et::Other(x),
    })(input)?;
    let (input, e_machine) = be_u16(input)?;
    let (input, e_version) = be_u32(input)?;
    let (input, e_entry) = be_u32(input)?;
    let (input, e_phoff) = be_u32(input)?;
    let (input, e_shoff) = be_u32(input)?;
    let (input, e_flags) = be_u32(input)?;
    let (input, e_ehsize) = be_u16(input)?;
    let (input, e_phentsize) = be_u16(input)?;
    let (input, e_phnum) = be_u16(input)?;
    let (input, e_shentsize) = be_u16(input)?;
    let (input, e_shnum) = be_u16(input)?;
    let (input, e_shstrndx) = be_u16(input)?;
    Ok((
        input,
        ElfHeader32 {
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
struct ElfHeader64 {
    e_ident: EiIdent,
    e_type: Et,
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

fn parse_elf_header_64(input: &[u8]) -> IResult<&[u8], ElfHeader64> {
    let (input, e_ident) = parse_ei_ident(input)?;
    let (input, e_type) = map(be_u16, |x| match x {
        1 => Et::Rel,
        2 => Et::Exec,
        3 => Et::Dyn,
        4 => Et::Core,
        x => Et::Other(x),
    })(input)?;
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
        ElfHeader64 {
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
enum Pt {
    Null,
    Load,
    Dynamic,
    Interp,
    Note,
    Shlib,
    Phdr,
    Tls,
    Other(u32),
}

#[derive(Debug)]
struct ProgramHeader32 {
    p_type: Pt,
    p_offset: u32,
    p_vaddr: u32,
    p_paddr: u32,
    p_filesz: u32,
    p_memsz: u32,
    p_flags: u32,
    p_align: u32,
}

fn parse_program_header_32(input: &[u8]) -> IResult<&[u8], ProgramHeader32> {
    let (input, p_type) = map(be_u32, |x| match x {
        0 => Pt::Null,
        1 => Pt::Load,
        2 => Pt::Dynamic,
        3 => Pt::Interp,
        4 => Pt::Note,
        5 => Pt::Shlib,
        6 => Pt::Phdr,
        7 => Pt::Tls,
        x => Pt::Other(x),
    })(input)?;
    let (input, p_offset) = be_u32(input)?;
    let (input, p_vaddr) = be_u32(input)?;
    let (input, p_paddr) = be_u32(input)?;
    let (input, p_filesz) = be_u32(input)?;
    let (input, p_memsz) = be_u32(input)?;
    let (input, p_flags) = be_u32(input)?;
    let (input, p_align) = be_u32(input)?;
    Ok((
        input,
        ProgramHeader32 {
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
struct ProgramHeader64 {
    p_type: Pt,
    p_flags: u32,
    p_offset: u64,
    p_vaddr: u64,
    p_paddr: u64,
    p_filesz: u64,
    p_memsz: u64,
    p_align: u64,
}

fn parse_program_header_64(input: &[u8]) -> IResult<&[u8], ProgramHeader64> {
    let (input, p_type) = map(be_u32, |x| match x {
        0 => Pt::Null,
        1 => Pt::Load,
        2 => Pt::Dynamic,
        3 => Pt::Interp,
        4 => Pt::Note,
        5 => Pt::Shlib,
        6 => Pt::Phdr,
        7 => Pt::Tls,
        x => Pt::Other(x),
    })(input)?;
    let (input, p_flags) = be_u32(input)?;
    let (input, p_offset) = be_u64(input)?;
    let (input, p_vaddr) = be_u64(input)?;
    let (input, p_paddr) = be_u64(input)?;
    let (input, p_filesz) = be_u64(input)?;
    let (input, p_memsz) = be_u64(input)?;
    let (input, p_align) = be_u64(input)?;
    Ok((
        input,
        ProgramHeader64 {
            p_type,
            p_flags,
            p_offset,
            p_vaddr,
            p_paddr,
            p_filesz,
            p_memsz,
            p_align,
        },
    ))
}

#[derive(Debug)]
enum Sht {
    Null,
    Progbits,
    Symtab,
    Strtab,
    Rela,
    Hash,
    Dynamic,
    Note,
    Nobits,
    Rel,
    Shlib,
    Dynsym,
    InitArray,
    FiniArray,
    PreinitArray,
    Group,
    SymtabShndx,
    Other(u16),
}

#[derive(Debug)]
struct SectionHeader32 {
    sh_name: u32,
    sh_type: Sht,
    sh_flags: u32,
    sh_addr: u32,
    sh_offset: u32,
    sh_size: u32,
    sh_link: u32,
    sh_info: u32,
    sh_addralign: u32,
    sh_entsize: u32,
}

fn parse_section_header_32(input: &[u8]) -> IResult<&[u8], SectionHeader32> {
    let (input, sh_name) = be_u32(input)?;
    let (input, sh_type) = map(be_u32, |x| match x {
        0 => Sht::Null,
        1 => Sht::Progbits,
        2 => Sht::Symtab,
        3 => Sht::Strtab,
        4 => Sht::Rela,
        5 => Sht::Hash,
        6 => Sht::Dynamic,
        7 => Sht::Note,
        8 => Sht::Nobits,
        9 => Sht::Rel,
        10 => Sht::Shlib,
        11 => Sht::Dynsym,
        12 => Sht::InitArray,
        13 => Sht::FiniArray,
        14 => Sht::PreinitArray,
        15 => Sht::Group,
        16 => Sht::SymtabShndx,
        x => Sht::Other(x as u16),
    })(input)?;
    let (input, sh_flags) = be_u32(input)?;
    let (input, sh_addr) = be_u32(input)?;
    let (input, sh_offset) = be_u32(input)?;
    let (input, sh_size) = be_u32(input)?;
    let (input, sh_link) = be_u32(input)?;
    let (input, sh_info) = be_u32(input)?;
    let (input, sh_addralign) = be_u32(input)?;
    let (input, sh_entsize) = be_u32(input)?;
    Ok((
        input,
        SectionHeader32 {
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

#[derive(Debug)]
struct SectionHeader64 {
    sh_name: u32,
    sh_type: Sht,
    sh_flags: u64,
    sh_addr: u64,
    sh_offset: u64,
    sh_size: u64,
    sh_link: u32,
    sh_info: u32,
    sh_addralign: u64,
    sh_entsize: u64,
}

fn parse_section_header_64(input: &[u8]) -> IResult<&[u8], SectionHeader64> {
    let (input, sh_name) = be_u32(input)?;
    let (input, sh_type) = map(be_u32, |x| match x {
        0 => Sht::Null,
        1 => Sht::Progbits,
        2 => Sht::Symtab,
        3 => Sht::Strtab,
        4 => Sht::Rela,
        5 => Sht::Hash,
        6 => Sht::Dynamic,
        7 => Sht::Note,
        8 => Sht::Nobits,
        9 => Sht::Rel,
        10 => Sht::Shlib,
        11 => Sht::Dynsym,
        12 => Sht::InitArray,
        13 => Sht::FiniArray,
        14 => Sht::PreinitArray,
        15 => Sht::Group,
        16 => Sht::SymtabShndx,
        x => Sht::Other(x as u16),
    })(input)?;
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
        SectionHeader64 {
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
        println!("Usage: {} <file>", args[0]);
        return;
    }
    let file = fs::read(args[1].clone()).expect("Failed to read file");
    match parse_ei_ident(&file) {
        Ok((remaining, e_ident)) => {
            match e_ident.ei_class {
                EiClass::Elf32 => {
                    match parse_elf_header_32(remaining) {
                        Ok((remaining, elf_header)) => {
                            println!("{:?}", elf_header);
                            let program_headers: Vec<ProgramHeader32> = many1(parse_program_header_32)(remaining)
                                .unwrap()
                                .1;
                            println!("{:?}", program_headers);
                            let section_headers: Vec<SectionHeader32> = many1(parse_section_header_32)(remaining)
                                .unwrap()
                                .1;
                            println!("{:?}", section_headers);
                        }
                        Err(e) => println!("{:?}", e),
                    }
                }
                EiClass::Elf64 => {
                    match parse_elf_header_64(remaining) {
                        Ok((remaining, elf_header)) => {
                            println!("{:?}", elf_header);
                            let program_headers: Vec<ProgramHeader64> = many1(parse_program_header_64)(remaining)
                                .unwrap()
                                .1;
                            println!("{:?}", program_headers);
                            let section_headers: Vec<SectionHeader64> = many1(parse_section_header_64)(remaining)
                                .unwrap()
                                .1;
                            println!("{:?}", section_headers);
                        }
                        Err(e) => println!("{:?}", e),
                    }
                }
            }
        }
        Err(e) => println!("{:?}", e),
    }
}