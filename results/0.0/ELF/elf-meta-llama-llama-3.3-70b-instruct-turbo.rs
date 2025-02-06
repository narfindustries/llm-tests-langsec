use nom::{
    bytes::complete::{tag, take},
    combinator::{map},
    multi::{count},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, PartialEq)]
enum EiClass {
    Elf32,
    Elf64,
}

#[derive(Debug, PartialEq)]
enum EiData {
    LittleEndian,
    BigEndian,
}

#[derive(Debug, PartialEq)]
enum EiOsabi {
    SystemV,
    HpUx,
    Netbsd,
    Linux,
    Solaris,
    Aix,
    Irix,
    FreeBSD,
    Tru64,
    NovellModesto,
    OpenBSD,
}

#[derive(Debug, PartialEq)]
enum EiAbiversion {
    CurrentVersion,
}

#[derive(Debug, PartialEq)]
enum Et {
    Rel,
    Exec,
    Dyn,
    Core,
}

#[derive(Debug, PartialEq)]
enum Em {
    X86,
    X86_64,
    Arm,
    // Add more architectures as needed
}

#[derive(Debug, PartialEq)]
enum Pt {
    Null,
    Load,
    Dynamic,
    Interp,
    Note,
    Shlib,
    Phdr,
}

#[derive(Debug, PartialEq)]
enum Pf {
    X,
    W,
    R,
}

#[derive(Debug, PartialEq)]
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
}

#[derive(Debug, PartialEq)]
enum Shf {
    Write,
    Alloc,
    Execinstr,
}

#[derive(Debug, PartialEq)]
enum Stb {
    Local,
    Global,
    Weak,
}

#[derive(Debug, PartialEq)]
enum Stv {
    Default,
    Internal,
    Hidden,
}

#[derive(Debug, PartialEq)]
enum Dt {
    Null,
    Needed,
    Pltrelsz,
    Pltgot,
    // Add more dynamic tags as needed
}

fn parse_ei_class(input: &[u8]) -> IResult<&[u8], EiClass> {
    map(be_u8, |x| match x {
        1 => EiClass::Elf32,
        2 => EiClass::Elf64,
        _ => panic!("Invalid EiClass"),
    })(input)
}

fn parse_ei_data(input: &[u8]) -> IResult<&[u8], EiData> {
    map(be_u8, |x| match x {
        1 => EiData::LittleEndian,
        2 => EiData::BigEndian,
        _ => panic!("Invalid EiData"),
    })(input)
}

fn parse_ei_osabi(input: &[u8]) -> IResult<&[u8], EiOsabi> {
    map(be_u8, |x| match x {
        0 => EiOsabi::SystemV,
        1 => EiOsabi::HpUx,
        2 => EiOsabi::Netbsd,
        3 => EiOsabi::Linux,
        6 => EiOsabi::Solaris,
        7 => EiOsabi::Aix,
        8 => EiOsabi::Irix,
        9 => EiOsabi::FreeBSD,
        10 => EiOsabi::Tru64,
        11 => EiOsabi::NovellModesto,
        12 => EiOsabi::OpenBSD,
        _ => panic!("Invalid EiOsabi"),
    })(input)
}

fn parse_ei_abiversion(input: &[u8]) -> IResult<&[u8], EiAbiversion> {
    map(be_u8, |x| match x {
        0 => EiAbiversion::CurrentVersion,
        _ => panic!("Invalid EiAbiversion"),
    })(input)
}

fn parse_et(input: &[u8]) -> IResult<&[u8], Et> {
    map(be_u16, |x| match x {
        1 => Et::Rel,
        2 => Et::Exec,
        3 => Et::Dyn,
        4 => Et::Core,
        _ => panic!("Invalid Et"),
    })(input)
}

fn parse_em(input: &[u8]) -> IResult<&[u8], Em> {
    map(be_u16, |x| match x {
        3 => Em::X86,
        40 => Em::Arm,
        62 => Em::X86_64,
        // Add more architectures as needed
        _ => panic!("Invalid Em"),
    })(input)
}

fn parse_pt(input: &[u8]) -> IResult<&[u8], Pt> {
    map(be_u32, |x| match x {
        0 => Pt::Null,
        1 => Pt::Load,
        2 => Pt::Dynamic,
        3 => Pt::Interp,
        4 => Pt::Note,
        5 => Pt::Shlib,
        6 => Pt::Phdr,
        _ => panic!("Invalid Pt"),
    })(input)
}

fn parse_pf(input: &[u8]) -> IResult<&[u8], Pf> {
    map(be_u32, |x| match x {
        1 => Pf::X,
        2 => Pf::W,
        4 => Pf::R,
        _ => panic!("Invalid Pf"),
    })(input)
}

fn parse_sht(input: &[u8]) -> IResult<&[u8], Sht> {
    map(be_u32, |x| match x {
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
        _ => panic!("Invalid Sht"),
    })(input)
}

fn parse_shf(input: &[u8]) -> IResult<&[u8], Shf> {
    map(be_u32, |x| match x {
        1 => Shf::Write,
        2 => Shf::Alloc,
        4 => Shf::Execinstr,
        _ => panic!("Invalid Shf"),
    })(input)
}

fn parse_stb(input: &[u8]) -> IResult<&[u8], Stb> {
    map(be_u8, |x| match x {
        0 => Stb::Local,
        1 => Stb::Global,
        2 => Stb::Weak,
        _ => panic!("Invalid Stb"),
    })(input)
}

fn parse_stv(input: &[u8]) -> IResult<&[u8], Stv> {
    map(be_u8, |x| match x {
        0 => Stv::Default,
        1 => Stv::Internal,
        2 => Stv::Hidden,
        _ => panic!("Invalid Stv"),
    })(input)
}

fn parse_dt(input: &[u8]) -> IResult<&[u8], Dt> {
    map(be_u32, |x| match x {
        0 => Dt::Null,
        1 => Dt::Needed,
        2 => Dt::Pltrelsz,
        3 => Dt::Pltgot,
        // Add more dynamic tags as needed
        _ => panic!("Invalid Dt"),
    })(input)
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0x7F, b'E', b'L', b'F'])(input)?;
    let (input, _) = parse_ei_class(input)?;
    let (input, _) = parse_ei_data(input)?;
    let (input, _) = parse_ei_osabi(input)?;
    let (input, _) = parse_ei_abiversion(input)?;
    let (input, _) = take(7u8)(input)?;
    let (input, _) = parse_et(input)?;
    let (input, _) = parse_em(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u16(input)?;
    let (input, _) = be_u16(input)?;
    let (input, _) = be_u16(input)?;
    let (input, _) = be_u16(input)?;
    let (input, _) = be_u16(input)?;
    Ok((input, ()))
}

fn parse_program_header(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = parse_pt(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = parse_pf(input)?;
    let (input, _) = be_u32(input)?;
    Ok((input, ()))
}

fn parse_section_header(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = be_u32(input)?;
    let (input, _) = parse_sht(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    Ok((input, ()))
}

fn parse_symbol_table(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = parse_stb(input)?;
    let (input, _) = parse_stv(input)?;
    let (input, _) = be_u16(input)?;
    Ok((input, ()))
}

fn parse_relocation_table(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    let (input, _) = be_u32(input)?;
    Ok((input, ()))
}

fn parse_dynamic_section(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = parse_dt(input)?;
    let (input, _) = be_u32(input)?;
    Ok((input, ()))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <file>", args[0]);
    }
    let file = File::open(&args[1])?;
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input)?;
    let mut remaining = input.as_slice();
    let _ = parse_elf_header(remaining);
    remaining = match parse_elf_header(remaining) {
        Ok((i, _)) => i,
        Err(_) => panic!("Invalid ELF file"),
    };
    let _ = count(parse_program_header, 1)(remaining);
    remaining = match count(parse_program_header, 1)(remaining) {
        Ok((i, _)) => i,
        Err(_) => panic!("Invalid ELF file"),
    };
    let _ = count(parse_section_header, 1)(remaining);
    remaining = match count(parse_section_header, 1)(remaining) {
        Ok((i, _)) => i,
        Err(_) => panic!("Invalid ELF file"),
    };
    let _ = count(parse_symbol_table, 1)(remaining);
    remaining = match count(parse_symbol_table, 1)(remaining) {
        Ok((i, _)) => i,
        Err(_) => panic!("Invalid ELF file"),
    };
    let _ = count(parse_relocation_table, 1)(remaining);
    remaining = match count(parse_relocation_table, 1)(remaining) {
        Ok((i, _)) => i,
        Err(_) => panic!("Invalid ELF file"),
    };
    let _ = count(parse_dynamic_section, 1)(remaining);
    remaining = match count(parse_dynamic_section, 1)(remaining) {
        Ok((i, _)) => i,
        Err(_) => panic!("Invalid ELF file"),
    };
    if !remaining.is_empty() {
        panic!("Invalid ELF file");
    }
    Ok(())
}