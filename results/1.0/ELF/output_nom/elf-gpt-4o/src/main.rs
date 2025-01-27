use nom::{
    bytes::complete::tag,
    combinator::map,
    multi::count,
    number::complete::{le_u16, le_u32, le_u64, le_i32, le_i16, le_i64, be_u32, be_u64, be_i32, be_i16, be_i64},
    sequence::{tuple, terminated},
    IResult, error::ParseError, Err,
};
use std::fs;
use std::env;

#[derive(Debug)]
enum ElfClass {
    Class32,
    Class64,
}

#[derive(Debug)]
enum ElfData {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
enum ElfOSABI {
    SystemV,
    HP_UX,
    NetBSD,
    Linux,
    Solaris,
    AIX,
    IRIX,
    FreeBSD,
    Tru64,
    NovellModesto,
    OpenBSD,
    OpenVMS,
    NSK,
    AROS,
    FenixOS,
    CloudABI,
    StratusTechnologiesOpenVOS,
}

#[derive(Debug)]
struct ElfHeader {
    ei_class: ElfClass,
    ei_data: ElfData,
    ei_version: u8,
    ei_osabi: ElfOSABI,
    ei_abiversion: u8,
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

fn parse_elf_class(input: &[u8]) -> IResult<&[u8], ElfClass> {
    let (input, class_byte) = nom::bytes::complete::take(1u8)(input)?;
    let class = match class_byte[0] {
        1 => ElfClass::Class32,
        2 => ElfClass::Class64,
        _ => return Err(Err::Error(ParseError::from_error_kind(input, nom::error::ErrorKind::Tag))),
    };
    Ok((input, class))
}

fn parse_elf_data(input: &[u8]) -> IResult<&[u8], ElfData> {
    let (input, data_byte) = nom::bytes::complete::take(1u8)(input)?;
    let data = match data_byte[0] {
        1 => ElfData::LittleEndian,
        2 => ElfData::BigEndian,
        _ => return Err(Err::Error(ParseError::from_error_kind(input, nom::error::ErrorKind::Tag))),
    };
    Ok((input, data))
}

fn parse_elf_osabi(input: &[u8]) -> IResult<&[u8], ElfOSABI> {
    let (input, osabi_byte) = nom::bytes::complete::take(1u8)(input)?;
    let osabi = match osabi_byte[0] {
        0 => ElfOSABI::SystemV,
        1 => ElfOSABI::HP_UX,
        2 => ElfOSABI::NetBSD,
        3 => ElfOSABI::Linux,
        6 => ElfOSABI::Solaris,
        7 => ElfOSABI::AIX,
        8 => ElfOSABI::IRIX,
        9 => ElfOSABI::FreeBSD,
        10 => ElfOSABI::Tru64,
        11 => ElfOSABI::NovellModesto,
        12 => ElfOSABI::OpenBSD,
        13 => ElfOSABI::OpenVMS,
        14 => ElfOSABI::NSK,
        15 => ElfOSABI::AROS,
        16 => ElfOSABI::FenixOS,
        17 => ElfOSABI::CloudABI,
        18 => ElfOSABI::StratusTechnologiesOpenVOS,
        _ => return Err(Err::Error(ParseError::from_error_kind(input, nom::error::ErrorKind::Tag))),
    };
    Ok((input, osabi))
}

fn parse_u16(input: &[u8], is_le: bool) -> IResult<&[u8], u16> {
    if is_le {
        le_u16(input)
    } else {
        be_u16(input)
    }
}

fn parse_u32(input: &[u8], is_le: bool) -> IResult<&[u8], u32> {
    if is_le {
        le_u32(input)
    } else {
        be_u32(input)
    }
}

fn parse_u64(input: &[u8], is_le: bool) -> IResult<&[u8], u64> {
    if is_le {
        le_u64(input)
    } else {
        be_u64(input)
    }
}

fn parse_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, _) = tag([0x7f, b'E', b'L', b'F'])(input)?;
    let (input, ei_class) = parse_elf_class(input)?;
    let (input, ei_data) = parse_elf_data(input)?;
    let (input, ei_version) = nom::number::complete::u8(input)?;
    let (input, ei_osabi) = parse_elf_osabi(input)?;
    let (input, ei_abiversion) = nom::number::complete::u8(input)?;
    let (input, _) = count(nom::number::complete::u8, 7)(input)?;

    let is_le = if let ElfData::LittleEndian = ei_data { true } else { false };

    let (input, e_type) = parse_u16(input, is_le)?;
    let (input, e_machine) = parse_u16(input, is_le)?;
    let (input, e_version) = parse_u32(input, is_le)?;
    let (input, e_entry) = match ei_class {
        ElfClass::Class32 => map(parse_u32(input, is_le), u64::from)?,
        ElfClass::Class64 => parse_u64(input, is_le)?,
    };
    let (input, e_phoff) = match ei_class {
        ElfClass::Class32 => map(parse_u32(input, is_le), u64::from)?,
        ElfClass::Class64 => parse_u64(input, is_le)?,
    };
    let (input, e_shoff) = match ei_class {
        ElfClass::Class32 => map(parse_u32(input, is_le), u64::from)?,
        ElfClass::Class64 => parse_u64(input, is_le)?,
    };
    let (input, e_flags) = parse_u32(input, is_le)?;
    let (input, e_ehsize) = parse_u16(input, is_le)?;
    let (input, e_phentsize) = parse_u16(input, is_le)?;
    let (input, e_phnum) = parse_u16(input, is_le)?;
    let (input, e_shentsize) = parse_u16(input, is_le)?;
    let (input, e_shnum) = parse_u16(input, is_le)?;
    let (input, e_shstrndx) = parse_u16(input, is_le)?;

    let header = ElfHeader {
        ei_class,
        ei_data,
        ei_version,
        ei_osabi,
        ei_abiversion,
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
    };

    Ok((input, header))
}

fn parse_elf(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    parse_header(input)
}

fn read_file(filename: &str) -> std::io::Result<Vec<u8>> {
    let data = fs::read(filename)?;
    Ok(data)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <ELF file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];

    let input = match read_file(filename) {
        Ok(data) => data,
        Err(err) => {
            eprintln!("Error reading file {}: {}", filename, err);
            std::process::exit(1);
        }
    };

    match parse_elf(&input) {
        Ok((_, header)) => {
            println!("{:?}", header);
        }
        Err(err) => {
            eprintln!("Failed to parse ELF file: {:?}", err);
        }
    }
}