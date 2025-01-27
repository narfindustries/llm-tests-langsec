use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::take_while_m_n,
    number::complete::{be_u16, be_u32, be_u64},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, Error};

#[derive(Debug)]
enum ElfClass {
    Elf32,
    Elf64,
}

impl ElfClass {
    fn from_byte(byte: u8) -> IResult<&[u8], ElfClass> {
        match byte {
            1 => Ok(([], ElfClass::Elf32)),
            2 => Ok(([], ElfClass::Elf64)),
            _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
        }
    }
}

#[derive(Debug)]
enum ElfData {
    LittleEndian,
    BigEndian,
}

impl ElfData {
    fn from_byte(byte: u8) -> IResult<&[u8], ElfData> {
        match byte {
            1 => Ok(([], ElfData::LittleEndian)),
            2 => Ok(([], ElfData::BigEndian)),
            _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
        }
    }
}

#[derive(Debug)]
enum ElfOsAbi {
    SystemV,
    HP_UX,
    NetBSD,
    GNU,
    Other(u8),
}

impl ElfOsAbi {
    fn from_byte(byte: u8) -> IResult<&[u8], ElfOsAbi> {
        match byte {
            0 => Ok(([], ElfOsAbi::SystemV)),
            1 => Ok(([], ElfOsAbi::HP_UX)),
            2 => Ok(([], ElfOsAbi::NetBSD)),
            3 => Ok(([], ElfOsAbi::GNU)),
            x => Ok(([], ElfOsAbi::Other(x))),
        }
    }
}

#[derive(Debug)]
enum ElfType {
    Rel,
    Exec,
    Dyn,
    Core,
    Other(u16),
}

impl ElfType {
    fn from_be_u16(byte: u16) -> IResult<&[u8], ElfType> {
        match byte {
            1 => Ok(([], ElfType::Rel)),
            2 => Ok(([], ElfType::Exec)),
            3 => Ok(([], ElfType::Dyn)),
            4 => Ok(([], ElfType::Core)),
            x => Ok(([], ElfType::Other(x))),
        }
    }
}

#[derive(Debug)]
enum ElfMachine {
    NoMachine,
    SPARC,
    X86,
    M68K,
    X86_64,
    Other(u16),
}

impl ElfMachine {
    fn from_be_u16(byte: u16) -> IResult<&[u8], ElfMachine> {
        match byte {
            0 => Ok(([], ElfMachine::NoMachine)),
            2 => Ok(([], ElfMachine::SPARC)),
            3 => Ok(([], ElfMachine::X86)),
            4 => Ok(([], ElfMachine::M68K)),
            62 => Ok(([], ElfMachine::X86_64)),
            x => Ok(([], ElfMachine::Other(x))),
        }
    }
}

#[derive(Debug)]
struct ElfHeader {
    class: ElfClass,
    data: ElfData,
    os_abi: ElfOsAbi,
    abi_version: u8,
    pad: [u8; 7],
    type_: ElfType,
    machine: ElfMachine,
    version: u32,
    entry_point: u64,
    program_header_offset: u64,
    section_header_offset: u64,
    flags: u32,
    header_size: u16,
    program_header_size: u16,
    program_header_count: u16,
    section_header_size: u16,
    section_header_count: u16,
    string_table_index: u16,
}

fn elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (
        input,
        (
            magic,
            class,
            data,
            os_abi,
            abi_version,
            pad,
            type_,
            machine,
            version,
            entry_point,
            program_header_offset,
            section_header_offset,
            flags,
            header_size,
            program_header_size,
            program_header_count,
            section_header_size,
            section_header_count,
            string_table_index,
        ),
    ) = tuple((
        tag("\x7fELF"),
        map(take(1usize), ElfClass::from_byte),
        map(take(1usize), ElfData::from_byte),
        map(take(1usize), ElfOsAbi::from_byte),
        take(1usize),
        take(7usize),
        map(be_u16, ElfType::from_be_u16),
        map(be_u16, ElfMachine::from_be_u16),
        be_u32,
        be_u64,
        be_u64,
        be_u64,
        be_u32,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
    ))(input)?;

    let header = ElfHeader {
        class,
        data,
        os_abi,
        abi_version: abi_version[0],
        pad: pad.try_into().unwrap(),
        type_,
        machine,
        version,
        entry_point,
        program_header_offset,
        section_header_offset,
        flags,
        header_size,
        program_header_size,
        program_header_count,
        section_header_size,
        section_header_count,
        string_table_index,
    };

    Ok((input, header))
}

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(Error::new(std::io::ErrorKind::InvalidInput, "Usage: elf_parser <file>"));
    }

    let mut file = File::open(&args[1])?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    match elf_header(&data) {
        Ok((remaining, header)) => {
            println!("{:?}", header);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining", remaining.len());
            }
        }
        Err(err) => {
            return Err(Error::new(std::io::ErrorKind::InvalidData, format!("{:?}", err)));
        }
    }

    Ok(())
}