use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::length_data,
    number::complete::{be_u16, be_u32, be_u64, be_u8},
    sequence::{tuple, pair},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, PartialEq)]
enum ElfClass {
    Elf32,
    Elf64,
}

#[derive(Debug, PartialEq)]
enum ElfDataEncoding {
    LittleEndian,
    BigEndian,
}

#[derive(Debug, PartialEq)]
enum ElfOsAbi {
    SystemV,
    HpUx,
    NetBsd,
    Linux,
    Solaris,
    Aix,
    Irix,
    FreeBSD,
    OpenBSD,
    Other(u8),
}

#[derive(Debug, PartialEq)]
enum ElfType {
    Rel,
    Exec,
    Dyn,
    Core,
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum ElfMachine {
    NoMachine,
    M32,
    Sparc,
    I386,
    M68K,
    M88K,
    I860,
    Mips,
    Other(u16),
}

#[derive(Debug, PartialEq)]
struct ElfHeader32 {
    ident: ElfIdent,
    type_: ElfType,
    machine: ElfMachine,
    version: u32,
    entry: u32,
    phoff: u32,
    shoff: u32,
    flags: u32,
    ehsize: u16,
    phentsize: u16,
    phnum: u16,
    shentsize: u16,
    shnum: u16,
    shstrndx: u16,
}

#[derive(Debug, PartialEq)]
struct ElfHeader64 {
    ident: ElfIdent,
    type_: ElfType,
    machine: ElfMachine,
    version: u32,
    entry: u64,
    phoff: u64,
    shoff: u64,
    flags: u32,
    ehsize: u16,
    phentsize: u16,
    phnum: u16,
    shentsize: u16,
    shnum: u16,
    shstrndx: u16,
}

#[derive(Debug, PartialEq)]
struct ElfIdent {
    class: ElfClass,
    data_encoding: ElfDataEncoding,
    os_abi: ElfOsAbi,
    abi_version: u8,
    pad: [u8; 7],
}

fn elf_class(input: &[u8]) -> IResult<&[u8], ElfClass> {
    map(be_u8, |class| match class {
        1 => ElfClass::Elf32,
        2 => ElfClass::Elf64,
        _ => panic!("Invalid ELF class"),
    })(input)
}

fn elf_data_encoding(input: &[u8]) -> IResult<&[u8], ElfDataEncoding> {
    map(be_u8, |encoding| match encoding {
        1 => ElfDataEncoding::LittleEndian,
        2 => ElfDataEncoding::BigEndian,
        _ => panic!("Invalid ELF data encoding"),
    })(input)
}

fn elf_os_abi(input: &[u8]) -> IResult<&[u8], ElfOsAbi> {
    map(be_u8, |abi| match abi {
        0 => ElfOsAbi::SystemV,
        1 => ElfOsAbi::HpUx,
        2 => ElfOsAbi::NetBsd,
        3 => ElfOsAbi::Linux,
        4 => ElfOsAbi::Solaris,
        5 => ElfOsAbi::Aix,
        6 => ElfOsAbi::Irix,
        7 => ElfOsAbi::FreeBSD,
        8 => ElfOsAbi::OpenBSD,
        abi => ElfOsAbi::Other(abi),
    })(input)
}

fn elf_type(input: &[u8]) -> IResult<&[u8], ElfType> {
    map(be_u16, |type_| match type_ {
        1 => ElfType::Rel,
        2 => ElfType::Exec,
        3 => ElfType::Dyn,
        4 => ElfType::Core,
        type_ => ElfType::Other(type_),
    })(input)
}

fn elf_machine(input: &[u8]) -> IResult<&[u8], ElfMachine> {
    map(be_u16, |machine| match machine {
        0 => ElfMachine::NoMachine,
        1 => ElfMachine::M32,
        2 => ElfMachine::Sparc,
        3 => ElfMachine::I386,
        4 => ElfMachine::M68K,
        5 => ElfMachine::M88K,
        7 => ElfMachine::I860,
        8 => ElfMachine::Mips,
        machine => ElfMachine::Other(machine),
    })(input)
}

fn elf_ident(input: &[u8]) -> IResult<&[u8], ElfIdent> {
    let (input, magic) = tag("\x7fELF".as_bytes())(input)?;
    let (input, class) = elf_class(input)?;
    let (input, data_encoding) = elf_data_encoding(input)?;
    let (input, os_abi) = elf_os_abi(input)?;
    let (input, abi_version) = be_u8(input)?;
    let (input, pad) = take(7u8)(input)?;
    Ok((input, ElfIdent { class, data_encoding, os_abi, abi_version, pad: pad.try_into().unwrap() }))
}

fn elf_header32(input: &[u8]) -> IResult<&[u8], ElfHeader32> {
    let (input, ident) = elf_ident(input)?;
    let (input, type_) = elf_type(input)?;
    let (input, machine) = elf_machine(input)?;
    let (input, version) = be_u32(input)?;
    let (input, entry) = be_u32(input)?;
    let (input, phoff) = be_u32(input)?;
    let (input, shoff) = be_u32(input)?;
    let (input, flags) = be_u32(input)?;
    let (input, ehsize) = be_u16(input)?;
    let (input, phentsize) = be_u16(input)?;
    let (input, phnum) = be_u16(input)?;
    let (input, shentsize) = be_u16(input)?;
    let (input, shnum) = be_u16(input)?;
    let (input, shstrndx) = be_u16(input)?;
    Ok((input, ElfHeader32 { ident, type_, machine, version, entry, phoff, shoff, flags, ehsize, phentsize, phnum, shentsize, shnum, shstrndx }))
}

fn elf_header64(input: &[u8]) -> IResult<&[u8], ElfHeader64> {
    let (input, ident) = elf_ident(input)?;
    let (input, type_) = elf_type(input)?;
    let (input, machine) = elf_machine(input)?;
    let (input, version) = be_u32(input)?;
    let (input, entry) = be_u64(input)?;
    let (input, phoff) = be_u64(input)?;
    let (input, shoff) = be_u64(input)?;
    let (input, flags) = be_u32(input)?;
    let (input, ehsize) = be_u16(input)?;
    let (input, phentsize) = be_u16(input)?;
    let (input, phnum) = be_u16(input)?;
    let (input, shentsize) = be_u16(input)?;
    let (input, shnum) = be_u16(input)?;
    let (input, shstrndx) = be_u16(input)?;
    Ok((input, ElfHeader64 { ident, type_, machine, version, entry, phoff, shoff, flags, ehsize, phentsize, phnum, shentsize, shnum, shstrndx }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <elf_file>", args[0]);
        return Ok(());
    }
    let file = File::open(&args[1])?;
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer)?;
    let (input, header) = verify(elf_ident, |ident| ident.class == ElfClass::Elf32)(buffer.as_slice())?;
    match header.ident.class {
        ElfClass::Elf32 => {
            let (_input, header) = elf_header32(input)?;
            println!("{:?}", header);
        }
        ElfClass::Elf64 => {
            let (_input, header) = elf_header64(input)?;
            println!("{:?}", header);
        }
    }
    Ok(())
}