use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::take_while_m_n,
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
enum ElfOsAbi {
    SystemV,
    HpUx,
    NetBsd,
    Linux,
    Solaris,
    Aix,
    Irix,
    FreeBsd,
    Tru64,
    NovellModesto,
    OpenBsd,
    OpenVms,
    Nsk,
    Aros,
    FenixOs,
    CloudAbi,
    StratusVos,
}

#[derive(Debug)]
enum ElfType {
    Rel,
    Exec,
    Dyn,
    Core,
}

#[derive(Debug)]
enum ElfMachine {
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
    Ncubble,
    Vpp500,
    Sparc32Plus,
    I960,
    PowerPc,
    PowerPc64,
    S390,
    Spu,
    V9,
    Ia64,
    X86_64,
    L1Om,
    K1Om,
    K10Om,
    MipsX,
    AmdGpu,
    Vdso,
}

#[derive(Debug)]
struct ElfHeader {
    magic: [u8; 4],
    class: ElfClass,
    data: ElfData,
    os_abi: ElfOsAbi,
    abi_version: u8,
    pad: [u8; 7],
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

fn parse_elf_os_abi(input: &[u8]) -> IResult<&[u8], ElfOsAbi> {
    map(be_u8, |x| match x {
        0 => ElfOsAbi::SystemV,
        1 => ElfOsAbi::HpUx,
        2 => ElfOsAbi::NetBsd,
        3 => ElfOsAbi::Linux,
        4 => ElfOsAbi::Solaris,
        5 => ElfOsAbi::Aix,
        6 => ElfOsAbi::Irix,
        7 => ElfOsAbi::FreeBsd,
        8 => ElfOsAbi::Tru64,
        9 => ElfOsAbi::NovellModesto,
        10 => ElfOsAbi::OpenBsd,
        11 => ElfOsAbi::OpenVms,
        12 => ElfOsAbi::Nsk,
        13 => ElfOsAbi::Aros,
        14 => ElfOsAbi::FenixOs,
        15 => ElfOsAbi::CloudAbi,
        16 => ElfOsAbi::StratusVos,
        _ => panic!("Invalid ELF OS/ABI"),
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

fn parse_elf_machine(input: &[u8]) -> IResult<&[u8], ElfMachine> {
    map(be_u16, |x| match x {
        0 => ElfMachine::NoMachine,
        1 => ElfMachine::M32,
        2 => ElfMachine::Sparc,
        3 => ElfMachine::I386,
        4 => ElfMachine::M68K,
        5 => ElfMachine::M88K,
        6 => ElfMachine::I860,
        7 => ElfMachine::Mips,
        8 => ElfMachine::S370,
        9 => ElfMachine::MipsRs3Le,
        10 => ElfMachine::Parisc,
        11 => ElfMachine::Ncubble,
        12 => ElfMachine::Vpp500,
        13 => ElfMachine::Sparc32Plus,
        14 => ElfMachine::I960,
        15 => ElfMachine::PowerPc,
        16 => ElfMachine::PowerPc64,
        17 => ElfMachine::S390,
        18 => ElfMachine::Spu,
        19 => ElfMachine::V9,
        20 => ElfMachine::Ia64,
        21 => ElfMachine::X86_64,
        22 => ElfMachine::L1Om,
        23 => ElfMachine::K1Om,
        24 => ElfMachine::K10Om,
        25 => ElfMachine::MipsX,
        26 => ElfMachine::AmdGpu,
        27 => ElfMachine::Vdso,
        _ => panic!("Invalid ELF machine"),
    })(input)
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, magic) = take(4u8)(input)?;
    let (input, class) = parse_elf_class(input)?;
    let (input, data) = parse_elf_data(input)?;
    let (input, os_abi) = parse_elf_os_abi(input)?;
    let (input, abi_version) = be_u8(input)?;
    let (input, pad) = take(7u8)(input)?;
    let (input, type_) = parse_elf_type(input)?;
    let (input, machine) = parse_elf_machine(input)?;
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

    Ok((
        input,
        ElfHeader {
            magic,
            class,
            data,
            os_abi,
            abi_version,
            pad,
            type_,
            machine,
            version,
            entry,
            phoff,
            shoff,
            flags,
            ehsize,
            phentsize,
            phnum,
            shentsize,
            shnum,
            shstrndx,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <file>", args[0]);
    }

    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).unwrap();

    let (_, header) = parse_elf_header(&buffer).unwrap();
    println!("{:?}", header);
}