use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    error::{context, VerboseError},
    multi::{count, many_till},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{pair, tuple},
    IResult,
};
use std::{env, fs};

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
    Gnu,
    Bsd,
    Solaris,
    Aix,
    Irix,
    FreeBSD,
    Tru64,
    Modesto,
    OpenBsd,
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
    M68K,
    M88K,
    I486,
    M860,
    Mips,
    S370,
    MipsRs4Be,
    Parisc,
    Vpp500,
    Sparc32Plus,
    I960,
    Ppc,
    Ppc64,
    S390,
    Spu,
    V9,
    Sha,
    S390X,
    Mma,
    Tpf,
    Hppa,
    X86_64,
    Vax,
    Cris,
    CrisV32,
    Fr30,
    Frv,
    Xtensa,
    Mn10200,
    Mn10300,
    M32R,
    Fr60,
    OpenRisc,
    Rl78,
    Ip2K,
    MmdspPlus,
    X86OpenRiscCore1,
    X86OpenRiscCore2,
    OpenRisc1200,
    RiscV,
    Lanai,
    L10M,
    TilePro,
    TileGx,
}

#[derive(Debug)]
enum ProgramHeaderType {
    Null,
    Load,
    Dynamic,
    Interp,
    Note,
    Shlib,
    Phdr,
    Tls,
    GnuEhFrame,
    GnuStack,
    GnuRelro,
    OpenBsdAnnotations,
    OpenBsdInfo,
    OpenBsdLdpcrel,
    OpenBsdSymbolicInfo,
}

#[derive(Debug)]
struct ProgramHeader {
    p_type: ProgramHeaderType,
    p_offset: u64,
    p_vaddr: u64,
    p_paddr: u64,
    p_filesz: u64,
    p_memsz: u64,
    p_flags: u64,
    p_align: u64,
}

fn parse_elf_class(i: &[u8]) -> IResult<&[u8], ElfClass> {
    context(
        "ELF Class",
        map(be_u8, |x| match x {
            1 => ElfClass::Elf32,
            2 => ElfClass::Elf64,
            _ => panic!("Invalid ELF Class"),
        }),
    )(i)
}

fn parse_data_encoding(i: &[u8]) -> IResult<&[u8], DataEncoding> {
    context(
        "Data Encoding",
        map(be_u8, |x| match x {
            1 => DataEncoding::LittleEndian,
            2 => DataEncoding::BigEndian,
            _ => panic!("Invalid Data Encoding"),
        }),
    )(i)
}

fn parse_os_abi(i: &[u8]) -> IResult<&[u8], OsAbi> {
    context(
        "OS ABI",
        map(be_u8, |x| match x {
            0 => OsAbi::SystemV,
            1 => OsAbi::HpUx,
            2 => OsAbi::NetBsd,
            3 => OsAbi::Gnu,
            4 => OsAbi::Bsd,
            6 => OsAbi::Solaris,
            7 => OsAbi::Aix,
            8 => OsAbi::Irix,
            9 => OsAbi::FreeBSD,
            10 => OsAbi::Tru64,
            11 => OsAbi::Modesto,
            12 => OsAbi::OpenBsd,
            _ => panic!("Invalid OS ABI"),
        }),
    )(i)
}

fn parse_elf_type(i: &[u8]) -> IResult<&[u8], ElfType> {
    context(
        "ELF Type",
        map(be_u16, |x| match x {
            1 => ElfType::Rel,
            2 => ElfType::Exec,
            3 => ElfType::Dyn,
            4 => ElfType::Core,
            _ => panic!("Invalid ELF Type"),
        }),
    )(i)
}

fn parse_machine(i: &[u8]) -> IResult<&[u8], Machine> {
    context(
        "Machine",
        map(be_u16, |x| match x {
            1 => Machine::M32,
            2 => Machine::Sparc,
            3 => Machine::I386,
            4 => Machine::M68K,
            5 => Machine::M88K,
            6 => Machine::I486,
            7 => Machine::M860,
            8 => Machine::Mips,
            9 => Machine::S370,
            10 => Machine::MipsRs4Be,
            11 => Machine::Parisc,
            12 => Machine::Vpp500,
            13 => Machine::Sparc32Plus,
            14 => Machine::I960,
            15 => Machine::Ppc,
            16 => Machine::Ppc64,
            17 => Machine::S390,
            18 => Machine::Spu,
            19 => Machine::V9,
            20 => Machine::Sha,
            21 => Machine::S390X,
            22 => Machine::Mma,
            23 => Machine::Tpf,
            24 => Machine::Hppa,
            25 => Machine::X86_64,
            26 => Machine::Vax,
            27 => Machine::Cris,
            28 => Machine::CrisV32,
            29 => Machine::Fr30,
            30 => Machine::Frv,
            31 => Machine::Xtensa,
            32 => Machine::Mn10200,
            33 => Machine::Mn10300,
            34 => Machine::M32R,
            35 => Machine::Fr60,
            36 => Machine::OpenRisc,
            37 => Machine::Rl78,
            38 => Machine::Ip2K,
            39 => Machine::MmdspPlus,
            40 => Machine::X86OpenRiscCore1,
            41 => Machine::X86OpenRiscCore2,
            42 => Machine::OpenRisc1200,
            43 => Machine::RiscV,
            44 => Machine::Lanai,
            45 => Machine::L10M,
            46 => Machine::TilePro,
            47 => Machine::TileGx,
            _ => panic!("Invalid Machine"),
        }),
    )(i)
}

fn parse_program_header_type(i: &[u8]) -> IResult<&[u8], ProgramHeaderType> {
    context(
        "Program Header Type",
        map(be_u32, |x| match x {
            0 => ProgramHeaderType::Null,
            1 => ProgramHeaderType::Load,
            2 => ProgramHeaderType::Dynamic,
            3 => ProgramHeaderType::Interp,
            4 => ProgramHeaderType::Note,
            5 => ProgramHeaderType::Shlib,
            6 => ProgramHeaderType::Phdr,
            7 => ProgramHeaderType::Tls,
            1685382480 => ProgramHeaderType::GnuEhFrame,
            1685382481 => ProgramHeaderType::GnuStack,
            1685382482 => ProgramHeaderType::GnuRelro,
            1702919312 => ProgramHeaderType::OpenBsdAnnotations,
            1702919313 => ProgramHeaderType::OpenBsdInfo,
            1702919314 => ProgramHeaderType::OpenBsdLdpcrel,
            1702919315 => ProgramHeaderType::OpenBsdSymbolicInfo,
            _ => panic!("Invalid Program Header Type"),
        }),
    )(i)
}

fn parse_program_header(i: &[u8]) -> IResult<&[u8], ProgramHeader> {
    context(
        "Program Header",
        map(
            tuple((
                parse_program_header_type,
                be_u32,
                be_u32,
                be_u32,
                be_u32,
                be_u32,
                be_u32,
                be_u32,
            )),
            |(p_type, p_offset, p_vaddr, p_paddr, p_filesz, p_memsz, p_flags, p_align)| {
                ProgramHeader {
                    p_type,
                    p_offset: p_offset as u64,
                    p_vaddr: p_vaddr as u64,
                    p_paddr: p_paddr as u64,
                    p_filesz: p_filesz as u64,
                    p_memsz: p_memsz as u64,
                    p_flags: p_flags as u64,
                    p_align: p_align as u64,
                }
            },
        ),
    )(i)
}

fn parse_elf_header(i: &[u8]) -> IResult<&[u8], (ElfClass, DataEncoding, OsAbi, ElfType, Machine)> {
    context(
        "ELF Header",
        map(
            tuple((
                tag([0x7F, 0x45, 0x4c, 0x46]),
                parse_elf_class,
                parse_data_encoding,
                parse_os_abi,
                parse_elf_type,
                parse_machine,
            )),
            |(_, e_class, data_encoding, os_abi, elf_type, machine)| {
                (e_class, data_encoding, os_abi, elf_type, machine)
            },
        ),
    )(i)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Please provide a file path");
        return;
    }
    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");
    let (remaining, result) = parse_elf_header(&data).expect("Failed to parse ELF Header");
    println!("ELF Class: {:?}", result.0);
    let ph_offset = 32 + 16;
    let mut i = &data[ph_offset..];
    while !i.is_empty() {
        match parse_program_header(i) {
            Ok((remaining, ph)) => {
                println!("Program Header: {:?}", ph);
                i = remaining;
            }
            Err(_) => {
                break;
            }
        }
    }
}