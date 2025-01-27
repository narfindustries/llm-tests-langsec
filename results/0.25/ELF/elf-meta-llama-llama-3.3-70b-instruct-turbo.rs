use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::{map, map_res};
use nom::error::{context, Err, ErrorKind};
use nom::multi::take_till;
use nom::number::complete::{be_u16, be_u32, be_u64};
use nom::sequence::{delimited, preceded, tuple};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum ElfClass {
    Elf32,
    Elf64,
}

#[derive(Debug, PartialEq)]
enum ElfData {
    LittleEndian,
    BigEndian,
}

#[derive(Debug, PartialEq)]
enum ElfOsAbi {
    SystemV,
    HP_UX,
    NetBSD,
    GNU,
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
    X86,
    M68K,
    M88K,
    I370,
    MIPS,
    Other(u16),
}

#[derive(Debug, PartialEq)]
struct ElfHeader {
    class: ElfClass,
    data: ElfData,
    os_abi: ElfOsAbi,
    abi_version: u8,
    type_: ElfType,
    machine: ElfMachine,
    version: u32,
    entry_point: u64,
    program_header_offset: u64,
    section_header_offset: u64,
    flags: u32,
    header_size: u16,
    program_header_entry_size: u16,
    program_header_count: u16,
    section_header_entry_size: u16,
    section_header_count: u16,
    string_table_index: u16,
}

fn parse_elf_class(input: &[u8]) -> nom::IResult<&[u8], ElfClass> {
    alt((map(tag([1]), |_| ElfClass::Elf32), map(tag([2]), |_| ElfClass::Elf64)))(input)
}

fn parse_elf_data(input: &[u8]) -> nom::IResult<&[u8], ElfData> {
    alt((map(tag([1]), |_| ElfData::LittleEndian), map(tag([2]), |_| ElfData::BigEndian)))(input)
}

fn parse_elf_os_abi(input: &[u8]) -> nom::IResult<&[u8], ElfOsAbi> {
    alt((
        map(tag([0]), |_| ElfOsAbi::SystemV),
        map(tag([1]), |_| ElfOsAbi::HP_UX),
        map(tag([2]), |_| ElfOsAbi::NetBSD),
        map(tag([3]), |_| ElfOsAbi::GNU),
        map(take(1usize), |x| ElfOsAbi::Other(x[0])),
    ))(input)
}

fn parse_elf_type(input: &[u8]) -> nom::IResult<&[u8], ElfType> {
    alt((
        map(be_u16, |x| match x {
            1 => ElfType::Rel,
            2 => ElfType::Exec,
            3 => ElfType::Dyn,
            4 => ElfType::Core,
            _ => ElfType::Other(x),
        }),
    ))(input)
}

fn parse_elf_machine(input: &[u8]) -> nom::IResult<&[u8], ElfMachine> {
    alt((
        map(be_u16, |x| match x {
            0 => ElfMachine::NoMachine,
            1 => ElfMachine::M32,
            2 => ElfMachine::Sparc,
            3 => ElfMachine::X86,
            4 => ElfMachine::M68K,
            5 => ElfMachine::M88K,
            6 => ElfMachine::I370,
            7 => ElfMachine::MIPS,
            _ => ElfMachine::Other(x),
        }),
    ))(input)
}

fn parse_elf_header(input: &[u8]) -> nom::IResult<&[u8], ElfHeader> {
    context(
        "Elf Header",
        tuple((
            parse_elf_class,
            parse_elf_data,
            parse_elf_os_abi,
            take(1usize),
            parse_elf_type,
            parse_elf_machine,
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
        )),
    )(input)
    .map(|(input, (class, data, os_abi, abi_version, type_, machine, version, entry_point, program_header_offset, section_header_offset, flags, header_size, program_header_entry_size, program_header_count, section_header_entry_size, section_header_count, string_table_index))| {
        (
            input,
            ElfHeader {
                class,
                data,
                os_abi,
                abi_version: abi_version[0],
                type_,
                machine,
                version,
                entry_point,
                program_header_offset,
                section_header_offset,
                flags,
                header_size,
                program_header_entry_size,
                program_header_count,
                section_header_entry_size,
                section_header_count,
                string_table_index,
            },
        )
    })
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <elf_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Could not open file");
    let mut contents = Vec::new();
    file.read_to_end(&mut contents).expect("Could not read file");

    let result = parse_elf_header(&contents);
    match result {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => println!("Error parsing ELF header: {:?}", e),
    }
}