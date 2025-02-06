use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u32, be_u64},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum ElfClass {
    Elf32,
    Elf64,
}

#[derive(Debug)]
enum ElfData {
    ElfData2LSB,
    ElfData2MSB,
}

#[derive(Debug)]
enum ElfType {
    EtNone,
    EtRel,
    EtExec,
    EtDyn,
    EtCore,
}

#[derive(Debug)]
struct ElfHeader {
    e_ident: [u8; 16],
    e_type: ElfType,
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

fn elf_class(input: &[u8]) -> IResult<&[u8], ElfClass> {
    let (input, class) = take(1usize)(input)?;
    match class[0] {
        1 => Ok((input, ElfClass::Elf32)),
        2 => Ok((input, ElfClass::Elf64)),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn elf_data(input: &[u8]) -> IResult<&[u8], ElfData> {
    let (input, data) = take(1usize)(input)?;
    match data[0] {
        1 => Ok((input, ElfData::ElfData2LSB)),
        2 => Ok((input, ElfData::ElfData2MSB)),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn elf_type(input: &[u8]) -> IResult<&[u8], ElfType> {
    let (input, type_val) = be_u16(input)?;
    match type_val {
        0 => Ok((input, ElfType::EtNone)),
        1 => Ok((input, ElfType::EtRel)),
        2 => Ok((input, ElfType::EtExec)),
        3 => Ok((input, ElfType::EtDyn)),
        4 => Ok((input, ElfType::EtCore)),
        _ => Ok((input, ElfType::EtNone)),
    }
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, e_ident) = take(16usize)(input)?;
    let e_ident_arr = e_ident.try_into().unwrap(); //Corrected this line
    let (input, e_type) = elf_type(input)?;
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
        ElfHeader {
            e_ident: e_ident_arr,
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

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: elf_parser <elf_file>");
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_elf_header(&buffer) {
        Ok((_, header)) => println!("ELF Header: {:?}", header),
        Err(e) => println!("Error parsing ELF header: {:?}", e),
    }
}
