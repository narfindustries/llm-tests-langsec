use nom::{
    bytes::complete::take,
    number::complete::{le_u16, le_u32, le_u64, u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ElfIdent {
    magic: [u8; 4],
    class: u8,
    data: u8,
    version: u8,
    osabi: u8,
    abiversion: u8,
    pad: [u8; 7],
}

#[derive(Debug)]
struct ElfHeader {
    ident: ElfIdent,
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

fn parse_elf_ident(input: &[u8]) -> IResult<&[u8], ElfIdent> {
    let (input, magic) = take(4usize)(input)?;
    let (input, class) = u8(input)?;
    let (input, data) = u8(input)?;
    let (input, version) = u8(input)?;
    let (input, osabi) = u8(input)?;
    let (input, abiversion) = u8(input)?;
    let (input, pad) = take(7usize)(input)?;

    Ok((
        input,
        ElfIdent {
            magic: [magic[0], magic[1], magic[2], magic[3]],
            class,
            data,
            version,
            osabi,
            abiversion,
            pad: [pad[0], pad[1], pad[2], pad[3], pad[4], pad[5], pad[6]],
        },
    ))
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, ident) = parse_elf_ident(input)?;
    let (input, e_type) = le_u16(input)?;
    let (input, e_machine) = le_u16(input)?;
    let (input, e_version) = le_u32(input)?;
    let (input, e_entry) = if ident.class == 1 {
        let (input, entry) = le_u32(input)?;
        (input, entry as u64)
    } else {
        le_u64(input)?
    };
    let (input, e_phoff) = if ident.class == 1 {
        let (input, phoff) = le_u32(input)?;
        (input, phoff as u64)
    } else {
        le_u64(input)?
    };
    let (input, e_shoff) = if ident.class == 1 {
        let (input, shoff) = le_u32(input)?;
        (input, shoff as u64)
    } else {
        le_u64(input)?
    };
    let (input, e_flags) = le_u32(input)?;
    let (input, e_ehsize) = le_u16(input)?;
    let (input, e_phentsize) = le_u16(input)?;
    let (input, e_phnum) = le_u16(input)?;
    let (input, e_shentsize) = le_u16(input)?;
    let (input, e_shnum) = le_u16(input)?;
    let (input, e_shstrndx) = le_u16(input)?;

    Ok((
        input,
        ElfHeader {
            ident,
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
    if args.len() < 2 {
        eprintln!("Usage: {} <elf_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_elf_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Failed to parse ELF header: {:?}", e),
    }
}