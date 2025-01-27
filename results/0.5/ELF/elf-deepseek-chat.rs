use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u64, u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::Read,
};

#[derive(Debug)]
struct ElfHeader {
    magic: [u8; 4],
    class: u8,
    data: u8,
    version: u8,
    os_abi: u8,
    abi_version: u8,
    pad: [u8; 7],
    e_type: u16,
    machine: u16,
    version2: u32,
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

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, (magic, class, data, version, os_abi, abi_version, pad, e_type, machine, version2, entry, phoff, shoff, flags, ehsize, phentsize, phnum, shentsize, shnum, shstrndx)) = tuple((
        tag(b"\x7FELF"),
        u8,
        u8,
        u8,
        u8,
        u8,
        take(7usize),
        le_u16,
        le_u16,
        le_u32,
        le_u64,
        le_u64,
        le_u64,
        le_u32,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
    ))(input)?;

    Ok((
        input,
        ElfHeader {
            magic: [magic[0], magic[1], magic[2], magic[3]],
            class,
            data,
            version,
            os_abi,
            abi_version,
            pad: *array_ref!(pad, 0, 7),
            e_type,
            machine,
            version2,
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