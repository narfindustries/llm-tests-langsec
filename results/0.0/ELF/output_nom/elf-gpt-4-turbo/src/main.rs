use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u64},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

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
    e_version: u32,
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
    let (input, magic) = tag(b"\x7FELF")(input)?;
    let (input, class) = take(1u8)(input)?;
    let (input, data) = take(1u8)(input)?;
    let (input, version) = take(1u8)(input)?;
    let (input, os_abi) = take(1u8)(input)?;
    let (input, abi_version) = take(1u8)(input)?;
    let (input, pad) = take(7u8)(input)?;
    let (input, e_type) = le_u16(input)?;
    let (input, machine) = le_u16(input)?;
    let (input, e_version) = le_u32(input)?;
    let (input, entry) = le_u64(input)?;
    let (input, phoff) = le_u64(input)?;
    let (input, shoff) = le_u64(input)?;
    let (input, flags) = le_u32(input)?;
    let (input, ehsize) = le_u16(input)?;
    let (input, phentsize) = le_u16(input)?;
    let (input, phnum) = le_u16(input)?;
    let (input, shentsize) = le_u16(input)?;
    let (input, shnum) = le_u16(input)?;
    let (input, shstrndx) = le_u16(input)?;

    Ok((
        input,
        ElfHeader {
            magic: [magic[0], magic[1], magic[2], magic[3]],
            class: class[0],
            data: data[0],
            version: version[0],
            os_abi: os_abi[0],
            abi_version: abi_version[0],
            pad: [
                pad[0], pad[1], pad[2], pad[3], pad[4], pad[5], pad[6],
            ],
            e_type,
            machine,
            e_version,
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

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_elf_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Failed to parse ELF header: {:?}", e),
    }

    Ok(())
}