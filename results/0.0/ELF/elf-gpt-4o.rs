use nom::{
    bytes::complete::take,
    combinator::map,
    number::complete::{le_u16, le_u32, le_u64, le_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct ElfHeader {
    e_ident: [u8; 16],
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

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, e_ident) = map(take(16usize), |bytes: &[u8]| {
        let mut arr = [0u8; 16];
        arr.copy_from_slice(bytes);
        arr
    })(input)?;
    let (input, e_type) = le_u16(input)?;
    let (input, e_machine) = le_u16(input)?;
    let (input, e_version) = le_u32(input)?;
    let (input, e_entry) = le_u64(input)?;
    let (input, e_phoff) = le_u64(input)?;
    let (input, e_shoff) = le_u64(input)?;
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
            e_ident,
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
        eprintln!("Usage: {} <elf-file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    if let Err(err) = file.read_to_end(&mut buffer) {
        eprintln!("Error reading file: {}", err);
        return;
    }

    match parse_elf_header(&buffer) {
        Ok((_, elf_header)) => {
            println!("{:?}", elf_header);
        }
        Err(err) => {
            eprintln!("Error parsing ELF header: {:?}", err);
        }
    }
}