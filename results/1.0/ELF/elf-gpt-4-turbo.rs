use nom::{
    number::complete::{le_u32, le_u16, le_u64, le_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

const EI_NIDENT: usize = 16;

#[derive(Debug)]
struct ElfHeader {
    e_ident: [u8; EI_NIDENT],
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
    let (input, e_ident) = nom::bytes::complete::take(EI_NIDENT)(input)?;
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

    let header = ElfHeader {
        e_ident: {
            let mut ident = [0u8; EI_NIDENT];
            ident.copy_from_slice(e_ident);
            ident
        },
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
    };
    Ok((input, header))
}

fn read_elf_file(path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <ELF file path>", args[0]);
        std::process::exit(1);
    }

    let path = &args[1];
    match read_elf_file(path) {
        Ok(bytes) => match parse_elf_header(&bytes) {
            Ok((_, header)) => {
                println!("{:#?}", header);
            }
            Err(e) => {
                eprintln!("Failed to parse ELF header: {:?}", e);
                std::process::exit(1);
            }
        },
        Err(e) => {
            eprintln!("Failed to read file: {:?}", e);
            std::process::exit(1);
        }
    }
}