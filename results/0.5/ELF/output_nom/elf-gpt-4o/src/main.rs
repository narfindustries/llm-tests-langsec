use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::count,
    number::complete::{le_u16, le_u32, le_u64, le_u8},
    IResult,
};
use std::fs;
use std::io::Read;
use std::path::Path;
use std::env;

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

#[derive(Debug)]
struct ProgramHeader {
    p_type: u32,
    p_flags: u32,
    p_offset: u64,
    p_vaddr: u64,
    p_paddr: u64,
    p_filesz: u64,
    p_memsz: u64,
    p_align: u64,
}

#[derive(Debug)]
struct SectionHeader {
    sh_name: u32,
    sh_type: u32,
    sh_flags: u64,
    sh_addr: u64,
    sh_offset: u64,
    sh_size: u64,
    sh_link: u32,
    sh_info: u32,
    sh_addralign: u64,
    sh_entsize: u64,
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, e_ident) = map(take(16usize), |bytes: &[u8]| {
        let mut array = [0u8; 16];
        array.copy_from_slice(bytes);
        array
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

fn parse_program_header(input: &[u8]) -> IResult<&[u8], ProgramHeader> {
    let (input, p_type) = le_u32(input)?;
    let (input, p_flags) = le_u32(input)?;
    let (input, p_offset) = le_u64(input)?;
    let (input, p_vaddr) = le_u64(input)?;
    let (input, p_paddr) = le_u64(input)?;
    let (input, p_filesz) = le_u64(input)?;
    let (input, p_memsz) = le_u64(input)?;
    let (input, p_align) = le_u64(input)?;

    Ok((
        input,
        ProgramHeader {
            p_type,
            p_flags,
            p_offset,
            p_vaddr,
            p_paddr,
            p_filesz,
            p_memsz,
            p_align,
        },
    ))
}

fn parse_section_header(input: &[u8]) -> IResult<&[u8], SectionHeader> {
    let (input, sh_name) = le_u32(input)?;
    let (input, sh_type) = le_u32(input)?;
    let (input, sh_flags) = le_u64(input)?;
    let (input, sh_addr) = le_u64(input)?;
    let (input, sh_offset) = le_u64(input)?;
    let (input, sh_size) = le_u64(input)?;
    let (input, sh_link) = le_u32(input)?;
    let (input, sh_info) = le_u32(input)?;
    let (input, sh_addralign) = le_u64(input)?;
    let (input, sh_entsize) = le_u64(input)?;

    Ok((
        input,
        SectionHeader {
            sh_name,
            sh_type,
            sh_flags,
            sh_addr,
            sh_offset,
            sh_size,
            sh_link,
            sh_info,
            sh_addralign,
            sh_entsize,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ELF file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = fs::File::open(&path).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    let (_, elf_header) = parse_elf_header(&buffer).expect("Failed to parse ELF header");
    println!("{:?}", elf_header);

    let ph_offset = elf_header.e_phoff as usize;
    let ph_entry_size = elf_header.e_phentsize as usize;
    let ph_num = elf_header.e_phnum as usize;
    let mut ph_table = &buffer[ph_offset..];
    for _ in 0..ph_num {
        let (_, program_header) = parse_program_header(ph_table).expect("Failed to parse program header");
        println!("{:?}", program_header);
        ph_table = &ph_table[ph_entry_size..];
    }

    let sh_offset = elf_header.e_shoff as usize;
    let sh_entry_size = elf_header.e_shentsize as usize;
    let sh_num = elf_header.e_shnum as usize;
    let mut sh_table = &buffer[sh_offset..];
    for _ in 0..sh_num {
        let (_, section_header) = parse_section_header(sh_table).expect("Failed to parse section header");
        println!("{:?}", section_header);
        sh_table = &sh_table[sh_entry_size..];
    }
}