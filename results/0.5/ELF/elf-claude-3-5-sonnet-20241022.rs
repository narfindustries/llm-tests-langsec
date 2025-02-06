use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u8, le_u16, le_u32, le_u64},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ElfIdent {
    ei_magic: [u8; 4],
    ei_class: u8,
    ei_data: u8,
    ei_version: u8,
    ei_osabi: u8,
    ei_abiversion: u8,
    ei_pad: [u8; 7],
}

#[derive(Debug)]
struct ElfHeader64 {
    e_ident: ElfIdent,
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
struct ProgramHeader64 {
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
struct SectionHeader64 {
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

fn parse_elf_ident(input: &[u8]) -> IResult<&[u8], ElfIdent> {
    let (input, magic) = take(4usize)(input)?;
    let (input, class) = le_u8(input)?;
    let (input, data) = le_u8(input)?;
    let (input, version) = le_u8(input)?;
    let (input, osabi) = le_u8(input)?;
    let (input, abiversion) = le_u8(input)?;
    let (input, pad) = take(7usize)(input)?;

    Ok((
        input,
        ElfIdent {
            ei_magic: magic.try_into().unwrap(),
            ei_class: class,
            ei_data: data,
            ei_version: version,
            ei_osabi: osabi,
            ei_abiversion: abiversion,
            ei_pad: pad.try_into().unwrap(),
        },
    ))
}

fn parse_elf_header64(input: &[u8]) -> IResult<&[u8], ElfHeader64> {
    let (input, e_ident) = parse_elf_ident(input)?;
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
        ElfHeader64 {
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

fn parse_program_header64(input: &[u8]) -> IResult<&[u8], ProgramHeader64> {
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
        ProgramHeader64 {
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

fn parse_section_header64(input: &[u8]) -> IResult<&[u8], SectionHeader64> {
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
        SectionHeader64 {
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

#[derive(Debug)]
struct Elf64 {
    header: ElfHeader64,
    program_headers: Vec<ProgramHeader64>,
    section_headers: Vec<SectionHeader64>,
}

fn parse_elf64(input: &[u8]) -> IResult<&[u8], Elf64> {
    let (input, header) = parse_elf_header64(input)?;
    
    let mut program_headers = Vec::new();
    let mut remaining = &input[header.e_phoff as usize..];
    for _ in 0..header.e_phnum {
        let (new_input, ph) = parse_program_header64(remaining)?;
        program_headers.push(ph);
        remaining = new_input;
    }

    let mut section_headers = Vec::new();
    let mut remaining = &input[header.e_shoff as usize..];
    for _ in 0..header.e_shnum {
        let (new_input, sh) = parse_section_header64(remaining)?;
        section_headers.push(sh);
        remaining = new_input;
    }

    Ok((
        input,
        Elf64 {
            header,
            program_headers,
            section_headers,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <elf-file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_elf64(&buffer) {
        Ok((_, elf)) => println!("{:#?}", elf),
        Err(e) => eprintln!("Failed to parse ELF file: {:?}", e),
    }
}