use nom::{
    bytes::complete::take,
    number::complete::{le_u16, le_u32, le_u64, u8},
    IResult,
};
use std::env;
use std::fs;

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
    if args.len() < 2 {
        eprintln!("Usage: {} <elf_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    let (_, elf_header) = parse_elf_header(&data).expect("Failed to parse ELF header");
    println!("{:#?}", elf_header);

    let mut offset = elf_header.e_phoff as usize;
    for _ in 0..elf_header.e_phnum {
        let (_, ph) = parse_program_header(&data[offset..]).expect("Failed to parse program header");
        println!("{:#?}", ph);
        offset += elf_header.e_phentsize as usize;
    }

    offset = elf_header.e_shoff as usize;
    for _ in 0..elf_header.e_shnum {
        let (_, sh) = parse_section_header(&data[offset..]).expect("Failed to parse section header");
        println!("{:#?}", sh);
        offset += elf_header.e_shentsize as usize;
    }
}