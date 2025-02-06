use nom::{
    bytes::complete::tag,
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
    p_offset: u64,
    p_vaddr: u64,
    p_paddr: u64,
    p_filesz: u64,
    p_memsz: u64,
    p_flags: u32,
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
    let (input, magic) = tag(&[0x7F, 0x45, 0x4C, 0x46])(input)?;
    let (input, class) = u8(input)?;
    let (input, data) = u8(input)?;
    let (input, version) = u8(input)?;
    let (input, osabi) = u8(input)?;
    let (input, abiversion) = u8(input)?;
    let (input, pad) = tag(&[0, 0, 0, 0, 0, 0, 0])(input)?;
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
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
    let (input, e_phoff) = if ident.class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
    let (input, e_shoff) = if ident.class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
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

fn parse_program_header(input: &[u8], class: u8) -> IResult<&[u8], ProgramHeader> {
    let (input, p_type) = le_u32(input)?;
    let (input, p_offset) = if class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
    let (input, p_vaddr) = if class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
    let (input, p_paddr) = if class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
    let (input, p_filesz) = if class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
    let (input, p_memsz) = if class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
    let (input, p_flags) = le_u32(input)?;
    let (input, p_align) = if class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
    Ok((
        input,
        ProgramHeader {
            p_type,
            p_offset,
            p_vaddr,
            p_paddr,
            p_filesz,
            p_memsz,
            p_flags,
            p_align,
        },
    ))
}

fn parse_section_header(input: &[u8], class: u8) -> IResult<&[u8], SectionHeader> {
    let (input, sh_name) = le_u32(input)?;
    let (input, sh_type) = le_u32(input)?;
    let (input, sh_flags) = if class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
    let (input, sh_addr) = if class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
    let (input, sh_offset) = if class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
    let (input, sh_size) = if class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
    let (input, sh_link) = le_u32(input)?;
    let (input, sh_info) = le_u32(input)?;
    let (input, sh_addralign) = if class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
    let (input, sh_entsize) = if class == 1 {
        le_u32(input).map(|(i, v)| (i, v as u64))?
    } else {
        le_u64(input)?
    };
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
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    let (_, elf_header) = parse_elf_header(&data).expect("Failed to parse ELF header");
    println!("{:#?