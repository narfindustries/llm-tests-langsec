use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    error::Error,
    number::complete::{be_u16, be_u32, be_u64, le_u16, le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::{env, fs, path::Path};

#[derive(Debug)]
struct ElfHeader {
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
    let (input, (
        ei_magic,
        ei_class,
        ei_data,
        ei_version,
        ei_osabi,
        ei_abiversion,
        ei_pad,
    )) = tuple((
        take(4usize),
        take(1usize),
        take(1usize),
        take(1usize),
        take(1usize),
        take(1usize),
        take(7usize),
    ))(input)?;

    Ok((input, ElfIdent {
        ei_magic: ei_magic.try_into().unwrap(),
        ei_class: ei_class[0],
        ei_data: ei_data[0],
        ei_version: ei_version[0],
        ei_osabi: ei_osabi[0],
        ei_abiversion: ei_abiversion[0],
        ei_pad: ei_pad.try_into().unwrap(),
    }))
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, ident) = parse_elf_ident(input)?;
    
    let parser = if ident.ei_data == 1 {
        // ELFDATA2LSB
        tuple((
            le_u16, le_u16, le_u32, le_u64, le_u64,
            le_u64, le_u32, le_u16, le_u16, le_u16,
            le_u16, le_u16, le_u16,
        ))
    } else {
        // ELFDATA2MSB
        tuple((
            be_u16, be_u16, be_u32, be_u64, be_u64,
            be_u64, be_u32, be_u16, be_u16, be_u16,
            be_u16, be_u16, be_u16,
        ))
    };

    let (input, (
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
    )) = parser(input)?;

    Ok((input, ElfHeader {
        e_ident: ident,
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
    }))
}

fn parse_program_header(input: &[u8], is_little_endian: bool) -> IResult<&[u8], ProgramHeader> {
    let parser = if is_little_endian {
        tuple((
            le_u32, le_u32, le_u64, le_u64,
            le_u64, le_u64, le_u64, le_u64,
        ))
    } else {
        tuple((
            be_u32, be_u32, be_u64, be_u64,
            be_u64, be_u64, be_u64, be_u64,
        ))
    };

    let (input, (
        p_type,
        p_flags,
        p_offset,
        p_vaddr,
        p_paddr,
        p_filesz,
        p_memsz,
        p_align,
    )) = parser(input)?;

    Ok((input, ProgramHeader {
        p_type,
        p_flags,
        p_offset,
        p_vaddr,
        p_paddr,
        p_filesz,
        p_memsz,
        p_align,
    }))
}

fn parse_section_header(input: &[u8], is_little_endian: bool) -> IResult<&[u8], SectionHeader> {
    let parser = if is_little_endian {
        tuple((
            le_u32, le_u32, le_u64, le_u64,
            le_u64, le_u64, le_u32, le_u32,
            le_u64, le_u64,
        ))
    } else {
        tuple((
            be_u32, be_u32, be_u64, be_u64,
            be_u64, be_u64, be_u32, be_u32,
            be_u64, be_u64,
        ))
    };

    let (input, (
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
    )) = parser(input)?;

    Ok((input, SectionHeader {
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
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <elf_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let data = fs::read(path)?;

    match parse_elf_header(&data) {
        Ok((remaining, header)) => {
            println!("ELF Header: {:?}", header);

            let is_little_endian = header.e_ident.ei_data == 1;

            // Parse program headers
            let mut offset = header.e_phoff as usize;
            for i in 0..header.e_phnum {
                if let Ok((_, ph)) = parse_program_header(&data[offset..], is_little_endian) {
                    println!("Program Header {}: {:?}", i, ph);
                }
                offset += header.e_phentsize as usize;
            }

            // Parse section headers
            let mut offset = header.e_shoff as usize;
            for i in 0..header.e_shnum {
                if let Ok((_, sh)) = parse_section_header(&data[offset..], is_little_endian) {
                    println!("Section Header {}: {:?}", i, sh);
                }
                offset += header.e_shentsize as usize;
            }
        }
        Err(e) => eprintln!("Failed to parse ELF header: {:?}", e),
    }

    Ok(())
}