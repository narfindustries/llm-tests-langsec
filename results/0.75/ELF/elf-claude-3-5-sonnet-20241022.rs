use nom::{
    bytes::complete::take,
    number::complete::{le_u8, le_u16, le_u32, le_u64, be_u16, be_u32, be_u64},
    IResult, Parser,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ElfIdent {
    ei_mag: [u8; 4],
    ei_class: u8,
    ei_data: u8,
    ei_version: u8,
    ei_osabi: u8,
    ei_abiversion: u8,
    ei_pad: [u8; 7],
}

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

#[derive(Debug)]
struct Elf {
    header: ElfHeader,
    program_headers: Vec<ProgramHeader>,
    section_headers: Vec<SectionHeader>,
}

fn parse_elf_ident(input: &[u8]) -> IResult<&[u8], ElfIdent> {
    let (input, ei_mag) = take(4usize)(input)?;
    let (input, ei_class) = le_u8(input)?;
    let (input, ei_data) = le_u8(input)?;
    let (input, ei_version) = le_u8(input)?;
    let (input, ei_osabi) = le_u8(input)?;
    let (input, ei_abiversion) = le_u8(input)?;
    let (input, ei_pad) = take(7usize)(input)?;

    Ok((
        input,
        ElfIdent {
            ei_mag: ei_mag.try_into().unwrap(),
            ei_class,
            ei_data,
            ei_version,
            ei_osabi,
            ei_abiversion,
            ei_pad: ei_pad.try_into().unwrap(),
        },
    ))
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, e_ident) = parse_elf_ident(input)?;
    
    let (input, e_type, e_machine, e_version, e_entry, e_phoff, e_shoff, e_flags, e_ehsize,
         e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx) = match e_ident.ei_data {
        1 => {
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
            (input, e_type, e_machine, e_version, e_entry, e_phoff, e_shoff, e_flags, e_ehsize,
             e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx)
        },
        2 => {
            let (input, e_type) = be_u16(input)?;
            let (input, e_machine) = be_u16(input)?;
            let (input, e_version) = be_u32(input)?;
            let (input, e_entry) = be_u64(input)?;
            let (input, e_phoff) = be_u64(input)?;
            let (input, e_shoff) = be_u64(input)?;
            let (input, e_flags) = be_u32(input)?;
            let (input, e_ehsize) = be_u16(input)?;
            let (input, e_phentsize) = be_u16(input)?;
            let (input, e_phnum) = be_u16(input)?;
            let (input, e_shentsize) = be_u16(input)?;
            let (input, e_shnum) = be_u16(input)?;
            let (input, e_shstrndx) = be_u16(input)?;
            (input, e_type, e_machine, e_version, e_entry, e_phoff, e_shoff, e_flags, e_ehsize,
             e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx)
        },
        _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    };

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

fn parse_program_header(input: &[u8], is_little_endian: bool) -> IResult<&[u8], ProgramHeader> {
    let (input, (p_type, p_flags, p_offset, p_vaddr, p_paddr, p_filesz, p_memsz, p_align)) = 
        if is_little_endian {
            let (input, p_type) = le_u32(input)?;
            let (input, p_flags) = le_u32(input)?;
            let (input, p_offset) = le_u64(input)?;
            let (input, p_vaddr) = le_u64(input)?;
            let (input, p_paddr) = le_u64(input)?;
            let (input, p_filesz) = le_u64(input)?;
            let (input, p_memsz) = le_u64(input)?;
            let (input, p_align) = le_u64(input)?;
            (input, (p_type, p_flags, p_offset, p_vaddr, p_paddr, p_filesz, p_memsz, p_align))
        } else {
            let (input, p_type) = be_u32(input)?;
            let (input, p_flags) = be_u32(input)?;
            let (input, p_offset) = be_u64(input)?;
            let (input, p_vaddr) = be_u64(input)?;
            let (input, p_paddr) = be_u64(input)?;
            let (input, p_filesz) = be_u64(input)?;
            let (input, p_memsz) = be_u64(input)?;
            let (input, p_align) = be_u64(input)?;
            (input, (p_type, p_flags, p_offset, p_vaddr, p_paddr, p_filesz, p_memsz, p_align))
        };

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

fn parse_section_header(input: &[u8], is_little_endian: bool) -> IResult<&[u8], SectionHeader> {
    let (input, (sh_name, sh_type, sh_flags, sh_addr, sh_offset, sh_size, sh_link, sh_info, 
                sh_addralign, sh_entsize)) = if is_little_endian {
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
        (input, (sh_name, sh_type, sh_flags, sh_addr, sh_offset, sh_size, sh_link, sh_info,
                sh_addralign, sh_entsize))
    } else {
        let (input, sh_name) = be_u32(input)?;
        let (input, sh_type) = be_u32(input)?;
        let (input, sh_flags) = be_u64(input)?;
        let (input, sh_addr) = be_u64(input)?;
        let (input, sh_offset) = be_u64(input)?;
        let (input, sh_size) = be_u64(input)?;
        let (input, sh_link) = be_u32(input)?;
        let (input, sh_info) = be_u32(input)?;
        let (input, sh_addralign) = be_u64(input)?;
        let (input, sh_entsize) = be_u64(input)?;
        (input, (sh_name, sh_type, sh_flags, sh_addr, sh_offset, sh_size, sh_link, sh_info,
                sh_addralign, sh_entsize))
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

fn parse_elf(input: &[u8]) -> IResult<&[u8], Elf> {
    let (input, header) = parse_elf_header(input)?;
    let is_little_endian = header.e_ident.ei_data == 1;

    let mut program_headers = Vec::new();
    let mut remaining = &input[header.e_phoff as usize..];
    for _ in 0..header.e_phnum {
        let (next_input, ph) = parse_program_header(remaining, is_little_endian)?;
        program_headers.push(ph);
        remaining = next_input;
    }

    let mut section_headers = Vec::new();
    let mut remaining = &input[header.e_shoff as usize..];
    for _ in 0..header.e_shnum {
        let (next_input, sh) = parse_section_header(remaining, is_little_endian)?;
        section_headers.push(sh);
        remaining = next_input;
    }

    Ok((
        input,
        Elf {
            header,
            program_headers,
            section_headers,
        },
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <elf-file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_elf(&buffer) {
        Ok((_, elf)) => println!("{:#?}", elf),
        Err(e) => eprintln!("Failed to parse ELF file: {:?}", e),
    }

    Ok(())
}