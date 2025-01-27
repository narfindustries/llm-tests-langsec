use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    error::Error,
    multi::count,
    number::complete::{be_u16, be_u32, be_u64, le_u16, le_u32, le_u64, u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

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
    ei_mag: [u8; 4],
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
    let (input, ei_mag) = verify(take(4usize), |b: &[u8]| b == b"\x7FELF")(input)?;
    let (input, ei_class) = u8(input)?;
    let (input, ei_data) = u8(input)?;
    let (input, ei_version) = u8(input)?;
    let (input, ei_osabi) = u8(input)?;
    let (input, ei_abiversion) = u8(input)?;
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
    let endianness = e_ident.ei_data;
    
    let (input, (e_type, e_machine, e_version, e_entry, e_phoff, e_shoff, e_flags,
                 e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx)) = 
    if endianness == 1 {
        tuple((
            le_u16, le_u16, le_u32, le_u64, le_u64, le_u64, le_u32,
            le_u16, le_u16, le_u16, le_u16, le_u16, le_u16,
        ))(input)?
    } else {
        tuple((
            be_u16, be_u16, be_u32, be_u64, be_u64, be_u64, be_u32,
            be_u16, be_u16, be_u16, be_u16, be_u16, be_u16,
        ))(input)?
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

fn parse_program_header(input: &[u8], endianness: u8) -> IResult<&[u8], ProgramHeader> {
    if endianness == 1 {
        let (input, (p_type, p_flags, p_offset, p_vaddr, p_paddr, p_filesz, p_memsz, p_align)) =
            tuple((
                le_u32, le_u32, le_u64, le_u64, le_u64, le_u64, le_u64, le_u64,
            ))(input)?;
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
    } else {
        let (input, (p_type, p_flags, p_offset, p_vaddr, p_paddr, p_filesz, p_memsz, p_align)) =
            tuple((
                be_u32, be_u32, be_u64, be_u64, be_u64, be_u64, be_u64, be_u64,
            ))(input)?;
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
}

fn parse_section_header(input: &[u8], endianness: u8) -> IResult<&[u8], SectionHeader> {
    if endianness == 1 {
        let (input, (sh_name, sh_type, sh_flags, sh_addr, sh_offset, sh_size, sh_link, sh_info, sh_addralign, sh_entsize)) =
            tuple((
                le_u32, le_u32, le_u64, le_u64, le_u64, le_u64, le_u32, le_u32, le_u64, le_u64,
            ))(input)?;
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
    } else {
        let (input, (sh_name, sh_type, sh_flags, sh_addr, sh_offset, sh_size, sh_link, sh_info, sh_addralign, sh_entsize)) =
            tuple((
                be_u32, be_u32, be_u64, be_u64, be_u64, be_u64, be_u32, be_u32, be_u64, be_u64,
            ))(input)?;
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
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <elf_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let (input, elf_header) = parse_elf_header(&buffer)?;
    println!("ELF Header: {:#?}", elf_header);

    // Parse program headers
    if elf_header.e_phnum > 0 {
        let ph_offset = elf_header.e_phoff as usize;
        let mut current_input = &buffer[ph_offset..];
        for i in 0..elf_header.e_phnum {
            let (next_input, program_header) = parse_program_header(current_input, elf_header.e_ident.ei_data)?;
            println!("Program Header {}: {:#?}", i, program_header);
            current_input = next_input;
        }
    }

    // Parse section headers
    if elf_header.e_shnum > 0 {
        let sh_offset = elf_header.e_shoff as usize;
        let mut current_input = &buffer[sh_offset..];
        for i in 0..elf_header.e_shnum {
            let (next_input, section_header) = parse_section_header(current_input, elf_header.e_ident.ei_data)?;
            println!("Section Header {}: {:#?}", i, section_header);
            current_input = next_input;
        }
    }

    Ok(())
}