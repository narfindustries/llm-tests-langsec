use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bits,
    bytes::complete::{tag, take},
    error::Error,
    number::complete::{be_u16, be_u32, be_u64, le_u16, le_u32, le_u64},
    sequence::tuple,
    IResult,
};

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
    let (input, ei_magic) = take(4usize)(input)?;
    let (input, ei_class) = take(1usize)(input)?;
    let (input, ei_data) = take(1usize)(input)?;
    let (input, ei_version) = take(1usize)(input)?;
    let (input, ei_osabi) = take(1usize)(input)?;
    let (input, ei_abiversion) = take(1usize)(input)?;
    let (input, ei_pad) = take(7usize)(input)?;

    Ok((
        input,
        ElfIdent {
            ei_magic: ei_magic.try_into().unwrap(),
            ei_class: ei_class[0],
            ei_data: ei_data[0],
            ei_version: ei_version[0],
            ei_osabi: ei_osabi[0],
            ei_abiversion: ei_abiversion[0],
            ei_pad: ei_pad.try_into().unwrap(),
        },
    ))
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, e_ident) = parse_elf_ident(input)?;
    let is_le = e_ident.ei_data == 1;
    
    let (input, (e_type, e_machine)) = if is_le {
        tuple((le_u16, le_u16))(input)?
    } else {
        tuple((be_u16, be_u16))(input)?
    };

    let (input, e_version) = if is_le { le_u32(input)? } else { be_u32(input)? };
    let (input, e_entry) = if is_le { le_u64(input)? } else { be_u64(input)? };
    let (input, e_phoff) = if is_le { le_u64(input)? } else { be_u64(input)? };
    let (input, e_shoff) = if is_le { le_u64(input)? } else { be_u64(input)? };
    let (input, e_flags) = if is_le { le_u32(input)? } else { be_u32(input)? };
    let (input, e_ehsize) = if is_le { le_u16(input)? } else { be_u16(input)? };
    let (input, e_phentsize) = if is_le { le_u16(input)? } else { be_u16(input)? };
    let (input, e_phnum) = if is_le { le_u16(input)? } else { be_u16(input)? };
    let (input, e_shentsize) = if is_le { le_u16(input)? } else { be_u16(input)? };
    let (input, e_shnum) = if is_le { le_u16(input)? } else { be_u16(input)? };
    let (input, e_shstrndx) = if is_le { le_u16(input)? } else { be_u16(input)? };

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

fn parse_program_header(input: &[u8], is_le: bool) -> IResult<&[u8], ProgramHeader> {
    let (input, p_type) = if is_le { le_u32(input)? } else { be_u32(input)? };
    let (input, p_flags) = if is_le { le_u32(input)? } else { be_u32(input)? };
    let (input, p_offset) = if is_le { le_u64(input)? } else { be_u64(input)? };
    let (input, p_vaddr) = if is_le { le_u64(input)? } else { be_u64(input)? };
    let (input, p_paddr) = if is_le { le_u64(input)? } else { be_u64(input)? };
    let (input, p_filesz) = if is_le { le_u64(input)? } else { be_u64(input)? };
    let (input, p_memsz) = if is_le { le_u64(input)? } else { be_u64(input)? };
    let (input, p_align) = if is_le { le_u64(input)? } else { be_u64(input)? };

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

fn parse_section_header(input: &[u8], is_le: bool) -> IResult<&[u8], SectionHeader> {
    let (input, sh_name) = if is_le { le_u32(input)? } else { be_u32(input)? };
    let (input, sh_type) = if is_le { le_u32(input)? } else { be_u32(input)? };
    let (input, sh_flags) = if is_le { le_u64(input)? } else { be_u64(input)? };
    let (input, sh_addr) = if is_le { le_u64(input)? } else { be_u64(input)? };
    let (input, sh_offset) = if is_le { le_u64(input)? } else { be_u64(input)? };
    let (input, sh_size) = if is_le { le_u64(input)? } else { be_u64(input)? };
    let (input, sh_link) = if is_le { le_u32(input)? } else { be_u32(input)? };
    let (input, sh_info) = if is_le { le_u32(input)? } else { be_u32(input)? };
    let (input, sh_addralign) = if is_le { le_u64(input)? } else { be_u64(input)? };
    let (input, sh_entsize) = if is_le { le_u64(input)? } else { be_u64(input)? };

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
        eprintln!("Usage: {} <elf_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_elf_header(&buffer) {
        Ok((remaining, header)) => {
            println!("ELF Header: {:#?}", header);
            
            let is_le = header.e_ident.ei_data == 1;
            
            // Parse program headers
            if header.e_phnum > 0 {
                let ph_offset = header.e_phoff as usize;
                let mut current_offset = &buffer[ph_offset..];
                
                for i in 0..header.e_phnum {
                    match parse_program_header(current_offset, is_le) {
                        Ok((remaining, ph)) => {
                            println!("Program Header {}: {:#?}", i, ph);
                            current_offset = remaining;
                        }
                        Err(e) => eprintln!("Failed to parse program header: {:?}", e),
                    }
                }
            }

            // Parse section headers
            if header.e_shnum > 0 {
                let sh_offset = header.e_shoff as usize;
                let mut current_offset = &buffer[sh_offset..];
                
                for i in 0..header.e_shnum {
                    match parse_section_header(current_offset, is_le) {
                        Ok((remaining, sh)) => {
                            println!("Section Header {}: {:#?}", i, sh);
                            current_offset = remaining;
                        }
                        Err(e) => eprintln!("Failed to parse section header: {:?}", e),
                    }
                }
            }
        }
        Err(e) => eprintln!("Failed to parse ELF header: {:?}", e),
    }
}