use nom::{
    be_u16, be_u32, be_u64, bytes::complete::take, combinator::map, IResult,
    multi::count, number::complete::le_u16,
};
use std::env;
use std::fs::File;
use std::io::Read;

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


fn elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, e_ident) = take(16usize)(input)?;
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

fn program_header(input: &[u8]) -> IResult<&[u8], ProgramHeader> {
    let (input, p_type) = be_u32(input)?;
    let (input, p_flags) = be_u32(input)?;
    let (input, p_offset) = be_u64(input)?;
    let (input, p_vaddr) = be_u64(input)?;
    let (input, p_paddr) = be_u64(input)?;
    let (input, p_filesz) = be_u64(input)?;
    let (input, p_memsz) = be_u64(input)?;
    let (input, p_align) = be_u64(input)?;
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

fn section_header(input: &[u8]) -> IResult<&[u8], SectionHeader> {
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
        println!("Usage: elfparser <filename>");
        return;
    }
    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match elf_header(&buffer) {
        Ok((remaining, header)) => {
            println!("ELF Header: {:?}", header);
            let program_headers = if header.e_phnum > 0 {
                let program_header_offset = header.e_phoff as usize;
                let program_header_data = &buffer[program_header_offset..];
                let program_headers_result: IResult<&[u8], Vec<ProgramHeader>> = count(
                    program_header,
                    header.e_phnum as usize,
                )(program_header_data);
                match program_headers_result {
                    Ok((_, headers)) => headers,
                    Err(e) => {
                        println!("Error parsing program headers: {:?}", e);
                        return;
                    }
                }

            } else {
                Vec::new()
            };
            println!("Program Headers: {:?}", program_headers);

            let section_headers = if header.e_shnum > 0 {
                let section_header_offset = header.e_shoff as usize;
                let section_header_data = &buffer[section_header_offset..];
                let section_headers_result: IResult<&[u8], Vec<SectionHeader>> = count(
                    section_header,
                    header.e_shnum as usize,
                )(section_header_data);
                match section_headers_result {
                    Ok((_, headers)) => headers,
                    Err(e) => {
                        println!("Error parsing section headers: {:?}", e);
                        return;
                    }
                }
            } else {
                Vec::new()
            };
            println!("Section Headers: {:?}", section_headers);

        }
        Err(e) => println!("Error parsing ELF header: {:?}", e),
    }
}
