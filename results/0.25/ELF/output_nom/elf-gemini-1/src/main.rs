use nom::{
    be_u16, be_u32, be_u64, bytes::complete::take, combinator::map, IResult,
    number::complete::le_u32, sequence::tuple,
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
    let (input, (p_type, p_flags, p_offset, p_vaddr, p_paddr, p_filesz, p_memsz, p_align)) = tuple((
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
}

fn section_header(input: &[u8]) -> IResult<&[u8], SectionHeader> {
    let (input, (sh_name, sh_type, sh_flags, sh_addr, sh_offset, sh_size, sh_link, sh_info, sh_addralign, sh_entsize)) = tuple((
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
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: elf_parser <elf_file>");
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match elf_header(&buffer) {
        Ok((remaining, header)) => {
            println!("ELF Header: {:?}", header);
            let program_headers_offset = header.e_phoff as usize;
            let program_headers_size = header.e_phentsize as usize * header.e_phnum as usize;
            let program_headers_data = &buffer[program_headers_offset..program_headers_offset + program_headers_size];

            for i in 0..header.e_phnum {
                let offset = (i as usize) * header.e_phentsize as usize;
                let ph = program_header(&program_headers_data[offset..]).unwrap().1;
                println!("Program Header {}: {:?}", i, ph);
            }

            let section_headers_offset = header.e_shoff as usize;
            let section_headers_size = header.e_shentsize as usize * header.e_shnum as usize;
            let section_headers_data = &buffer[section_headers_offset..section_headers_offset + section_headers_size];

            for i in 0..header.e_shnum {
                let offset = (i as usize) * header.e_shentsize as usize;
                let sh = section_header(&section_headers_data[offset..]).unwrap().1;
                println!("Section Header {}: {:?}", i, sh);
            }
        }
        Err(e) => println!("Error parsing ELF header: {:?}", e),
    }
}
