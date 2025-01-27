use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u64},
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug)]
struct ElfHeader {
    magic: [u8; 4],
    class: u8,
    data: u8,
    version: u8,
    os_abi: u8,
    abi_version: u8,
    padding: [u8; 7],
    e_type: u16,
    machine: u16,
    e_version: u32,
    entry_point: u64,
    ph_off: u64,
    sh_off: u64,
    flags: u32,
    eh_size: u16,
    ph_ent_size: u16,
    ph_num: u16,
    sh_ent_size: u16,
    sh_num: u16,
    sh_str_idx: u16,
}

fn parse_elf_header(input: &[u8]) -> IResult<&[u8], ElfHeader> {
    let (input, magic) = tag(b"\x7FELF")(input)?;
    let (input, class) = le_u8(input)?;
    let (input, data) = le_u8(input)?;
    let (input, version) = le_u8(input)?;
    let (input, os_abi) = le_u8(input)?;
    let (input, abi_version) = le_u8(input)?;
    let (input, padding) = take::<_, _, nom::error::Error<_>>(7usize)(input)?;
    let (input, e_type) = le_u16(input)?;
    let (input, machine) = le_u16(input)?;
    let (input, e_version) = le_u32(input)?;
    let (input, entry_point) = le_u64(input)?;
    let (input, ph_off) = le_u64(input)?;
    let (input, sh_off) = le_u64(input)?;
    let (input, flags) = le_u32(input)?;
    let (input, eh_size) = le_u16(input)?;
    let (input, ph_ent_size) = le_u16(input)?;
    let (input, ph_num) = le_u16(input)?;
    let (input, sh_ent_size) = le_u16(input)?;
    let (input, sh_num) = le_u16(input)?;
    let (input, sh_str_idx) = le_u16(input)?;

    Ok((
        input,
        ElfHeader {
            magic: [magic[0], magic[1], magic[2], magic[3]],
            class,
            data,
            version,
            os_abi,
            abi_version,
            padding: [
                padding[0], padding[1], padding[2], padding[3], padding[4], padding[5], padding[6],
            ],
            e_type,
            machine,
            e_version,
            entry_point,
            ph_off,
            sh_off,
            flags,
            eh_size,
            ph_ent_size,
            ph_num,
            sh_ent_size,
            sh_num,
            sh_str_idx,
        },
    ))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let file_path = Path::new(&args[1]);
    let mut file = File::open(file_path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_elf_header(&buffer) {
        Ok((_, elf_header)) => {
            println!("{:?}", elf_header);
        }
        Err(e) => println!("Failed to parse ELF header: {:?}", e),
    }

    Ok(())
}