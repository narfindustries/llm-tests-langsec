use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    number::complete::{le_u16, le_u32, le_u64},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct SqliteHeader {
    magic: [u8; 16],
    page_size: u16,
    write_version: u16,
    read_version: u16,
    reserved_byte: u8,
    max_page_count: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u64,
    version_valid_for_all_pages: u32,
    page_count: u32,
    checksum_flag: u32,
    free_page_count: u32,
    schema_version: u32,
    schema_format: u32,
    default_page_cache_size: u32,
    large_file_support: u32,
    page_size_extension: u32,
    reserved_bytes: [u8; 20],
}

fn sqlite_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
    let (input, magic) = take(16usize)(input)?;
    let (input, page_size) = le_u16(input)?;
    let (input, write_version) = le_u16(input)?;
    let (input, read_version) = le_u16(input)?;
    let (input, reserved_byte) = take(1usize)(input)?;
    let (input, max_page_count) = le_u32(input)?;
    let (input, text_encoding) = le_u32(input)?;
    let (input, user_version) = le_u32(input)?;
    let (input, incremental_vacuum_mode) = le_u32(input)?;
    let (input, application_id) = le_u64(input)?;
    let (input, version_valid_for_all_pages) = le_u32(input)?;
    let (input, page_count) = le_u32(input)?;
    let (input, checksum_flag) = le_u32(input)?;
    let (input, free_page_count) = le_u32(input)?;
    let (input, schema_version) = le_u32(input)?;
    let (input, schema_format) = le_u32(input)?;
    let (input, default_page_cache_size) = le_u32(input)?;
    let (input, large_file_support) = le_u32(input)?;
    let (input, page_size_extension) = le_u32(input)?;
    let (input, reserved_bytes) = take(20usize)(input)?;

    Ok((
        input,
        SqliteHeader {
            magic: magic.try_into().unwrap(),
            page_size,
            write_version,
            read_version,
            reserved_byte: reserved_byte[0],
            max_page_count,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            version_valid_for_all_pages,
            page_count,
            checksum_flag,
            free_page_count,
            schema_version,
            schema_format,
            default_page_cache_size,
            large_file_support,
            page_size_extension,
            reserved_bytes: reserved_bytes.try_into().unwrap(),
        },
    ))
}


fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_db_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            std::process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            std::process::exit(1);
        }
    };

    match sqlite_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(err) => {
            eprintln!("Error parsing header: {:?}", err);
            std::process::exit(1);
        }
    };
}
