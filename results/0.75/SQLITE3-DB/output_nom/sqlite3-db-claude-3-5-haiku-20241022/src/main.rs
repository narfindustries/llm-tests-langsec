use nom::{
    bytes::complete::{tag, take},
    multi::{count, many0, many_m_n},
    number::complete::{le_u8, le_u16, le_u32, le_u64},
    sequence::{tuple, preceded},
    IResult, Parser,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct SQLite3Header {
    magic_string: Vec<u8>,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_payload_fraction: u8,
    min_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size_pages: u32,
    first_freelist_page: u32,
    total_freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_cache_size: u32,
    largest_root_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    reserved: Vec<u8>,
    version_valid_for: u32,
    sqlite_version: u32,
}

fn parse_sqlite3_header(input: &[u8]) -> IResult<&[u8], SQLite3Header> {
    let (input, magic_string) = take(16usize)(input)?;
    let (input, page_size) = le_u16(input)?;
    let (input, write_version) = le_u8(input)?;
    let (input, read_version) = le_u8(input)?;
    let (input, reserved_space) = le_u8(input)?;
    let (input, max_payload_fraction) = le_u8(input)?;
    let (input, min_payload_fraction) = le_u8(input)?;
    let (input, leaf_payload_fraction) = le_u8(input)?;
    let (input, file_change_counter) = le_u32(input)?;
    let (input, database_size_pages) = le_u32(input)?;
    let (input, first_freelist_page) = le_u32(input)?;
    let (input, total_freelist_pages) = le_u32(input)?;
    let (input, schema_cookie) = le_u32(input)?;
    let (input, schema_format) = le_u32(input)?;
    let (input, default_cache_size) = le_u32(input)?;
    let (input, largest_root_page) = le_u32(input)?;
    let (input, text_encoding) = le_u32(input)?;
    let (input, user_version) = le_u32(input)?;
    let (input, incremental_vacuum_mode) = le_u32(input)?;
    let (input, application_id) = le_u32(input)?;
    let (input, reserved) = take(20usize)(input)?;
    let (input, version_valid_for) = le_u32(input)?;
    let (input, sqlite_version) = le_u32(input)?;

    Ok((input, SQLite3Header {
        magic_string: magic_string.to_vec(),
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_payload_fraction,
        min_payload_fraction,
        leaf_payload_fraction,
        file_change_counter,
        database_size_pages,
        first_freelist_page,
        total_freelist_pages,
        schema_cookie,
        schema_format,
        default_cache_size,
        largest_root_page,
        text_encoding,
        user_version,
        incremental_vacuum_mode,
        application_id,
        reserved: reserved.to_vec(),
        version_valid_for,
        sqlite_version,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_sqlite3_header(&buffer) {
        Ok((_, header)) => {
            println!("SQLite3 Header parsed successfully: {:?}", header);
            Ok(())
        },
        Err(e) => {
            eprintln!("Failed to parse header: {:?}", e);
            Err(e.into())
        }
    }
}