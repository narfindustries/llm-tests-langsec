use nom::{
    bytes::complete::{tag, take_until},
    multi::{many0, many1},
    number::complete::{le_u8, le_u16, le_u32, le_u64},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct SQLiteHeader {
    magic_string: Vec<u8>,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    page_count: u32,
    first_freelist_trunk_page: u32,
    freelist_page_count: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_page_cache_size: u32,
    largest_root_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    reserved: Vec<u8>,
    version_valid_for: u32,
    sqlite_version: u32,
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    let (input, magic_string) = take_until("\0")(input)?;
    let (input, _) = tag("\0")(input)?;
    let (input, (
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_embedded_payload_fraction,
        min_embedded_payload_fraction,
        leaf_payload_fraction,
        file_change_counter,
        page_count,
        first_freelist_trunk_page,
        freelist_page_count,
        schema_cookie,
        schema_format,
        default_page_cache_size,
        largest_root_page,
        text_encoding,
        user_version,
        incremental_vacuum_mode,
        application_id,
        reserved,
        version_valid_for,
        sqlite_version
    )) = tuple((
        le_u16,
        le_u8,
        le_u8,
        le_u8,
        le_u8,
        le_u8,
        le_u8,
        le_u32,
        le_u32,
        le_u32,
        le_u32,
        le_u32,
        le_u32,
        le_u32,
        le_u32,
        le_u32,
        le_u32,
        le_u32,
        le_u32,
        take_until("\0"),
        le_u32,
        le_u32
    ))(input)?;

    Ok((input, SQLiteHeader {
        magic_string: magic_string.to_vec(),
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_embedded_payload_fraction,
        min_embedded_payload_fraction,
        leaf_payload_fraction,
        file_change_counter,
        page_count,
        first_freelist_trunk_page,
        freelist_page_count,
        schema_cookie,
        schema_format,
        default_page_cache_size,
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
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_sqlite_header(&buffer) {
        Ok((_, header)) => {
            println!("SQLite Header parsed successfully: {:?}", header);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing SQLite header: {:?}", e);
            Err(e.into())
        }
    }
}