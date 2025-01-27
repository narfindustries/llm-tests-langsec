use nom::{
    bytes::complete::{tag, take_while},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct SQLiteHeader {
    magic: [u8; 16],
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_payload_fraction: u8,
    min_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size: u32,
    first_freelist_trunk_page: u32,
    total_freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_cache_size: u32,
    largest_root_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    reserved: [u8; 20],
    version_valid_for: u32,
    sqlite_version: u32,
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    map(
        tuple((
            take_while(|_| true),
            le_u16,
            take_while(|_| true),
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
            le_u32,
            le_u32,
            le_u32,
            take_while(|_| true),
            le_u32,
            le_u32,
        )),
        |(
            magic,
            page_size,
            _,
            file_change_counter,
            database_size,
            first_freelist_trunk_page,
            total_freelist_pages,
            schema_cookie,
            schema_format,
            default_cache_size,
            largest_root_page,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            _,
            version_valid_for,
            sqlite_version,
            reserved,
            write_version,
            read_version,
        )| SQLiteHeader {
            magic: magic.try_into().unwrap(),
            page_size,
            write_version: write_version as u8,
            read_version: read_version as u8,
            reserved_space: 0,
            max_payload_fraction: 64,
            min_payload_fraction: 32,
            leaf_payload_fraction: 32,
            file_change_counter,
            database_size,
            first_freelist_trunk_page,
            total_freelist_pages,
            schema_cookie,
            schema_format,
            default_cache_size,
            largest_root_page,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            reserved: reserved.try_into().unwrap(),
            version_valid_for,
            sqlite_version,
        },
    )(input)
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
            println!("SQLite Database Header: {:?}", header);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            Err(e.into())
        }
    }
}