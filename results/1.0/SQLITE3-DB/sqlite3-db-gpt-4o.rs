extern crate nom;

use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;
use std::path::Path;

#[derive(Debug)]
pub struct Sqlite3Header {
    pub magic_header: [u8; 16],
    pub page_size: u16,
    pub write_version: u8,
    pub read_version: u8,
    pub reserved_space: u8,
    pub max_payload_frac: u8,
    pub min_payload_frac: u8,
    pub leaf_payload_frac: u8,
    pub file_change_counter: u32,
    pub database_size: u32,
    pub page_number_first_freelist: u32,
    pub freelist_count: u32,
    pub schema_cookie: u32,
    pub schema_format_number: u32,
    pub default_page_cache_size: u32,
    pub largest_root_btree_page: u32,
    pub text_encoding: u32,
    pub user_version: u32,
    pub incremental_vacuum_mode: u32,
    pub application_id: u32,
    pub reserved: [u8; 20],
    pub version_valid_for: u32,
    pub sqlite_version_number: u32,
}

fn parse_sqlite3_header(input: &[u8]) -> IResult<&[u8], Sqlite3Header> {
    let (input, magic_header) = take(16usize)(input)?;
    let (input, page_size) = be_u16(input)?;
    let (input, write_version) = be_u8(input)?;
    let (input, read_version) = be_u8(input)?;
    let (input, reserved_space) = be_u8(input)?;
    let (input, max_payload_frac) = be_u8(input)?;
    let (input, min_payload_frac) = be_u8(input)?;
    let (input, leaf_payload_frac) = be_u8(input)?;
    let (input, file_change_counter) = be_u32(input)?;
    let (input, database_size) = be_u32(input)?;
    let (input, page_number_first_freelist) = be_u32(input)?;
    let (input, freelist_count) = be_u32(input)?;
    let (input, schema_cookie) = be_u32(input)?;
    let (input, schema_format_number) = be_u32(input)?;
    let (input, default_page_cache_size) = be_u32(input)?;
    let (input, largest_root_btree_page) = be_u32(input)?;
    let (input, text_encoding) = be_u32(input)?;
    let (input, user_version) = be_u32(input)?;
    let (input, incremental_vacuum_mode) = be_u32(input)?;
    let (input, application_id) = be_u32(input)?;
    let (input, reserved) = take(20usize)(input)?;
    let (input, version_valid_for) = be_u32(input)?;
    let (input, sqlite_version_number) = be_u32(input)?;

    let mut magic_header_array = [0u8; 16];
    magic_header_array.copy_from_slice(magic_header);
    let mut reserved_array = [0u8; 20];
    reserved_array.copy_from_slice(reserved);

    Ok((
        input,
        Sqlite3Header {
            magic_header: magic_header_array,
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_payload_frac,
            min_payload_frac,
            leaf_payload_frac,
            file_change_counter,
            database_size,
            page_number_first_freelist,
            freelist_count,
            schema_cookie,
            schema_format_number,
            default_page_cache_size,
            largest_root_btree_page,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            reserved: reserved_array,
            version_valid_for,
            sqlite_version_number,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite3_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file {}: {}", path.display(), err);
            return;
        }
    };

    let mut buffer = Vec::new();
    if let Err(err) = file.read_to_end(&mut buffer) {
        eprintln!("Error reading file {}: {}", path.display(), err);
        return;
    }

    match parse_sqlite3_header(&buffer) {
        Ok((_, header)) => {
            println!("Parsed SQLite3 Header: {:?}", header);
        }
        Err(err) => {
            eprintln!("Parsing error: {:?}", err);
        }
    }
}