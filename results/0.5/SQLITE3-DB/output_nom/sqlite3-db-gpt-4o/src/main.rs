extern crate nom;

use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, be_u64, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct SqliteHeader {
    header_string: [u8; 16],
    page_size: u16,
    file_format_write_version: u8,
    file_format_read_version: u8,
    reserved_space: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size_pages: u32,
    first_freelist_trunk_page: u32,
    total_freelist_trunk_pages: u32,
    schema_cookie: u32,
    schema_format_number: u32,
    default_page_cache_size: u32,
    largest_root_btree_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    reserved: [u8; 20],
    version_valid_for_number: u32,
    sqlite_version_number: u32,
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
    let (input, header_string) = take(16usize)(input)?;
    let (input, page_size) = be_u16(input)?;
    let (input, file_format_write_version) = be_u8(input)?;
    let (input, file_format_read_version) = be_u8(input)?;
    let (input, reserved_space) = be_u8(input)?;
    let (input, max_embedded_payload_fraction) = be_u8(input)?;
    let (input, min_embedded_payload_fraction) = be_u8(input)?;
    let (input, leaf_payload_fraction) = be_u8(input)?;
    let (input, file_change_counter) = be_u32(input)?;
    let (input, database_size_pages) = be_u32(input)?;
    let (input, first_freelist_trunk_page) = be_u32(input)?;
    let (input, total_freelist_trunk_pages) = be_u32(input)?;
    let (input, schema_cookie) = be_u32(input)?;
    let (input, schema_format_number) = be_u32(input)?;
    let (input, default_page_cache_size) = be_u32(input)?;
    let (input, largest_root_btree_page) = be_u32(input)?;
    let (input, text_encoding) = be_u32(input)?;
    let (input, user_version) = be_u32(input)?;
    let (input, incremental_vacuum_mode) = be_u32(input)?;
    let (input, application_id) = be_u32(input)?;
    let (input, reserved) = take(20usize)(input)?;
    let (input, version_valid_for_number) = be_u32(input)?;
    let (input, sqlite_version_number) = be_u32(input)?;

    Ok((
        input,
        SqliteHeader {
            header_string: *array_ref!(header_string, 0, 16),
            page_size,
            file_format_write_version,
            file_format_read_version,
            reserved_space,
            max_embedded_payload_fraction,
            min_embedded_payload_fraction,
            leaf_payload_fraction,
            file_change_counter,
            database_size_pages,
            first_freelist_trunk_page,
            total_freelist_trunk_pages,
            schema_cookie,
            schema_format_number,
            default_page_cache_size,
            largest_root_btree_page,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            reserved: *array_ref!(reserved, 0, 20),
            version_valid_for_number,
            sqlite_version_number,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <path to sqlite3 db file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Err(why) => {
            eprintln!("Couldn't open {}: {}", path.display(), why);
            return;
        }
        Ok(file) => file,
    };

    let mut buffer = Vec::new();
    if let Err(why) = file.read_to_end(&mut buffer) {
        eprintln!("Couldn't read {}: {}", path.display(), why);
        return;
    }

    match parse_sqlite_header(&buffer) {
        Ok((_, header)) => {
            println!("{:?}", header);
        }
        Err(err) => {
            eprintln!("Failed to parse SQLite header: {:?}", err);
        }
    }
}