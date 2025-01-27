use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::count,
    number::complete::{le_i16, le_i32, le_i64, le_u16, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct Sqlite3Header {
    magic: String,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_payload_frac: u8,
    min_payload_frac: u8,
    leaf_payload_frac: u8,
    file_change_counter: u32,
    num_pages: u32,
    first_freelist_trunk_page: u32,
    num_freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_page_cache_size: u32,
    largest_root_btree_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    version_valid_for: u32,
    sqlite_version_number: u32,
}

fn parse_sqlite3_header(input: &[u8]) -> IResult<&[u8], Sqlite3Header> {
    map(
        tuple((
            tag("SQLite format 3\0"),
            le_u16,
            le_u8,
            le_u8,
            le_u8,
            le_u8,
            le_u8,
            le_u8,
            le_i32,
            le_i32,
            le_i32,
            le_i32,
            le_i32,
            le_i32,
            le_i32,
            le_i32,
            le_i32,
            le_i32,
            le_i32,
            le_i32,
            le_i32,
            le_i32,
            le_i32,
        )),
        |(
            _,
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_payload_frac,
            min_payload_frac,
            leaf_payload_frac,
            file_change_counter,
            num_pages,
            first_freelist_trunk_page,
            num_freelist_pages,
            schema_cookie,
            schema_format,
            default_page_cache_size,
            largest_root_btree_page,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            version_valid_for,
            sqlite_version_number,
        )| Sqlite3Header {
            magic: "SQLite format 3".to_string(),
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_payload_frac,
            min_payload_frac,
            leaf_payload_frac,
            file_change_counter: file_change_counter as u32,
            num_pages: num_pages as u32,
            first_freelist_trunk_page: first_freelist_trunk_page as u32,
            num_freelist_pages: num_freelist_pages as u32,
            schema_cookie: schema_cookie as u32,
            schema_format: schema_format as u32,
            default_page_cache_size: default_page_cache_size as u32,
            largest_root_btree_page: largest_root_btree_page as u32,
            text_encoding: text_encoding as u32,
            user_version: user_version as u32,
            incremental_vacuum_mode: incremental_vacuum_mode as u32,
            application_id: application_id as u32,
            version_valid_for: version_valid_for as u32,
            sqlite_version_number: sqlite_version_number as u32,
        },
    )(input)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_sqlite3_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Failed to parse SQLite3 header: {:?}", e),
    }

    Ok(())
}