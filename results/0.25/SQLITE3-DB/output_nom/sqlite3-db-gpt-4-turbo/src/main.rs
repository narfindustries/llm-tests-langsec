use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::count,
    number::complete::{le_i16, le_i32, le_i64, le_u16, le_u32, le_u8},
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

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
    default_cache_size: u32,
    largest_root_btree_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    version_valid_for: u32,
    sqlite_version_number: u32,
    reserved: Vec<u8>,
}

fn parse_sqlite3_header(input: &[u8]) -> IResult<&[u8], Sqlite3Header> {
    let (input, magic) = map_res(take(16usize), std::str::from_utf8)(input)?;
    let (input, page_size) = le_u16(input)?;
    let (input, write_version) = le_u8(input)?;
    let (input, read_version) = le_u8(input)?;
    let (input, reserved_space) = le_u8(input)?;
    let (input, max_payload_frac) = le_u8(input)?;
    let (input, min_payload_frac) = le_u8(input)?;
    let (input, leaf_payload_frac) = le_u8(input)?;
    let (input, file_change_counter) = le_u32(input)?;
    let (input, num_pages) = le_u32(input)?;
    let (input, first_freelist_trunk_page) = le_u32(input)?;
    let (input, num_freelist_pages) = le_u32(input)?;
    let (input, schema_cookie) = le_u32(input)?;
    let (input, schema_format) = le_u32(input)?;
    let (input, default_cache_size) = le_u32(input)?;
    let (input, largest_root_btree_page) = le_u32(input)?;
    let (input, text_encoding) = le_u32(input)?;
    let (input, user_version) = le_u32(input)?;
    let (input, incremental_vacuum_mode) = le_u32(input)?;
    let (input, application_id) = le_u32(input)?;
    let (input, version_valid_for) = le_u32(input)?;
    let (input, sqlite_version_number) = le_u32(input)?;
    let (input, reserved) = take(60usize)(input)?;

    Ok((
        input,
        Sqlite3Header {
            magic: magic.to_string(),
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
            default_cache_size,
            largest_root_btree_page,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            version_valid_for,
            sqlite_version_number,
            reserved: reserved.to_vec(),
        },
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <sqlite3_db_file>", args[0]);
        return Ok(());
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_sqlite3_header(&buffer) {
        Ok((_, header)) => {
            println!("{:#?}", header);
        }
        Err(e) => {
            println!("Failed to parse SQLite3 header: {:?}", e);
        }
    }

    Ok(())
}