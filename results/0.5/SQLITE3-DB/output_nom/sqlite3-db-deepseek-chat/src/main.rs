use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::Read,
};

#[derive(Debug)]
struct SqliteHeader {
    magic: String,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_bytes: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size_in_pages: u32,
    first_freelist_page: u32,
    freelist_page_count: u32,
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

fn parse_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
    let (input, (magic, page_size, write_version, read_version, reserved_bytes, max_embedded_payload_fraction, min_embedded_payload_fraction, leaf_payload_fraction, file_change_counter, database_size_in_pages, first_freelist_page, freelist_page_count, schema_cookie, schema_format, default_page_cache_size, largest_root_btree_page, text_encoding, user_version, incremental_vacuum_mode, application_id, version_valid_for, sqlite_version_number)) = tuple((
        take(16usize),
        le_u16,
        take(1u8),
        take(1u8),
        take(1u8),
        take(1u8),
        take(1u8),
        take(1u8),
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
    ))(input)?;

    Ok((input, SqliteHeader {
        magic: String::from_utf8_lossy(magic).to_string(),
        page_size,
        write_version: write_version[0],
        read_version: read_version[0],
        reserved_bytes: reserved_bytes[0],
        max_embedded_payload_fraction: max_embedded_payload_fraction[0],
        min_embedded_payload_fraction: min_embedded_payload_fraction[0],
        leaf_payload_fraction: leaf_payload_fraction[0],
        file_change_counter,
        database_size_in_pages,
        first_freelist_page,
        freelist_page_count,
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
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => eprintln!("Failed to parse SQLite header: {:?}", e),
    }
}