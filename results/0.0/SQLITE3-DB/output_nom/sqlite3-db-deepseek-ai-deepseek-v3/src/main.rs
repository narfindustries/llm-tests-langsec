use nom::{
    bytes::complete::tag,
    number::complete::{le_u16, le_u32, le_u8},
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct SqliteHeader {
    header_string: Vec<u8>,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_embedded_payload: u8,
    min_embedded_payload: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size: u32,
    first_freelist_page: u32,
    freelist_page_count: u32,
    schema_cookie: u32,
    schema_format_number: u32,
    default_cache_size: u32,
    autovacuum_top_root: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum: u32,
    application_id: u32,
    reserved: [u8; 20],
    version_valid_for: u32,
    sqlite_version: u32,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
    let (input, header_string) = tag("SQLite format 3\0")(input)?;
    let (input, page_size) = le_u16(input)?;
    let (input, write_version) = le_u8(input)?;
    let (input, read_version) = le_u8(input)?;
    let (input, reserved_space) = le_u8(input)?;
    let (input, max_embedded_payload) = le_u8(input)?;
    let (input, min_embedded_payload) = le_u8(input)?;
    let (input, leaf_payload_fraction) = le_u8(input)?;
    let (input, file_change_counter) = le_u32(input)?;
    let (input, database_size) = le_u32(input)?;
    let (input, first_freelist_page) = le_u32(input)?;
    let (input, freelist_page_count) = le_u32(input)?;
    let (input, schema_cookie) = le_u32(input)?;
    let (input, schema_format_number) = le_u32(input)?;
    let (input, default_cache_size) = le_u32(input)?;
    let (input, autovacuum_top_root) = le_u32(input)?;
    let (input, text_encoding) = le_u32(input)?;
    let (input, user_version) = le_u32(input)?;
    let (input, incremental_vacuum) = le_u32(input)?;
    let (input, application_id) = le_u32(input)?;
    let (input, reserved) = nom::bytes::complete::take(20u8)(input)?;
    let (input, version_valid_for) = le_u32(input)?;
    let (input, sqlite_version) = le_u32(input)?;

    Ok((
        input,
        SqliteHeader {
            header_string: header_string.to_vec(),
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_embedded_payload,
            min_embedded_payload,
            leaf_payload_fraction,
            file_change_counter,
            database_size,
            first_freelist_page,
            freelist_page_count,
            schema_cookie,
            schema_format_number,
            default_cache_size,
            autovacuum_top_root,
            text_encoding,
            user_version,
            incremental_vacuum,
            application_id,
            reserved: reserved.try_into().unwrap(),
            version_valid_for,
            sqlite_version,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    match parse_header(&data) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Failed to parse SQLite header: {:?}", e),
    }
}