use nom::{
    bytes::complete::tag,
    number::complete::{le_u16, le_u32, le_u8},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct SQLiteHeader {
    magic_header: String,
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
    number_of_freelist_pages: u32,
    schema_cookie: u32,
    schema_format_number: u32,
    default_page_cache_size: u32,
    largest_root_btree_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    version_valid_for: u32,
    sqlite_version_number: u32,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    let (input, magic_header) = tag(b"SQLite format 3\0")(input)?;
    let (input, page_size) = le_u16(input)?;
    let (input, write_version) = le_u8(input)?;
    let (input, read_version) = le_u8(input)?;
    let (input, reserved_space) = le_u8(input)?;
    let (input, max_payload_fraction) = le_u8(input)?;
    let (input, min_payload_fraction) = le_u8(input)?;
    let (input, leaf_payload_fraction) = le_u8(input)?;
    let (input, file_change_counter) = le_u32(input)?;
    let (input, database_size) = le_u32(input)?;
    let (input, first_freelist_trunk_page) = le_u32(input)?;
    let (input, number_of_freelist_pages) = le_u32(input)?;
    let (input, schema_cookie) = le_u32(input)?;
    let (input, schema_format_number) = le_u32(input)?;
    let (input, default_page_cache_size) = le_u32(input)?;
    let (input, largest_root_btree_page) = le_u32(input)?;
    let (input, text_encoding) = le_u32(input)?;
    let (input, user_version) = le_u32(input)?;
    let (input, incremental_vacuum_mode) = le_u32(input)?;
    let (input, application_id) = le_u32(input)?;
    let (input, _reserved) = tag([0; 20])(input)?;
    let (input, version_valid_for) = le_u32(input)?;
    let (input, sqlite_version_number) = le_u32(input)?;

    Ok((
        input,
        SQLiteHeader {
            magic_header: String::from_utf8_lossy(magic_header).to_string(),
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_payload_fraction,
            min_payload_fraction,
            leaf_payload_fraction,
            file_change_counter,
            database_size,
            first_freelist_trunk_page,
            number_of_freelist_pages,
            schema_cookie,
            schema_format_number,
            default_page_cache_size,
            largest_root_btree_page,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            version_valid_for,
            sqlite_version_number,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_db_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Failed to parse header: {:?}", e),
    }
}