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
    header_string: String,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size_in_pages: u32,
    first_freelist_page: u32,
    number_of_freelist_pages: u32,
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
    let (input, header_string) = take(16usize)(input)?;
    let (input, page_size) = le_u16(input)?;
    let (input, write_version) = take(1usize)(input)?;
    let (input, read_version) = take(1usize)(input)?;
    let (input, reserved_space) = take(1usize)(input)?;
    let (input, max_embedded_payload_fraction) = take(1usize)(input)?;
    let (input, min_embedded_payload_fraction) = take(1usize)(input)?;
    let (input, leaf_payload_fraction) = take(1usize)(input)?;
    let (input, file_change_counter) = le_u32(input)?;
    let (input, database_size_in_pages) = le_u32(input)?;
    let (input, first_freelist_page) = le_u32(input)?;
    let (input, number_of_freelist_pages) = le_u32(input)?;
    let (input, schema_cookie) = le_u32(input)?;
    let (input, schema_format) = le_u32(input)?;
    let (input, default_page_cache_size) = le_u32(input)?;
    let (input, largest_root_btree_page) = le_u32(input)?;
    let (input, text_encoding) = le_u32(input)?;
    let (input, user_version) = le_u32(input)?;
    let (input, incremental_vacuum_mode) = le_u32(input)?;
    let (input, application_id) = le_u32(input)?;
    let (input, version_valid_for) = le_u32(input)?;
    let (input, sqlite_version_number) = le_u32(input)?;

    Ok((
        input,
        SqliteHeader {
            header_string: String::from_utf8_lossy(header_string).to_string(),
            page_size,
            write_version: write_version[0],
            read_version: read_version[0],
            reserved_space: reserved_space[0],
            max_embedded_payload_fraction: max_embedded_payload_fraction[0],
            min_embedded_payload_fraction: min_embedded_payload_fraction[0],
            leaf_payload_fraction: leaf_payload_fraction[0],
            file_change_counter,
            database_size_in_pages,
            first_freelist_page,
            number_of_freelist_pages,
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
        },
    ))
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