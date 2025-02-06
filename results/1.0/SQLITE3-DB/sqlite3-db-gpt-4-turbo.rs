use nom::{
    IResult, bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
};

use std::{
    env,
    fs::File,
    io::{self, Read},
};

const HEADER_STRING: &[u8] = b"SQLite format 3\0";

#[derive(Debug, PartialEq)]
struct DatabaseHeader {
    header_string: Vec<u8>,
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
    freelist_pages_count: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_cache_size: u32,
    largest_root_btree_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    reserved: [u8; 20],
    version_valid_for: u32,
    sqlite_version_number: u32,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DatabaseHeader> {
    let (input, _) = tag(HEADER_STRING)(input)?;
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
    let (input, freelist_pages_count) = le_u32(input)?;
    let (input, schema_cookie) = le_u32(input)?;
    let (input, schema_format) = le_u32(input)?;
    let (input, default_cache_size) = le_u32(input)?;
    let (input, largest_root_btree_page) = le_u32(input)?;
    let (input, text_encoding) = le_u32(input)?;
    let (input, user_version) = le_u32(input)?;
    let (input, incremental_vacuum_mode) = le_u32(input)?;
    let (input, application_id) = le_u32(input)?;
    let (input, reserved) = take(20usize)(input)?;
    let (input, version_valid_for) = le_u32(input)?;
    let (input, sqlite_version_number) = le_u32(input)?;

    Ok((input, DatabaseHeader {
        header_string: HEADER_STRING.to_vec(),
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
        freelist_pages_count,
        schema_cookie,
        schema_format,
        default_cache_size,
        largest_root_btree_page,
        text_encoding,
        user_version,
        incremental_vacuum_mode,
        application_id,
        reserved: reserved.try_into().expect("Failed to convert reserved bytes"),
        version_valid_for,
        sqlite_version_number,
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No file provided"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_header(&buffer) {
        Ok((_, header)) => println!("Parsed SQLite header: {:?}", header),
        Err(e) => println!("Error parsing SQLite header: {:?}", e),
    }

    Ok(())
}