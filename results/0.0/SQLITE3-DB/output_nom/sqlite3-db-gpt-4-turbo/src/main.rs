use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

const SQLITE_HEADER_STRING: &[u8] = b"SQLite format 3\0";

#[derive(Debug)]
struct DatabaseHeader {
    header_string: String,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_payload_frac: u8,
    min_payload_frac: u8,
    leaf_payload_frac: u8,
    file_change_counter: u32,
    database_size: u32,
    first_freelist_trunk_page: u32,
    freelist_page_count: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_cache_size: u32,
    largest_root_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    reserved: Vec<u8>,
    version_valid_for: u32,
    sqlite_version_number: u32,
}

fn parse_database_header(input: &[u8]) -> IResult<&[u8], DatabaseHeader> {
    let (input, _) = tag(SQLITE_HEADER_STRING)(input)?;
    let (input, (page_size, write_version, read_version, reserved_space, max_payload_frac, min_payload_frac, leaf_payload_frac, file_change_counter, database_size, first_freelist_trunk_page, freelist_page_count, schema_cookie, schema_format, default_cache_size, largest_root_page, text_encoding, user_version, incremental_vacuum_mode, application_id, reserved, version_valid_for, sqlite_version_number)) = tuple((
        le_u16,
        le_u8,
        le_u8,
        le_u8,
        le_u8,
        le_u8,
        le_u8,
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
        take(20usize),
        le_u32,
        le_u32,
    ))(input)?;

    Ok((
        input,
        DatabaseHeader {
            header_string: String::from_utf8_lossy(SQLITE_HEADER_STRING).to_string(),
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_payload_frac,
            min_payload_frac,
            leaf_payload_frac,
            file_change_counter,
            database_size,
            first_freelist_trunk_page,
            freelist_page_count,
            schema_cookie,
            schema_format,
            default_cache_size,
            largest_root_page,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            reserved: reserved.to_vec(),
            version_valid_for,
            sqlite_version_number,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Usage: sqlite_parser <SQLite_file>",
        ));
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_database_header(&buffer) {
        Ok((_, header)) => {
            println!("Parsed SQLite Database Header: {:?}", header);
        }
        Err(e) => {
            println!("Failed to parse SQLite Database Header: {:?}", e);
        }
    }

    Ok(())
}