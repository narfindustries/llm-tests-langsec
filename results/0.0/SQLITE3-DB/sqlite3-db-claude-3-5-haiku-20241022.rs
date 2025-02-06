use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::error::Error;

#[derive(Debug)]
struct SQLiteHeader {
    magic: [u8; 16],
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size_pages: u32,
    first_freelist_page: u32,
    total_freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    let (input, magic) = take(16usize)(input)?;
    let (input, page_size) = be_u16(input)?;
    let (input, write_version) = be_u8(input)?;
    let (input, read_version) = be_u8(input)?;
    let (input, reserved_space) = be_u8(input)?;
    let (input, max_embedded_payload_fraction) = be_u8(input)?;
    let (input, min_embedded_payload_fraction) = be_u8(input)?;
    let (input, leaf_payload_fraction) = be_u8(input)?;
    let (input, file_change_counter) = be_u32(input)?;
    let (input, database_size_pages) = be_u32(input)?;
    let (input, first_freelist_page) = be_u32(input)?;
    let (input, total_freelist_pages) = be_u32(input)?;
    let (input, schema_cookie) = be_u32(input)?;
    let (input, schema_format) = be_u32(input)?;
    let (input, default_encoding) = be_u32(input)?;
    let (input, user_version) = be_u32(input)?;
    let (input, incremental_vacuum_mode) = be_u32(input)?;
    let (input, application_id) = be_u32(input)?;

    Ok((input, SQLiteHeader {
        magic: magic.try_into().unwrap(),
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_embedded_payload_fraction,
        min_embedded_payload_fraction,
        leaf_payload_fraction,
        file_change_counter,
        database_size_pages,
        first_freelist_page,
        total_freelist_pages,
        schema_cookie,
        schema_format,
        default_encoding,
        user_version,
        incremental_vacuum_mode,
        application_id,
    }))
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_sqlite_header(&buffer) {
        Ok((_, header)) => {
            println!("SQLite Header: {:?}", header);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            Err(Box::new(std::io::Error::new(std::io::ErrorKind::InvalidData, "Parsing failed")))
        }
    }
}