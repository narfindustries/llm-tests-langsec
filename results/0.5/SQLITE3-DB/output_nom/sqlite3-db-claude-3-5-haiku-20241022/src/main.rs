use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::count,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct SQLiteHeader {
    magic: [u8; 16],
    page_size: u16,
    write_version: u8,
    read_version: u8,
    unused: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size_pages: u32,
    first_freelist_trunk_page: u32,
    total_freelist_pages: u32,
    schema_cookie: u32,
    schema_format_number: u32,
    default_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    reserved_space: Vec<u8>,
}

#[derive(Debug)]
enum PageType {
    IndexInterior = 2,
    TableInterior = 5,
    IndexLeaf = 10,
    TableLeaf = 13,
}

#[derive(Debug)]
struct SerialType {
    code: u8,
    length: usize,
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    let (input, magic) = take(16usize)(input)?;
    let (input, page_size) = be_u16(input)?;
    let (input, write_version) = be_u8(input)?;
    let (input, read_version) = be_u8(input)?;
    let (input, unused) = be_u8(input)?;
    let (input, max_embedded_payload_fraction) = be_u8(input)?;
    let (input, min_embedded_payload_fraction) = be_u8(input)?;
    let (input, leaf_payload_fraction) = be_u8(input)?;
    let (input, file_change_counter) = be_u32(input)?;
    let (input, database_size_pages) = be_u32(input)?;
    let (input, first_freelist_trunk_page) = be_u32(input)?;
    let (input, total_freelist_pages) = be_u32(input)?;
    let (input, schema_cookie) = be_u32(input)?;
    let (input, schema_format_number) = be_u32(input)?;
    let (input, default_encoding) = be_u32(input)?;
    let (input, user_version) = be_u32(input)?;
    let (input, incremental_vacuum_mode) = be_u32(input)?;
    let (input, application_id) = be_u32(input)?;
    let (input, reserved_space) = count(be_u8, 20)(input)?;

    Ok((input, SQLiteHeader {
        magic: magic.try_into().unwrap(),
        page_size,
        write_version,
        read_version,
        unused,
        max_embedded_payload_fraction,
        min_embedded_payload_fraction,
        leaf_payload_fraction,
        file_change_counter,
        database_size_pages,
        first_freelist_trunk_page,
        total_freelist_pages,
        schema_cookie,
        schema_format_number,
        default_encoding,
        user_version,
        incremental_vacuum_mode,
        application_id,
        reserved_space,
    }))
}

fn parse_serial_type(input: &[u8]) -> IResult<&[u8], SerialType> {
    let (input, code) = be_u8(input)?;
    let length = match code {
        0 => 0,
        1 => 1,
        2 => 2,
        3 => 3,
        4 => 4,
        5 => 6,
        6 => 8,
        7 => 8,
        8 => 0,
        9 => 0,
        n if n >= 12 && n % 2 == 0 => (n - 12) as usize / 2,
        n if n >= 13 && n % 2 == 1 => (n - 13) as usize / 2,
        _ => panic!("Invalid serial type"),
    };
    Ok((input, SerialType { code, length }))
}

fn main() -> std::io::Result<()> {
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
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}