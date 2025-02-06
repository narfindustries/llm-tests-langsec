use nom::{
    bytes::complete::take,
    combinator::map_opt,
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
    reserved_space: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size: u32,
    first_freelist_page: u32,
    total_freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
}

#[derive(Debug)]
enum PageType {
    IndexInterior = 2,
    TableInterior = 5,
    IndexLeaf = 10,
    TableLeaf = 13,
}

#[derive(Debug)]
enum SerialType {
    Null,
    Int8,
    Int16,
    Int24,
    Int32,
    Int48,
    Int64,
    Float64,
    Zero,
    One,
    Blob(usize),
    Text(usize),
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    let (input, (
        magic,
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_embedded_payload_fraction,
        min_embedded_payload_fraction,
        leaf_payload_fraction,
        file_change_counter,
        database_size,
        first_freelist_page,
        total_freelist_pages,
        schema_cookie,
        schema_format,
        default_encoding,
        user_version,
        incremental_vacuum_mode
    )) = tuple((
        take(16usize),
        be_u16,
        be_u8,
        be_u8,
        be_u8,
        be_u8,
        be_u8,
        be_u8,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        be_u32
    ))(input)?;

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
        database_size,
        first_freelist_page,
        total_freelist_pages,
        schema_cookie,
        schema_format,
        default_encoding,
        user_version,
        incremental_vacuum_mode,
    }))
}

fn parse_serial_type(input: &[u8]) -> IResult<&[u8], SerialType> {
    map_opt(be_u8, |code| match code {
        0 => Some(SerialType::Null),
        1 => Some(SerialType::Int8),
        2 => Some(SerialType::Int16),
        3 => Some(SerialType::Int24),
        4 => Some(SerialType::Int32),
        5 => Some(SerialType::Int48),
        6 => Some(SerialType::Int64),
        7 => Some(SerialType::Float64),
        8 => Some(SerialType::Zero),
        9 => Some(SerialType::One),
        n if n >= 12 && n % 2 == 0 => Some(SerialType::Blob(((n - 12) / 2).into())),
        n if n >= 13 && n % 2 == 1 => Some(SerialType::Text(((n - 13) / 2).into())),
        _ => None,
    })(input)
}

fn parse_page_type(input: &[u8]) -> IResult<&[u8], PageType> {
    map_opt(be_u8, |code| match code {
        2 => Some(PageType::IndexInterior),
        5 => Some(PageType::TableInterior),
        10 => Some(PageType::IndexLeaf),
        13 => Some(PageType::TableLeaf),
        _ => None,
    })(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
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
        }
    }

    Ok(())
}