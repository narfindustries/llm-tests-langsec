use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::many0,
    number::complete::{be_u16, be_u32, be_u64, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct SQLiteHeader {
    magic_string: [u8; 16],
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_fraction_empty: u8,
    min_fraction_empty: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size: u32,
    first_freelist_trunk_page: u32,
    total_freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_page_cache_size: u32,
    largest_root_btree: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum: u32,
    application_id: u32,
    reserved: [u8; 20],
    version_valid_for: u32,
    sqlite_version_number: u32,
}

#[derive(Debug)]
struct SQLiteDatabase {
    header: SQLiteHeader,
    pages: Vec<Vec<u8>>,
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    let (input, (
        magic_string,
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_fraction_empty,
        min_fraction_empty,
        leaf_payload_fraction,
        file_change_counter,
        database_size,
        first_freelist_trunk_page,
        total_freelist_pages,
        schema_cookie,
        schema_format,
        default_page_cache_size,
        largest_root_btree,
        text_encoding,
        user_version,
        incremental_vacuum,
        application_id,
        reserved,
        version_valid_for,
        sqlite_version_number,
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
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        take(20usize),
        be_u32,
        be_u32,
    ))(input)?;

    let mut header = SQLiteHeader {
        magic_string: [0; 16],
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_fraction_empty,
        min_fraction_empty,
        leaf_payload_fraction,
        file_change_counter,
        database_size,
        first_freelist_trunk_page,
        total_freelist_pages,
        schema_cookie,
        schema_format,
        default_page_cache_size,
        largest_root_btree,
        text_encoding,
        user_version,
        incremental_vacuum,
        application_id,
        reserved: [0; 20],
        version_valid_for,
        sqlite_version_number,
    };

    header.magic_string.copy_from_slice(magic_string);
    header.reserved.copy_from_slice(reserved);

    Ok((input, header))
}

fn parse_sqlite_database(input: &[u8]) -> IResult<&[u8], SQLiteDatabase> {
    let (input, header) = parse_sqlite_header(input)?;
    let page_size = if header.page_size == 1 {
        65536
    } else {
        header.page_size as usize
    };

    let mut pages = Vec::new();
    let mut remaining = input;

    while !remaining.is_empty() {
        let (input, page) = take(page_size)(remaining)?;
        pages.push(page.to_vec());
        remaining = input;
    }

    Ok((remaining, SQLiteDatabase { header, pages }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_sqlite_database(&buffer) {
        Ok((_, db)) => println!("Successfully parsed SQLite database: {:#?}", db),
        Err(e) => eprintln!("Failed to parse SQLite database: {:?}", e),
    }
}