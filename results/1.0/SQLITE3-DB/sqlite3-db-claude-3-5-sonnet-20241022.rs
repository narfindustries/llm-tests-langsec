use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, be_u64, le_u16, le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct SqliteHeader {
    header_string: [u8; 16],
    page_size: u16,
    write_version: u8,
    read_version: u8,
    unused_space: u8,
    max_fraction: u8,
    min_fraction: u8,
    leaf_payload: u8,
    file_change_counter: u32,
    db_size_pages: u32,
    first_freelist: u32,
    total_freelist: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_page_cache: u32,
    largest_root_btree: u32,
    text_encoding: u32,
    user_version: u32,
    vacuum_mode: u32,
    version_valid_for: u32,
    sqlite_version: u32,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
    let (input, (
        header_string,
        page_size,
        write_version,
        read_version,
        unused_space,
        max_fraction,
        min_fraction,
        leaf_payload,
        file_change_counter,
        db_size_pages,
        first_freelist,
        total_freelist,
        schema_cookie,
        schema_format,
        default_page_cache,
        largest_root_btree,
        text_encoding,
        user_version,
        vacuum_mode,
        version_valid_for,
        sqlite_version,
    )) = tuple((
        take(16usize),
        be_u16,
        take(1usize),
        take(1usize),
        take(1usize),
        take(1usize),
        take(1usize),
        take(1usize),
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
        be_u32,
    ))(input)?;

    let header = SqliteHeader {
        header_string: header_string.try_into().unwrap(),
        page_size,
        write_version: write_version[0],
        read_version: read_version[0],
        unused_space: unused_space[0],
        max_fraction: max_fraction[0],
        min_fraction: min_fraction[0],
        leaf_payload: leaf_payload[0],
        file_change_counter,
        db_size_pages,
        first_freelist,
        total_freelist,
        schema_cookie,
        schema_format,
        default_page_cache,
        largest_root_btree,
        text_encoding,
        user_version,
        vacuum_mode,
        version_valid_for,
        sqlite_version,
    };

    Ok((input, header))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite-file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Failed to parse SQLite header: {:?}", e),
    }

    Ok(())
}