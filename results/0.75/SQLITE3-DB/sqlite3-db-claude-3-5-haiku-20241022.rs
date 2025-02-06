use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::{le_u8, le_u16, le_u32},
    sequence::tuple,
    combinator::{map, opt},
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct SQLiteHeader {
    magic: [u8; 16],
    page_size: u16,
    write_format: u8,
    read_format: u8,
    reserved_bytes: u8,
    max_payload_fraction: u8,
    min_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size_pages: u32,
    first_freelist_page: u32,
    total_freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_encoding: u32,
    user_version: u16,
    incremental_vacuum_mode: u8,
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    map(
        tuple((
            take(16usize),
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
            le_u16,
            le_u8
        )),
        |(magic, page_size, write_format, read_format, reserved_bytes, 
          max_payload_fraction, min_payload_fraction, leaf_payload_fraction,
          file_change_counter, database_size_pages, first_freelist_page, 
          total_freelist_pages, schema_cookie, schema_format, 
          default_encoding, user_version, incremental_vacuum_mode)| {
            let mut magic_arr = [0u8; 16];
            magic_arr.copy_from_slice(magic);
            SQLiteHeader {
                magic: magic_arr,
                page_size,
                write_format,
                read_format,
                reserved_bytes,
                max_payload_fraction,
                min_payload_fraction,
                leaf_payload_fraction,
                file_change_counter,
                database_size_pages,
                first_freelist_page,
                total_freelist_pages,
                schema_cookie,
                schema_format,
                default_encoding,
                user_version,
                incremental_vacuum_mode
            }
        }
    )(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = [0; 100];
    file.read_exact(&mut buffer)?;

    match parse_sqlite_header(&buffer) {
        Ok((_, header)) => {
            println!("SQLite Header parsed successfully:");
            println!("Magic Header: {:?}", header.magic);
            println!("Page Size: {}", header.page_size);
            println!("Write Format: {}", header.write_format);
            println!("Database Size (pages): {}", header.database_size_pages);
        },
        Err(e) => {
            eprintln!("Parsing failed: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}