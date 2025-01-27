use nom::{
    bytes::complete::{tag, take_while, take_while1},
    combinator::{map, map_res, opt},
    error::ErrorKind,
    number::complete::le_u32,
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::process;

#[derive(Debug)]
struct Sqlite3Header {
    magic: u32,
    page_size: u32,
    write_version: u32,
    read_version: u32,
    reserved_space: u32,
    max_page_count: u32,
    change_count: u32,
    sector_size: u32,
    free_page_count: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_page_cache_size: u32,
    largest_btree_page: u32,
    file_format_write_version: u32,
    source_id: u32,
    application_id: u32,
    version_valid_for_all_schema_and_file_format: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_hma_key: [u8; 20], //simplified
    unused1: u32,
    unused2: u32,
    unused3: u32,
    unused4: u32,
    unused5: u32,
    unused6: u32,
    unused7: u32,
    unused8: u32,
    unused9: u32,
    unused10: u32,
}

fn parse_sqlite3_header(input: &[u8]) -> IResult<&[u8], Sqlite3Header> {
    let (input, magic) = le_u32(input)?;
    let (input, page_size) = le_u32(input)?;
    let (input, write_version) = le_u32(input)?;
    let (input, read_version) = le_u32(input)?;
    let (input, reserved_space) = le_u32(input)?;
    let (input, max_page_count) = le_u32(input)?;
    let (input, change_count) = le_u32(input)?;
    let (input, sector_size) = le_u32(input)?;
    let (input, free_page_count) = le_u32(input)?;
    let (input, schema_cookie) = le_u32(input)?;
    let (input, schema_format) = le_u32(input)?;
    let (input, default_page_cache_size) = le_u32(input)?;
    let (input, largest_btree_page) = le_u32(input)?;
    let (input, file_format_write_version) = le_u32(input)?;
    let (input, source_id) = le_u32(input)?;
    let (input, application_id) = le_u32(input)?;
    let (input, version_valid_for_all_schema_and_file_format) = le_u32(input)?;
    let (input, text_encoding) = le_u32(input)?;
    let (input, user_version) = le_u32(input)?;
    let (input, incremental_vacuum_mode) = le_u32(input)?;
    let (input, application_hma_key) = take_while(|b| b != &0)(input)?;
    let application_hma_key: [u8; 20] = application_hma_key.try_into().unwrap();
    let (input, unused1) = le_u32(input)?;
    let (input, unused2) = le_u32(input)?;
    let (input, unused3) = le_u32(input)?;
    let (input, unused4) = le_u32(input)?;
    let (input, unused5) = le_u32(input)?;
    let (input, unused6) = le_u32(input)?;
    let (input, unused7) = le_u32(input)?;
    let (input, unused8) = le_u32(input)?;
    let (input, unused9) = le_u32(input)?;
    let (input, unused10) = le_u32(input)?;

    Ok((
        input,
        Sqlite3Header {
            magic,
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_page_count,
            change_count,
            sector_size,
            free_page_count,
            schema_cookie,
            schema_format,
            default_page_cache_size,
            largest_btree_page,
            file_format_write_version,
            source_id,
            application_id,
            version_valid_for_all_schema_and_file_format,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_hma_key,
            unused1,
            unused2,
            unused3,
            unused4,
            unused5,
            unused6,
            unused7,
            unused8,
            unused9,
            unused10,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Failed to open file {}: {}", filename, err);
            process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Failed to read file {}: {}", filename, err);
            process::exit(1);
        }
    };

    match parse_sqlite3_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => {
            eprintln!("Failed to parse header: {:?}", e);
            process::exit(1);
        }
    }
}
