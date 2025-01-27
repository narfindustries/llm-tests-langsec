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
    free_list_trunk_page: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_page_cache_size: u32,
    largest_btree_page: u32,
    file_format_write_version: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    version_valid_for: u32,
}


fn parse_header(input: &[u8]) -> IResult<&[u8], Sqlite3Header> {
    map(
        tuple((
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
            le_u32,
            le_u32,
            le_u32,
            le_u32,
            le_u32,
            le_u32,
            le_u32,
        )),
        |(magic, page_size, write_version, read_version, reserved_space, max_page_count, change_count, sector_size, free_list_trunk_page, schema_cookie, schema_format, default_page_cache_size, largest_btree_page, file_format_write_version, text_encoding, user_version, incremental_vacuum_mode, application_id, version_valid_for)| Sqlite3Header {
            magic,
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_page_count,
            change_count,
            sector_size,
            free_list_trunk_page,
            schema_cookie,
            schema_format,
            default_page_cache_size,
            largest_btree_page,
            file_format_write_version,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            version_valid_for,
        },
    )(input)
}


fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
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
    if let Err(err) = file.read_to_end(&mut buffer) {
        eprintln!("Failed to read file {}: {}", filename, err);
        process::exit(1);
    }

    match parse_header(&buffer) {
        Ok((remaining, header)) => {
            println!("Parsed header: {:?}", header);
            println!("Remaining bytes: {:?}", remaining);
        }
        Err(e) => {
            println!("Error parsing header: {:?}", e);
            process::exit(1);

        }
    }
}
