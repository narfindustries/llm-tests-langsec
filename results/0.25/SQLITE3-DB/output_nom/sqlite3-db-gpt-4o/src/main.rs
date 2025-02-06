use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::env;

#[derive(Debug)]
struct SQLiteHeader {
    header_string: [u8; 16],
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
    freelist_page_count: u32,
    schema_cookie: u32,
    schema_format_number: u32,
    default_page_cache_size: u32,
    largest_root_btree_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    reserved_for_expansion: [u8; 20],
    version_valid_for_number: u32,
    sqlite_version_number: u32,
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    let (input, header_string) = map_res(take(16usize), |s: &[u8]| {
        let mut arr = [0u8; 16];
        arr.copy_from_slice(s);
        Ok::<_, nom::error::Error<&[u8]>>(arr)
    })(input)?;
    let (input, page_size) = be_u16(input)?;
    let (input, write_version) = be_u8(input)?;
    let (input, read_version) = be_u8(input)?;
    let (input, reserved_space) = be_u8(input)?;
    let (input, max_embedded_payload_fraction) = be_u8(input)?;
    let (input, min_embedded_payload_fraction) = be_u8(input)?;
    let (input, leaf_payload_fraction) = be_u8(input)?;
    let (input, file_change_counter) = be_u32(input)?;
    let (input, database_size) = be_u32(input)?;
    let (input, first_freelist_page) = be_u32(input)?;
    let (input, freelist_page_count) = be_u32(input)?;
    let (input, schema_cookie) = be_u32(input)?;
    let (input, schema_format_number) = be_u32(input)?;
    let (input, default_page_cache_size) = be_u32(input)?;
    let (input, largest_root_btree_page) = be_u32(input)?;
    let (input, text_encoding) = be_u32(input)?;
    let (input, user_version) = be_u32(input)?;
    let (input, incremental_vacuum_mode) = be_u32(input)?;
    let (input, application_id) = be_u32(input)?;
    let (input, reserved_for_expansion) = map_res(take(20usize), |s: &[u8]| {
        let mut arr = [0u8; 20];
        arr.copy_from_slice(s);
        Ok::<_, nom::error::Error<&[u8]>>(arr)
    })(input)?;
    let (input, version_valid_for_number) = be_u32(input)?;
    let (input, sqlite_version_number) = be_u32(input)?;

    Ok((
        input,
        SQLiteHeader {
            header_string,
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
            freelist_page_count,
            schema_cookie,
            schema_format_number,
            default_page_cache_size,
            largest_root_btree_page,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            reserved_for_expansion,
            version_valid_for_number,
            sqlite_version_number,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite3-db-file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_sqlite_header(&buffer) {
        Ok((_, header)) => {
            println!("{:?}", header);
        }
        Err(e) => {
            eprintln!("Failed to parse SQLite header: {:?}", e);
        }
    }
}