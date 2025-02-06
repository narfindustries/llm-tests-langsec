use nom::{
    bytes::complete::tag,
    number::complete::{be_u16, be_u32, be_u64, le_u16, le_u32, le_u64, u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct DatabaseHeader {
    header_string: String,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_bytes: u8,
    max_embedded_fraction: u8,
    min_embedded_fraction: u8,
    leaf_fraction: u8,
    file_change_counter: u32,
    database_size: u32,
    first_freelist_page: u32,
    freelist_page_count: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_cache_size: u32,
    auto_vacuum: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum: u32,
    application_id: u32,
    reserved: [u8; 20],
    version_valid_for: u32,
    sqlite_version: u32,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DatabaseHeader> {
    let (input, header_string) = nom::bytes::complete::take(16usize)(input)?;
    let (input, page_size) = le_u16(input)?;
    let (input, write_version) = u8(input)?;
    let (input, read_version) = u8(input)?;
    let (input, reserved_bytes) = u8(input)?;
    let (input, max_embedded_fraction) = u8(input)?;
    let (input, min_embedded_fraction) = u8(input)?;
    let (input, leaf_fraction) = u8(input)?;
    let (input, file_change_counter) = le_u32(input)?;
    let (input, database_size) = le_u32(input)?;
    let (input, first_freelist_page) = le_u32(input)?;
    let (input, freelist_page_count) = le_u32(input)?;
    let (input, schema_cookie) = le_u32(input)?;
    let (input, schema_format) = le_u32(input)?;
    let (input, default_cache_size) = le_u32(input)?;
    let (input, auto_vacuum) = le_u32(input)?;
    let (input, text_encoding) = le_u32(input)?;
    let (input, user_version) = le_u32(input)?;
    let (input, incremental_vacuum) = le_u32(input)?;
    let (input, application_id) = le_u32(input)?;
    let (input, reserved) = nom::bytes::complete::take(20usize)(input)?;
    let (input, version_valid_for) = le_u32(input)?;
    let (input, sqlite_version) = le_u32(input)?;

    Ok((
        input,
        DatabaseHeader {
            header_string: String::from_utf8_lossy(header_string).to_string(),
            page_size,
            write_version,
            read_version,
            reserved_bytes,
            max_embedded_fraction,
            min_embedded_fraction,
            leaf_fraction,
            file_change_counter,
            database_size,
            first_freelist_page,
            freelist_page_count,
            schema_cookie,
            schema_format,
            default_cache_size,
            auto_vacuum,
            text_encoding,
            user_version,
            incremental_vacuum,
            application_id,
            reserved: reserved.try_into().unwrap(),
            version_valid_for,
            sqlite_version,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Failed to parse header: {:?}", e),
    }
}