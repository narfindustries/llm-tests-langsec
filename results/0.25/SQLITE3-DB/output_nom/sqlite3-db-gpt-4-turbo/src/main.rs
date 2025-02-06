use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct SQLiteHeader {
    signature: String,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_payload_frac: u8,
    min_payload_frac: u8,
    leaf_payload_frac: u8,
    change_counter: u32,
    db_size: u32,
    first_freelist_trunk_page: u32,
    freelist_page_count: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_cache_size: u32,
    largest_root_page: u32,
    db_text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    reserved: Vec<u8>,
    version_valid_for: u32,
    sqlite_version: u32,
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    let (input, (
        signature,
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_payload_frac,
        min_payload_frac,
        leaf_payload_frac,
        change_counter,
        db_size,
        first_freelist_trunk_page,
        freelist_page_count,
        schema_cookie,
        schema_format,
        default_cache_size,
        largest_root_page,
        db_text_encoding,
        user_version,
        incremental_vacuum_mode,
        application_id,
        reserved,
        version_valid_for,
        sqlite_version,
    )) = tuple((
        tag("SQLite format 3\0"),
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
        le_u32,
        le_u32,
        le_u32,
        le_u32,
        le_u32,
        take(20usize),
        le_u32,
        le_u32,
    ))(input)?;

    Ok((
        input,
        SQLiteHeader {
            signature: String::from_utf8_lossy(signature).to_string(),
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_payload_frac,
            min_payload_frac,
            leaf_payload_frac,
            change_counter,
            db_size,
            first_freelist_trunk_page,
            freelist_page_count,
            schema_cookie,
            schema_format,
            default_cache_size,
            largest_root_page,
            db_text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            reserved: reserved.to_vec(),
            version_valid_for,
            sqlite_version,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <SQLite file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_sqlite_header(&buffer) {
        Ok((_, header)) => {
            println!("{:#?}", header);
        }
        Err(e) => {
            eprintln!("Failed to parse SQLite header: {:?}", e);
        }
    }

    Ok(())
}