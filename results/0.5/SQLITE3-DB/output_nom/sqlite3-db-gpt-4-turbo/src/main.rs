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
struct DatabaseHeader {
    header_string: String,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_payload_frac: u8,
    min_payload_frac: u8,
    leaf_payload_frac: u8,
    file_change_counter: u32,
    database_size: u32,
    first_freelist_page: u32,
    number_freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_cache_size: u32,
    largest_root_btree_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    reserved: Vec<u8>,
    version_valid_for: u32,
    sqlite_version: u32,
}

fn parse_database_header(input: &[u8]) -> IResult<&[u8], DatabaseHeader> {
    let (input, (
        header_string,
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_payload_frac,
        min_payload_frac,
        leaf_payload_frac,
        file_change_counter,
        database_size,
        first_freelist_page,
        number_freelist_pages,
        schema_cookie,
        schema_format,
        default_cache_size,
        largest_root_btree_page,
        text_encoding,
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
        DatabaseHeader {
            header_string: String::from_utf8_lossy(header_string).to_string(),
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_payload_frac,
            min_payload_frac,
            leaf_payload_frac,
            file_change_counter,
            database_size,
            first_freelist_page,
            number_freelist_pages,
            schema_cookie,
            schema_format,
            default_cache_size,
            largest_root_btree_page,
            text_encoding,
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
        println!("Usage: {} <SQLite3 db file path>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_database_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => println!("Failed to parse SQLite database header: {:?}", e),
    }

    Ok(())
}