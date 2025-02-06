use nom::{
    bytes::complete::{take},
    combinator::{map_res},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct SqliteHeader {
    header_string: String,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_payload_frac: u8,
    min_payload_frac: u8,
    leaf_payload_frac: u8,
    change_counter: u32,
    file_size: u32,
    first_freelist_page: u32,
    freelist_page_count: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_cache_size: u32,
    largest_btree_page: u32,
    text_encoding: u32,
    user_version: u32,
    vacuum_mode: u32,
    application_id: u32,
    reserved_expansion: Vec<u8>,
    version_valid_for: u32,
    sqlite_version_number: u32,
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
    let (
        input,
        (
            header_string,
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_payload_frac,
            min_payload_frac,
            leaf_payload_frac,
            change_counter,
            file_size,
            first_freelist_page,
            freelist_page_count,
            schema_cookie,
            schema_format,
            default_cache_size,
            largest_btree_page,
            text_encoding,
            user_version,
            vacuum_mode,
            application_id,
            reserved_expansion,
            version_valid_for,
            sqlite_version_number,
        ),
    ) = tuple((
        map_res(take(16usize), std::str::from_utf8),
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
        SqliteHeader {
            header_string: header_string.to_string(),
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_payload_frac,
            min_payload_frac,
            leaf_payload_frac,
            change_counter,
            file_size,
            first_freelist_page,
            freelist_page_count,
            schema_cookie,
            schema_format,
            default_cache_size,
            largest_btree_page,
            text_encoding,
            user_version,
            vacuum_mode,
            application_id,
            reserved_expansion: reserved_expansion.to_vec(),
            version_valid_for,
            sqlite_version_number,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Please provide exactly one argument, the path to the SQLite file.",
        ));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_sqlite_header(&buffer) {
        Ok((_remaining, header)) => {
            println!("{:?}", header);
        }
        Err(e) => {
            eprintln!("Failed to parse SQLite header: {:?}", e);
        }
    }

    Ok(())
}