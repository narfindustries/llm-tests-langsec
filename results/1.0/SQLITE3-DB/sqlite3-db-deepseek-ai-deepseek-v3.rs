use nom::{
    bytes::complete::tag,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
    path::Path,
};

#[derive(Debug)]
struct SQLiteHeader {
    header_string: String,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_payload_fraction: u8,
    min_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    db_size_in_pages: u32,
    first_freelist_page: u32,
    total_freelist_pages: u32,
    schema_cookie: u32,
    schema_format_number: u32,
    default_cache_size: u32,
    largest_root_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum: u32,
    application_id: u32,
    version_valid_for: u32,
    sqlite_version: u32,
}

fn parse_header_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, header) = tag("SQLite format 3\0")(input)?;
    Ok((input, String::from_utf8_lossy(header).into_owned()))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    let (input, header_string) = parse_header_string(input)?;
    let (input, page_size) = be_u16(input)?;
    let (input, write_version) = be_u8(input)?;
    let (input, read_version) = be_u8(input)?;
    let (input, reserved_space) = be_u8(input)?;
    let (input, max_payload_fraction) = be_u8(input)?;
    let (input, min_payload_fraction) = be_u8(input)?;
    let (input, leaf_payload_fraction) = be_u8(input)?;
    let (input, file_change_counter) = be_u32(input)?;
    let (input, db_size_in_pages) = be_u32(input)?;
    let (input, first_freelist_page) = be_u32(input)?;
    let (input, total_freelist_pages) = be_u32(input)?;
    let (input, schema_cookie) = be_u32(input)?;
    let (input, schema_format_number) = be_u32(input)?;
    let (input, default_cache_size) = be_u32(input)?;
    let (input, largest_root_page) = be_u32(input)?;
    let (input, text_encoding) = be_u32(input)?;
    let (input, user_version) = be_u32(input)?;
    let (input, incremental_vacuum) = be_u32(input)?;
    let (input, application_id) = be_u32(input)?;
    let (input, _) = tag(&[0; 20])(input)?;
    let (input, version_valid_for) = be_u32(input)?;
    let (input, sqlite_version) = be_u32(input)?;

    Ok((
        input,
        SQLiteHeader {
            header_string,
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_payload_fraction,
            min_payload_fraction,
            leaf_payload_fraction,
            file_change_counter,
            db_size_in_pages,
            first_freelist_page,
            total_freelist_pages,
            schema_cookie,
            schema_format_number,
            default_cache_size,
            largest_root_page,
            text_encoding,
            user_version,
            incremental_vacuum,
            application_id,
            version_valid_for,
            sqlite_version,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        return Ok(());
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Failed to parse file: {:?}", e),
    }

    Ok(())
}