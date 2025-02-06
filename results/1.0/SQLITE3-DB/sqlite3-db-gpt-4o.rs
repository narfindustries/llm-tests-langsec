use nom::{
    bytes::complete::tag,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

// Parsing constants
const SQLITE_HEADER_STRING: &[u8; 16] = b"SQLite format 3\0";

#[derive(Debug)]
struct SQLiteHeader {
    page_size: u16,
    file_format_write_version: u8,
    file_format_read_version: u8,
    reserved_space: u8,
    max_emb_payload_frac: u8,
    min_emb_payload_frac: u8,
    leaf_payload_frac: u8,
    file_change_counter: u32,
    db_size_pages: u32,
    first_freelist_page: u32,
    total_freelist_pages: u32,
    schema_cookie: u32,
    schema_format_number: u32,
    default_page_cache_size: u32,
    largest_root_btree_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    reserved: [u8; 20],
    version_valid_for: u32,
    sqlite_version_number: u32,
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    let (input, _) = tag(SQLITE_HEADER_STRING)(input)?;
    let (input, page_size) = be_u16(input)?;
    let (input, file_format_write_version) = be_u8(input)?;
    let (input, file_format_read_version) = be_u8(input)?;
    let (input, reserved_space) = be_u8(input)?;
    let (input, max_emb_payload_frac) = be_u8(input)?;
    let (input, min_emb_payload_frac) = be_u8(input)?;
    let (input, leaf_payload_frac) = be_u8(input)?;
    let (input, file_change_counter) = be_u32(input)?;
    let (input, db_size_pages) = be_u32(input)?;
    let (input, first_freelist_page) = be_u32(input)?;
    let (input, total_freelist_pages) = be_u32(input)?;
    let (input, schema_cookie) = be_u32(input)?;
    let (input, schema_format_number) = be_u32(input)?;
    let (input, default_page_cache_size) = be_u32(input)?;
    let (input, largest_root_btree_page) = be_u32(input)?;
    let (input, text_encoding) = be_u32(input)?;
    let (input, user_version) = be_u32(input)?;
    let (input, incremental_vacuum_mode) = be_u32(input)?;
    let (input, application_id) = be_u32(input)?;
    let (input, reserved) = nom::bytes::complete::take(20usize)(input)?;
    let (input, version_valid_for) = be_u32(input)?;
    let (input, sqlite_version_number) = be_u32(input)?;

    Ok((
        input,
        SQLiteHeader {
            page_size,
            file_format_write_version,
            file_format_read_version,
            reserved_space,
            max_emb_payload_frac,
            min_emb_payload_frac,
            leaf_payload_frac,
            file_change_counter,
            db_size_pages,
            first_freelist_page,
            total_freelist_pages,
            schema_cookie,
            schema_format_number,
            default_page_cache_size,
            largest_root_btree_page,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            reserved: {
                let mut array = [0; 20];
                array.copy_from_slice(reserved);
                array
            },
            version_valid_for,
            sqlite_version_number,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    if !path.exists() || !path.is_file() {
        eprintln!("File does not exist.");
        return;
    }

    let mut file = File::open(&path).expect("Failed to open file");
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).expect("Failed to read file");

    match parse_sqlite_header(&buf) {
        Ok((_, header)) => {
            println!("{:?}", header);
        }
        Err(e) => {
            eprintln!("Failed to parse SQLite header: {:?}", e);
        }
    }
}