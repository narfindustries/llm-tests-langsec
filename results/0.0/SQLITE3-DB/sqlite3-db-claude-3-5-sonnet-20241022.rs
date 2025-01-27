use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::many0,
    number::complete::{be_i32, be_i64, be_u16, be_u32, be_u64, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct SQLiteHeader {
    magic_string: [u8; 16],
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_fraction: u8,
    min_fraction: u8,
    leaf_payload: u8,
    file_change_counter: u32,
    database_size: u32,
    first_freelist_trunk: u32,
    total_freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_page_cache: u32,
    largest_root_btree: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum: u32,
    application_id: u32,
    reserved: [u8; 20],
    version_valid_for: u32,
    sqlite_version: u32,
}

#[derive(Debug)]
struct BTreePage {
    page_type: u8,
    first_freeblock: u16,
    cell_count: u16,
    cell_content_start: u16,
    fragmented_free_bytes: u8,
    cells: Vec<u16>,
    right_most_pointer: Option<u32>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    let (input, magic_string) = take(16usize)(input)?;
    let (input, page_size) = be_u16(input)?;
    let (input, write_version) = be_u8(input)?;
    let (input, read_version) = be_u8(input)?;
    let (input, reserved_space) = be_u8(input)?;
    let (input, max_fraction) = be_u8(input)?;
    let (input, min_fraction) = be_u8(input)?;
    let (input, leaf_payload) = be_u8(input)?;
    let (input, file_change_counter) = be_u32(input)?;
    let (input, database_size) = be_u32(input)?;
    let (input, first_freelist_trunk) = be_u32(input)?;
    let (input, total_freelist_pages) = be_u32(input)?;
    let (input, schema_cookie) = be_u32(input)?;
    let (input, schema_format) = be_u32(input)?;
    let (input, default_page_cache) = be_u32(input)?;
    let (input, largest_root_btree) = be_u32(input)?;
    let (input, text_encoding) = be_u32(input)?;
    let (input, user_version) = be_u32(input)?;
    let (input, incremental_vacuum) = be_u32(input)?;
    let (input, application_id) = be_u32(input)?;
    let (input, reserved) = take(20usize)(input)?;
    let (input, version_valid_for) = be_u32(input)?;
    let (input, sqlite_version) = be_u32(input)?;

    Ok((
        input,
        SQLiteHeader {
            magic_string: magic_string.try_into().unwrap(),
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_fraction,
            min_fraction,
            leaf_payload,
            file_change_counter,
            database_size,
            first_freelist_trunk,
            total_freelist_pages,
            schema_cookie,
            schema_format,
            default_page_cache,
            largest_root_btree,
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

fn parse_btree_page(input: &[u8]) -> IResult<&[u8], BTreePage> {
    let (input, page_type) = be_u8(input)?;
    let (input, first_freeblock) = be_u16(input)?;
    let (input, cell_count) = be_u16(input)?;
    let (input, cell_content_start) = be_u16(input)?;
    let (input, fragmented_free_bytes) = be_u8(input)?;
    let (input, right_most_pointer) = if page_type == 2 || page_type == 5 {
        let (input, ptr) = be_u32(input)?;
        (input, Some(ptr))
    } else {
        (input, None)
    };
    let (input, cells) = many0(be_u16)(input)?;

    Ok((
        input,
        BTreePage {
            page_type,
            first_freeblock,
            cell_count,
            cell_content_start,
            fragmented_free_bytes,
            cells,
            right_most_pointer,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_header(&buffer) {
        Ok((remaining, header)) => {
            println!("SQLite Header: {:?}", header);
            match parse_btree_page(remaining) {
                Ok((_, btree_page)) => {
                    println!("First B-tree page: {:?}", btree_page);
                }
                Err(e) => eprintln!("Failed to parse B-tree page: {:?}", e),
            }
        }
        Err(e) => eprintln!("Failed to parse header: {:?}", e),
    }
}