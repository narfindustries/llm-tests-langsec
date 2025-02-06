use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct SqliteHeader {
    magic: [u8; 16],
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_payload_fraction: u8,
    min_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    db_size_pages: u32,
    first_freelist_trunk: u32,
    freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    page_cache_size: u32,
    largest_root_page: u32,
    text_encoding: u32,
    user_version: u32,
    vacuum_mode: u32,
    app_id: u32,
    reserved: [u8; 20],
    version_valid: u32,
    sqlite_version: u32,
}

#[derive(Debug)]
enum PageType {
    InteriorIndex,
    InteriorTable,
    LeafIndex,
    LeafTable,
    Overflow,
}

#[derive(Debug)]
struct BTreePageHeader {
    page_type: PageType,
    first_freeblock: u16,
    cell_count: u16,
    cell_content_offset: u16,
    fragmented_free_bytes: u8,
    right_child: Option<u32>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let mut result: u64 = 0;
    let mut shift = 0;
    let mut remaining = input;
    
    for _ in 0..9 {
        let (rest, byte) = be_u8(remaining)?;
        remaining = rest;
        
        result |= ((byte & 0x7F) as u64) << shift;
        if byte & 0x80 == 0 {
            return Ok((remaining, result));
        }
        shift += 7;
    }
    
    let (rest, byte) = be_u8(remaining)?;
    result |= (byte as u64) << shift;
    Ok((rest, result))
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
    let (input, magic) = take(16usize)(input)?;
    let (input, page_size) = be_u16(input)?;
    let (input, write_version) = be_u8(input)?;
    let (input, read_version) = be_u8(input)?;
    let (input, reserved_space) = be_u8(input)?;
    let (input, max_payload_fraction) = be_u8(input)?;
    let (input, min_payload_fraction) = be_u8(input)?;
    let (input, leaf_payload_fraction) = be_u8(input)?;
    let (input, file_change_counter) = be_u32(input)?;
    let (input, db_size_pages) = be_u32(input)?;
    let (input, first_freelist_trunk) = be_u32(input)?;
    let (input, freelist_pages) = be_u32(input)?;
    let (input, schema_cookie) = be_u32(input)?;
    let (input, schema_format) = be_u32(input)?;
    let (input, page_cache_size) = be_u32(input)?;
    let (input, largest_root_page) = be_u32(input)?;
    let (input, text_encoding) = be_u32(input)?;
    let (input, user_version) = be_u32(input)?;
    let (input, vacuum_mode) = be_u32(input)?;
    let (input, app_id) = be_u32(input)?;
    let (input, reserved) = take(20usize)(input)?;
    let (input, version_valid) = be_u32(input)?;
    let (input, sqlite_version) = be_u32(input)?;

    let mut magic_arr = [0u8; 16];
    magic_arr.copy_from_slice(magic);
    
    let mut reserved_arr = [0u8; 20];
    reserved_arr.copy_from_slice(reserved);

    Ok((input, SqliteHeader {
        magic: magic_arr,
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_payload_fraction,
        min_payload_fraction,
        leaf_payload_fraction,
        file_change_counter,
        db_size_pages,
        first_freelist_trunk,
        freelist_pages,
        schema_cookie,
        schema_format,
        page_cache_size,
        largest_root_page,
        text_encoding,
        user_version,
        vacuum_mode,
        app_id,
        reserved: reserved_arr,
        version_valid,
        sqlite_version,
    }))
}

fn parse_btree_page_header(input: &[u8]) -> IResult<&[u8], BTreePageHeader> {
    let (input, page_type_byte) = be_u8(input)?;
    let page_type = match page_type_byte {
        2 => PageType::InteriorIndex,
        5 => PageType::InteriorTable,
        10 => PageType::LeafIndex,
        13 => PageType::LeafTable,
        _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    };

    let (input, first_freeblock) = be_u16(input)?;
    let (input, cell_count) = be_u16(input)?;
    let (input, cell_content_offset) = be_u16(input)?;
    let (input, fragmented_free_bytes) = be_u8(input)?;

    let (input, right_child) = match page_type {
        PageType::InteriorIndex | PageType::InteriorTable => {
            let (input, val) = be_u32(input)?;
            (input, Some(val))
        },
        _ => (input, None),
    };

    Ok((input, BTreePageHeader {
        page_type,
        first_freeblock,
        cell_count,
        cell_content_offset,
        fragmented_free_bytes,
        right_child,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_sqlite_header(&buffer) {
        Ok((remaining, header)) => {
            println!("SQLite Header: {:#?}", header);
            
            if !remaining.is_empty() {
                match parse_btree_page_header(remaining) {
                    Ok((_, page_header)) => {
                        println!("First Page Header: {:#?}", page_header);
                    },
                    Err(e) => eprintln!("Error parsing page header: {:?}", e),
                }
            }
        },
        Err(e) => eprintln!("Error parsing SQLite header: {:?}", e),
    }
}