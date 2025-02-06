use nom::{
    bytes::complete::*,
    number::complete::*,
    multi::*,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct DatabaseHeader {
    magic: [u8; 16],
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_payload_fraction: u8,
    min_payload_fraction: u8,
    leaf_payload_fraction: u8,
    change_counter: u32,
    database_size: u32,
    first_freelist_trunk: u32,
    freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    suggested_cache_size: u32,
    largest_root_btree: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum: u32,
    application_id: u32,
    reserved: [u8; 20],
    version_valid: u32,
    sqlite_version: u32,
}

#[derive(Debug)]
enum PageType {
    InteriorIndex = 2,
    InteriorTable = 5,
    LeafIndex = 10,
    LeafTable = 13,
}

#[derive(Debug)]
struct BTreePage {
    page_type: PageType,
    first_freeblock: u16,
    cell_count: u16,
    cell_content_offset: u16,
    fragmented_free_bytes: u8,
    rightmost_pointer: Option<u32>,
    cells: Vec<Cell>,
}

#[derive(Debug)]
enum Cell {
    TableLeaf {
        payload_length: u64,
        row_id: u64,
        payload: Vec<u8>,
        overflow_page: Option<u32>,
    },
    TableInterior {
        left_child: u32,
        row_id: u64,
    },
    IndexLeaf {
        payload_length: u64,
        payload: Vec<u8>,
        overflow_page: Option<u32>,
    },
    IndexInterior {
        left_child: u32,
        payload_length: u64,
        payload: Vec<u8>,
        overflow_page: Option<u32>,
    },
}

fn parse_varint<'a>(input: &'a [u8]) -> IResult<&'a [u8], u64> {
    let (input, first) = be_u8(input)?;
    if first & 0x80 == 0 {
        return Ok((input, first as u64));
    }
    let mut result = (first & 0x7f) as u64;
    let mut shift = 7;
    let mut input = input;
    
    for _ in 0..8 {
        let (remaining, byte) = be_u8(input)?;
        input = remaining;
        result |= ((byte & 0x7f) as u64) << shift;
        if byte & 0x80 == 0 {
            return Ok((input, result));
        }
        shift += 7;
    }
    
    let (input, byte) = be_u8(input)?;
    result |= (byte as u64) << shift;
    Ok((input, result))
}

fn parse_header<'a>(input: &'a [u8]) -> IResult<&'a [u8], DatabaseHeader> {
    let (input, magic) = take(16usize)(input)?;
    let (input, page_size) = be_u16(input)?;
    let (input, write_version) = be_u8(input)?;
    let (input, read_version) = be_u8(input)?;
    let (input, reserved_space) = be_u8(input)?;
    let (input, max_payload_fraction) = be_u8(input)?;
    let (input, min_payload_fraction) = be_u8(input)?;
    let (input, leaf_payload_fraction) = be_u8(input)?;
    let (input, change_counter) = be_u32(input)?;
    let (input, database_size) = be_u32(input)?;
    let (input, first_freelist_trunk) = be_u32(input)?;
    let (input, freelist_pages) = be_u32(input)?;
    let (input, schema_cookie) = be_u32(input)?;
    let (input, schema_format) = be_u32(input)?;
    let (input, suggested_cache_size) = be_u32(input)?;
    let (input, largest_root_btree) = be_u32(input)?;
    let (input, text_encoding) = be_u32(input)?;
    let (input, user_version) = be_u32(input)?;
    let (input, incremental_vacuum) = be_u32(input)?;
    let (input, application_id) = be_u32(input)?;
    let (input, reserved) = take(20usize)(input)?;
    let (input, version_valid) = be_u32(input)?;
    let (input, sqlite_version) = be_u32(input)?;

    Ok((input, DatabaseHeader {
        magic: magic.try_into().unwrap(),
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_payload_fraction,
        min_payload_fraction,
        leaf_payload_fraction,
        change_counter,
        database_size,
        first_freelist_trunk,
        freelist_pages,
        schema_cookie,
        schema_format,
        suggested_cache_size,
        largest_root_btree,
        text_encoding,
        user_version,
        incremental_vacuum,
        application_id,
        reserved: reserved.try_into().unwrap(),
        version_valid,
        sqlite_version,
    }))
}

fn parse_btree_page<'a>(input: &'a [u8]) -> IResult<&'a [u8], BTreePage> {
    let (input, page_type_raw) = be_u8(input)?;
    let page_type = match page_type_raw {
        2 => PageType::InteriorIndex,
        5 => PageType::InteriorTable,
        10 => PageType::LeafIndex,
        13 => PageType::LeafTable,
        _ => panic!("Invalid page type"),
    };
    
    let (input, first_freeblock) = be_u16(input)?;
    let (input, cell_count) = be_u16(input)?;
    let (input, cell_content_offset) = be_u16(input)?;
    let (input, fragmented_free_bytes) = be_u8(input)?;
    
    let (input, rightmost_pointer) = match page_type {
        PageType::InteriorIndex | PageType::InteriorTable => {
            let (input, ptr) = be_u32(input)?;
            (input, Some(ptr))
        },
        _ => (input, None),
    };
    
    let (input, cells) = count(
        |input| parse_cell(input, &page_type),
        cell_count as usize
    )(input)?;

    Ok((input, BTreePage {
        page_type,
        first_freeblock,
        cell_count,
        cell_content_offset,
        fragmented_free_bytes,
        rightmost_pointer,
        cells,
    }))
}

fn parse_cell<'a>(input: &'a [u8], page_type: &PageType) -> IResult<&'a [u8], Cell> {
    match page_type {
        PageType::LeafTable => {
            let (input, payload_length) = parse_varint(input)?;
            let (input, row_id) = parse_varint(input)?;
            let (input, payload) = take(payload_length as usize)(input)?;
            let (input, overflow_page) = if payload_length > 0 {
                let (input, page) = be_u32(input)?;
                (input, Some(page))
            } else {
                (input, None)
            };
            Ok((input, Cell::TableLeaf {
                payload_length,
                row_id,
                payload: payload.to_vec(),
                overflow_page,
            }))
        },
        PageType::InteriorTable => {
            let (input, left_child) = be_u32(input)?;
            let (input, row_id) = parse_varint(input)?;
            Ok((input, Cell::TableInterior {
                left_child,
                row_id,
            }))
        },
        PageType::LeafIndex => {
            let (input, payload_length) = parse_varint(input)?;
            let (input, payload) = take(payload_length as usize)(input)?;
            let (input, overflow_page) = if payload_length > 0 {
                let (input, page) = be_u32(input)?;
                (input, Some(page))
            } else {
                (input, None)
            };
            Ok((input, Cell::IndexLeaf {
                payload_length,
                payload: payload.to_vec(),
                overflow_page,
            }))
        },
        PageType::InteriorIndex => {
            let (input, left_child) = be_u32(input)?;
            let (input, payload_length) = parse_varint(input)?;
            let (input, payload) = take(payload_length as usize)(input)?;
            let (input, overflow_page) = if payload_length > 0 {
                let (input, page) = be_u32(input)?;
                (input, Some(page))
            } else {
                (input, None)
            };
            Ok((input, Cell::IndexInterior {
                left_child,
                payload_length,
                payload: payload.to_vec(),
                overflow_page,
            }))
        },
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite-db-file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_header(&buffer) {
        Ok((remainder, header)) => {
            println!("Successfully parsed header: {:?}", header);
            
            match parse_btree_page(remainder) {
                Ok((_, page)) => {
                    println!("Successfully parsed first page: {:?}", page);
                }
                Err(e) => eprintln!("Error parsing first page: {:?}", e),
            }
        }
        Err(e) => eprintln!("Error parsing header: {:?}", e),
    }
}