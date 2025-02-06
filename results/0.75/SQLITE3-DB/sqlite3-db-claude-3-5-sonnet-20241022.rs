use nom::{
    bytes::complete::take,
    number::complete::{be_u32, be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs;

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
    Free,
    Invalid,
}

#[derive(Debug)]
enum Cell {
    InteriorTable {
        left_child: u32,
        key: u64,
    },
    InteriorIndex {
        left_child: u32,
        payload_size: u64,
        payload: Vec<u8>,
        overflow_page: Option<u32>,
    },
    LeafTable {
        payload_size: u64,
        row_id: u64,
        payload: Vec<u8>,
        overflow_page: Option<u32>,
    },
    LeafIndex {
        payload_size: u64,
        payload: Vec<u8>,
        overflow_page: Option<u32>,
    },
}

fn parse_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
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

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let mut result: u64 = 0;
    let mut shift = 0;
    let mut current_input = input;
    
    for _ in 0..9 {
        let (next_input, byte) = be_u8(current_input)?;
        current_input = next_input;
        
        result |= ((byte & 0x7F) as u64) << shift;
        if byte & 0x80 == 0 {
            return Ok((current_input, result));
        }
        shift += 7;
    }
    
    Ok((current_input, result))
}

fn parse_page_type(input: &[u8]) -> IResult<&[u8], PageType> {
    let (input, page_type) = be_u8(input)?;
    let page_type = match page_type {
        0x02 => PageType::InteriorIndex,
        0x05 => PageType::InteriorTable,
        0x0A => PageType::LeafIndex,
        0x0D => PageType::LeafTable,
        0x00 => PageType::Free,
        _ => PageType::Invalid,
    };
    Ok((input, page_type))
}

fn parse_cell<'a>(input: &'a [u8], page_type: &PageType) -> IResult<&'a [u8], Cell> {
    match page_type {
        PageType::InteriorTable => {
            let (input, left_child) = be_u32(input)?;
            let (input, key) = parse_varint(input)?;
            Ok((input, Cell::InteriorTable { left_child, key }))
        },
        PageType::InteriorIndex => {
            let (input, left_child) = be_u32(input)?;
            let (input, payload_size) = parse_varint(input)?;
            let (input, payload) = take(payload_size as usize)(input)?;
            let (input, overflow_page) = if payload_size > 0 {
                let (input, page) = be_u32(input)?;
                (input, Some(page))
            } else {
                (input, None)
            };
            Ok((input, Cell::InteriorIndex {
                left_child,
                payload_size,
                payload: payload.to_vec(),
                overflow_page,
            }))
        },
        PageType::LeafTable => {
            let (input, payload_size) = parse_varint(input)?;
            let (input, row_id) = parse_varint(input)?;
            let (input, payload) = take(payload_size as usize)(input)?;
            let (input, overflow_page) = if payload_size > 0 {
                let (input, page) = be_u32(input)?;
                (input, Some(page))
            } else {
                (input, None)
            };
            Ok((input, Cell::LeafTable {
                payload_size,
                row_id,
                payload: payload.to_vec(),
                overflow_page,
            }))
        },
        PageType::LeafIndex => {
            let (input, payload_size) = parse_varint(input)?;
            let (input, payload) = take(payload_size as usize)(input)?;
            let (input, overflow_page) = if payload_size > 0 {
                let (input, page) = be_u32(input)?;
                (input, Some(page))
            } else {
                (input, None)
            };
            Ok((input, Cell::LeafIndex {
                payload_size,
                payload: payload.to_vec(),
                overflow_page,
            }))
        },
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        return;
    }

    let data = match fs::read(&args[1]) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };

    match parse_header(&data) {
        Ok((_, header)) => println!("Successfully parsed header: {:?}", header),
        Err(e) => eprintln!("Error parsing header: {:?}", e),
    }
}