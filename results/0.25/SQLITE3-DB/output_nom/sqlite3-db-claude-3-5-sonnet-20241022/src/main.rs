use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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
    file_change_counter: u32,
    db_size_pages: u32,
    first_freelist_trunk: u32,
    freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_page_cache: u32,
    largest_root_btree: u32,
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
    BTreeInteriorIndex,
    BTreeInteriorTable,
    BTreeLeafIndex,
    BTreeLeafTable,
    Overflow,
    Freelist,
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

fn parse_header(input: &[u8]) -> IResult<&[u8], DatabaseHeader> {
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
    let (input, default_page_cache) = be_u32(input)?;
    let (input, largest_root_btree) = be_u32(input)?;
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

    Ok((
        input,
        DatabaseHeader {
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
            default_page_cache,
            largest_root_btree,
            text_encoding,
            user_version,
            vacuum_mode,
            app_id,
            reserved: reserved_arr,
            version_valid,
            sqlite_version,
        },
    ))
}

fn parse_page_type(input: &[u8]) -> IResult<&[u8], PageType> {
    let (input, type_byte) = be_u8(input)?;
    let page_type = match type_byte {
        0x02 => PageType::BTreeInteriorIndex,
        0x05 => PageType::BTreeInteriorTable,
        0x0a => PageType::BTreeLeafIndex,
        0x0d => PageType::BTreeLeafTable,
        0x00 => PageType::Overflow, // or Freelist, context dependent
        _ => return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    };
    Ok((input, page_type))
}

fn parse_btree_page_header(input: &[u8]) -> IResult<&[u8], BTreePageHeader> {
    let (input, page_type) = parse_page_type(input)?;
    let (input, first_freeblock) = be_u16(input)?;
    let (input, cell_count) = be_u16(input)?;
    let (input, cell_content_offset) = be_u16(input)?;
    let (input, fragmented_free_bytes) = be_u8(input)?;
    
    let (input, right_child) = match page_type {
        PageType::BTreeInteriorIndex | PageType::BTreeInteriorTable => {
            let (input, right_child) = be_u32(input)?;
            (input, Some(right_child))
        }
        _ => (input, None),
    };

    Ok((
        input,
        BTreePageHeader {
            page_type,
            first_freeblock,
            cell_count,
            cell_content_offset,
            fragmented_free_bytes,
            right_child,
        },
    ))
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let mut result: u64 = 0;
    let mut shift = 0;
    let mut current_input = input;
    
    for _ in 0..9 {
        let (new_input, byte) = be_u8(current_input)?;
        current_input = new_input;
        
        result |= ((byte & 0x7F) as u64) << shift;
        if byte & 0x80 == 0 {
            return Ok((current_input, result));
        }
        shift += 7;
    }
    
    Err(nom::Err::Error(nom::error::Error::new(
        input,
        nom::error::ErrorKind::TooLarge,
    )))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite-db-file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_header(&buffer) {
        Ok((_, header)) => println!("Database Header: {:#?}", header),
        Err(e) => eprintln!("Failed to parse header: {:?}", e),
    }
}