use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{length_data, many_till},
    number::complete::{be_u16, be_u32, be_u64},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
enum SQLite3PageType {
    Leaf,
    Interior,
    IndexLeaf,
    IndexInterior,
}

impl SQLite3PageType {
    fn from_value(value: u8) -> Option<Self> {
        match value {
            0x0d => Some(SQLite3PageType::Leaf),
            0x05 => Some(SQLite3PageType::Interior),
            0x0a => Some(SQLite3PageType::IndexLeaf),
            0x02 => Some(SQLite3PageType::IndexInterior),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct SQLite3PageHeader {
    page_type: SQLite3PageType,
    first_free_block: u16,
    cell_count: u16,
    cell_content_area_start: u16,
    fragmented_free_bytes: u8,
    right_child_page_number: u32,
}

fn parse_page_header(input: &[u8]) -> IResult<&[u8], SQLite3PageHeader> {
    let (input, page_type) = map_res(be_u8, SQLite3PageType::from_value)(input)?;
    let (input, first_free_block) = be_u16(input)?;
    let (input, cell_count) = be_u16(input)?;
    let (input, cell_content_area_start) = be_u16(input)?;
    let (input, fragmented_free_bytes) = be_u8(input)?;
    let (input, right_child_page_number) = be_u32(input)?;
    Ok((
        input,
        SQLite3PageHeader {
            page_type,
            first_free_block,
            cell_count,
            cell_content_area_start,
            fragmented_free_bytes,
            right_child_page_number,
        },
    ))
}

#[derive(Debug)]
struct SQLite3Page {
    page_header: SQLite3PageHeader,
    cells: Vec<SQLite3Cell>,
}

fn parse_page(input: &[u8]) -> IResult<&[u8], SQLite3Page> {
    let (input, page_header) = parse_page_header(input)?;
    let (input, cells) = many_till(
        parse_cell,
        tag(&[0xff; 1]),
    )(input)?;
    Ok((input, SQLite3Page { page_header, cells }))
}

#[derive(Debug)]
struct SQLite3Cell {
    payload_length: u16,
    payload: Vec<u8>,
}

fn parse_cell(input: &[u8]) -> IResult<&[u8], SQLite3Cell> {
    let (input, payload_length) = be_u16(input)?;
    let (input, payload) = take(payload_length)(input)?;
    Ok((input, SQLite3Cell { payload_length, payload }))
}

#[derive(Debug)]
struct SQLite3FileHeader {
    magic: [u8; 16],
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size_in_pages: u32,
    first_freelist_trunk_page: u32,
    total_freelist_pages: u32,
    schema_cookie: u32,
    schema_format_number: u32,
    default_page_cache_size: u32,
    largest_root_btree_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    reserved_for_expansion: [u8; 20],
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], SQLite3FileHeader> {
    let (input, magic) = take(16usize)(input)?;
    let (input, page_size) = be_u16(input)?;
    let (input, write_version) = be_u8(input)?;
    let (input, read_version) = be_u8(input)?;
    let (input, reserved_space) = be_u8(input)?;
    let (input, max_embedded_payload_fraction) = be_u8(input)?;
    let (input, min_embedded_payload_fraction) = be_u8(input)?;
    let (input, leaf_payload_fraction) = be_u8(input)?;
    let (input, file_change_counter) = be_u32(input)?;
    let (input, database_size_in_pages) = be_u32(input)?;
    let (input, first_freelist_trunk_page) = be_u32(input)?;
    let (input, total_freelist_pages) = be_u32(input)?;
    let (input, schema_cookie) = be_u32(input)?;
    let (input, schema_format_number) = be_u32(input)?;
    let (input, default_page_cache_size) = be_u32(input)?;
    let (input, largest_root_btree_page) = be_u32(input)?;
    let (input, text_encoding) = be_u32(input)?;
    let (input, user_version) = be_u32(input)?;
    let (input, incremental_vacuum_mode) = be_u32(input)?;
    let (input, application_id) = be_u32(input)?;
    let (input, reserved_for_expansion) = take(20usize)(input)?;
    Ok((
        input,
        SQLite3FileHeader {
            magic: magic.try_into().unwrap(),
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_embedded_payload_fraction,
            min_embedded_payload_fraction,
            leaf_payload_fraction,
            file_change_counter,
            database_size_in_pages,
            first_freelist_trunk_page,
            total_freelist_pages,
            schema_cookie,
            schema_format_number,
            default_page_cache_size,
            largest_root_btree_page,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            reserved_for_expansion: reserved_for_expansion.try_into().unwrap(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut data = Vec::new();
    reader.read_to_end(&mut data).unwrap();
    let (_remaining, file_header) = parse_file_header(&data).unwrap();
    println!("{:?}", file_header);
    let (_remaining, page) = parse_page(&data[100..]).unwrap();
    println!("{:?}", page);
}