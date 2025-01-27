use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::many0,
    number::complete::{be_i8, be_u16, be_u32, be_u64, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct SQLite3Header {
    magic_header: [u8; 16],
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
    freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    page_cache_size: u32,
    largest_root_page: u32,
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
    cells: Vec<Cell>,
}

#[derive(Debug)]
enum Cell {
    InteriorIndexCell {
        left_child_page: u32,
        payload_size: u64,
        payload: Vec<u8>,
        child_page: u32,
    },
    InteriorTableCell {
        left_child_page: u32,
        row_id: u64,
        child_page: u32,
    },
    LeafIndexCell {
        payload_size: u64,
        payload: Vec<u8>,
    },
    LeafTableCell {
        payload_size: u64,
        row_id: u64,
        payload: Vec<u8>,
    },
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first) = be_u8(input)?;
    if first & 0x80 == 0 {
        return Ok((input, first as u64));
    }
    let (input, second) = be_u8(input)?;
    if first & 0x40 == 0 {
        return Ok((input, ((first & 0x7f) as u64) << 7 | second as u64));
    }
    let mut result: u64 = ((first & 0x3f) as u64) << 14 | (second as u64) << 7;
    let mut shift = 21;
    let mut current = input;
    
    for _ in 0..7 {
        let (remaining, byte) = be_u8(current)?;
        result |= (byte as u64) << shift;
        if byte & 0x80 == 0 {
            return Ok((remaining, result));
        }
        shift += 7;
        current = remaining;
    }
    
    let (remaining, byte) = be_u8(current)?;
    result |= (byte as u64) << shift;
    Ok((remaining, result))
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SQLite3Header> {
    let (input, (
        magic_header,
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
        freelist_pages,
        schema_cookie,
        schema_format,
        page_cache_size,
        largest_root_page,
        text_encoding,
        user_version,
        incremental_vacuum,
        application_id,
        reserved,
        version_valid_for,
        sqlite_version,
    )) = tuple((
        take(16usize),
        be_u16,
        be_u8,
        be_u8,
        be_u8,
        be_u8,
        be_u8,
        be_u8,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        be_u32,
        take(20usize),
        be_u32,
        be_u32,
    ))(input)?;

    let mut magic = [0u8; 16];
    magic.copy_from_slice(magic_header);
    let mut res = [0u8; 20];
    res.copy_from_slice(reserved);

    Ok((input, SQLite3Header {
        magic_header: magic,
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
        freelist_pages,
        schema_cookie,
        schema_format,
        page_cache_size,
        largest_root_page,
        text_encoding,
        user_version,
        incremental_vacuum,
        application_id,
        reserved: res,
        version_valid_for,
        sqlite_version,
    }))
}

fn parse_btree_page(input: &[u8]) -> IResult<&[u8], BTreePage> {
    let (input, (
        page_type,
        first_freeblock,
        cell_count,
        cell_content_start,
        fragmented_free_bytes,
    )) = tuple((
        be_u8,
        be_u16,
        be_u16,
        be_u16,
        be_u8,
    ))(input)?;

    let (input, cells) = many0(parse_cell)(input)?;

    Ok((input, BTreePage {
        page_type,
        first_freeblock,
        cell_count,
        cell_content_start,
        fragmented_free_bytes,
        cells,
    }))
}

fn parse_cell(input: &[u8]) -> IResult<&[u8], Cell> {
    alt((
        parse_interior_index_cell,
        parse_interior_table_cell,
        parse_leaf_index_cell,
        parse_leaf_table_cell,
    ))(input)
}

fn parse_interior_index_cell(input: &[u8]) -> IResult<&[u8], Cell> {
    let (input, (
        left_child_page,
        payload_size,
        payload,
        child_page,
    )) = tuple((
        be_u32,
        parse_varint,
        take(0usize), // This should be adjusted based on payload_size
        be_u32,
    ))(input)?;

    Ok((input, Cell::InteriorIndexCell {
        left_child_page,
        payload_size,
        payload: payload.to_vec(),
        child_page,
    }))
}

fn parse_interior_table_cell(input: &[u8]) -> IResult<&[u8], Cell> {
    let (input, (
        left_child_page,
        row_id,
        child_page,
    )) = tuple((
        be_u32,
        parse_varint,
        be_u32,
    ))(input)?;

    Ok((input, Cell::InteriorTableCell {
        left_child_page,
        row_id,
        child_page,
    }))
}

fn parse_leaf_index_cell(input: &[u8]) -> IResult<&[u8], Cell> {
    let (input, payload_size) = parse_varint(input)?;
    let (input, payload) = take(payload_size as usize)(input)?;

    Ok((input, Cell::LeafIndexCell {
        payload_size,
        payload: payload.to_vec(),
    }))
}

fn parse_leaf_table_cell(input: &[u8]) -> IResult<&[u8], Cell> {
    let (input, (
        payload_size,
        row_id,
        payload,
    )) = tuple((
        parse_varint,
        parse_varint,
        take(0usize), // This should be adjusted based on payload_size
    ))(input)?;

    Ok((input, Cell::LeafTableCell {
        payload_size,
        row_id,
        payload: payload.to_vec(),
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_sqlite_header(&buffer) {
        Ok((remaining, header)) => {
            println!("SQLite Header: {:?}", header);
            match parse_btree_page(remaining) {
                Ok((_, page)) => println!("First B-tree page: {:?}", page),
                Err(e) => eprintln!("Error parsing B-tree page: {:?}", e),
            }
        }
        Err(e) => eprintln!("Error parsing header: {:?}", e),
    }

    Ok(())
}