use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{length_data},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, BufReader};
use std::path::Path;

#[derive(Debug)]
enum CellType {
    Integer,
    Float,
    Text,
    Blob,
    Null,
    Reserved,
}

impl CellType {
    fn from_u8(n: u8) -> Result<Self, String> {
        match n {
            0 => Ok(CellType::Integer),
            1 => Ok(CellType::Float),
            2 => Ok(CellType::Text),
            3 => Ok(CellType::Blob),
            4 => Ok(CellType::Null),
            5 => Ok(CellType::Reserved),
            _ => Err(format!("Invalid cell type: {}", n)),
        }
    }
}

#[derive(Debug)]
struct Cell {
    payload_length: u32,
    header_length: u8,
    num_header_bytes: u8,
    cell_type: CellType,
    payload: Vec<u8>,
}

impl Cell {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, payload_length) = be_u32(input)?;
        let (input, header_length) = be_u8(input)?;
        let (input, num_header_bytes) = be_u8(input)?;
        let (input, cell_type) = map_res(be_u8, |n| CellType::from_u8(n))(input)?;
        let (input, payload) = take(payload_length as usize)(input)?;
        Ok((input, Cell {
            payload_length,
            header_length,
            num_header_bytes,
            cell_type,
            payload: payload.to_vec(),
        }))
    }
}

#[derive(Debug)]
enum PageType {
    Interior,
    Leaf,
    LeafIndex,
    Overflow,
    PointerMap,
}

impl PageType {
    fn from_u8(n: u8) -> Result<Self, String> {
        match n {
            2 => Ok(PageType::Interior),
            3 => Ok(PageType::Leaf),
            4 => Ok(PageType::LeafIndex),
            5 => Ok(PageType::Overflow),
            6 => Ok(PageType::PointerMap),
            _ => Err(format!("Invalid page type: {}", n)),
        }
    }
}

#[derive(Debug)]
struct BTreePage {
    page_type: PageType,
    first_freeblock: u16,
    number_of_cells: u16,
    start_of_content_area: u16,
    fragmented_free_bytes: u8,
    right_child_page: u32,
    cell_content_area: Vec<Cell>,
}

impl BTreePage {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, page_type) = map_res(be_u8, |n| PageType::from_u8(n))(input)?;
        let (input, first_freeblock) = be_u16(input)?;
        let (input, number_of_cells) = be_u16(input)?;
        let (input, start_of_content_area) = be_u16(input)?;
        let (input, fragmented_free_bytes) = be_u8(input)?;
        let (input, right_child_page) = be_u32(input)?;
        let (input, cell_content_area) = length_data(be_u16)(input)?;
        let mut cells = Vec::new();
        let mut remaining = cell_content_area;
        while !remaining.is_empty() {
            let (new_remaining, cell) = Cell::parse(remaining)?;
            cells.push(cell);
            remaining = new_remaining;
        }
        Ok((input, BTreePage {
            page_type,
            first_freeblock,
            number_of_cells,
            start_of_content_area,
            fragmented_free_bytes,
            right_child_page,
            cell_content_area: cells,
        }))
    }
}

#[derive(Debug)]
struct FileHeader {
    magic: [u8; 16],
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size: u32,
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

impl FileHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, magic) = take(16usize)(input)?;
        let (input, page_size) = be_u16(input)?;
        let (input, write_version) = be_u8(input)?;
        let (input, read_version) = be_u8(input)?;
        let (input, reserved_space) = be_u8(input)?;
        let (input, max_embedded_payload_fraction) = be_u8(input)?;
        let (input, min_embedded_payload_fraction) = be_u8(input)?;
        let (input, leaf_payload_fraction) = be_u8(input)?;
        let (input, file_change_counter) = be_u32(input)?;
        let (input, database_size) = be_u32(input)?;
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
        Ok((input, FileHeader {
            magic: magic.try_into().unwrap(),
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_embedded_payload_fraction,
            min_embedded_payload_fraction,
            leaf_payload_fraction,
            file_change_counter,
            database_size,
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
        }))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }
    let file_path = Path::new(&args[1]);
    let file = File::open(file_path).unwrap();
    let mut reader = BufReader::new(file);
    let mut data = Vec::new();
    reader.read_to_end(&mut data).unwrap();
    let (_remaining, file_header) = FileHeader::parse(&data).unwrap();
    println!("{:?}", file_header);
    let (_remaining, btree_page) = BTreePage::parse(&data[100..]).unwrap();
    println!("{:?}", btree_page);
}