use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map_res},
    error::{Error, ErrorKind},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read};
use std::path::Path;

#[derive(Debug, PartialEq)]
enum PageType {
    IndexInterior,
    TableInterior,
    TableLeaf,
    IndexLeaf,
}

impl PageType {
    fn from_u8(n: u8) -> Result<PageType, Error<Vec<u8>>> {
        match n {
            2 => Ok(PageType::IndexInterior),
            5 => Ok(PageType::TableInterior),
            10 => Ok(PageType::TableLeaf),
            13 => Ok(PageType::IndexLeaf),
            _ => Err(nom::Err::Error((n.to_le_bytes().to_vec(), ErrorKind::AlphaNumeric))),
        }
    }
}

#[derive(Debug, PartialEq)]
struct FileHeader {
    magic: [u8; 16],
    file_format_write_version: u16,
    file_format_read_version: u16,
    reserved_space: u16,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    in_header_db_size: u32,
    first_freelist_trunk_page: u32,
    total_change_count: u32,
    free_page_list: u32,
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

fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, magic) = take(16usize)(input)?;
    let (input, file_format_write_version) = be_u16(input)?;
    let (input, file_format_read_version) = be_u16(input)?;
    let (input, reserved_space) = be_u16(input)?;
    let (input, max_embedded_payload_fraction) = be_u8(input)?;
    let (input, min_embedded_payload_fraction) = be_u8(input)?;
    let (input, leaf_payload_fraction) = be_u8(input)?;
    let (input, file_change_counter) = be_u32(input)?;
    let (input, in_header_db_size) = be_u32(input)?;
    let (input, first_freelist_trunk_page) = be_u32(input)?;
    let (input, total_change_count) = be_u32(input)?;
    let (input, free_page_list) = be_u32(input)?;
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
        FileHeader {
            magic: {
                let mut arr = [0u8; 16];
                arr.copy_from_slice(magic);
                arr
            },
            file_format_write_version,
            file_format_read_version,
            reserved_space,
            max_embedded_payload_fraction,
            min_embedded_payload_fraction,
            leaf_payload_fraction,
            file_change_counter,
            in_header_db_size,
            first_freelist_trunk_page,
            total_change_count,
            free_page_list,
            schema_cookie,
            schema_format_number,
            default_page_cache_size,
            largest_root_btree_page,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            reserved_for_expansion: {
                let mut arr = [0u8; 20];
                arr.copy_from_slice(reserved_for_expansion);
                arr
            },
        },
    ))
}

#[derive(Debug, PartialEq)]
struct BTreePage {
    page_type: PageType,
    first_freeblock: u16,
    num_cells: u16,
    offset_to_first_cell: u16,
    num_frag: u8,
    right_child: u32,
    cell_content_offset: u16,
    cell_size: u16,
    payload: Vec<u8>,
}

fn parse_btree_page(input: &[u8]) -> IResult<&[u8], BTreePage> {
    let (input, page_type) = map_res(be_u8, PageType::from_u8)(input)?;
    let (input, first_freeblock) = be_u16(input)?;
    let (input, num_cells) = be_u16(input)?;
    let (input, offset_to_first_cell) = be_u16(input)?;
    let (input, num_frag) = be_u8(input)?;
    let (input, right_child) = be_u32(input)?;
    let (input, cell_content_offset) = be_u16(input)?;
    let (input, cell_size) = be_u16(input)?;
    let (input, payload) = take(cell_size as usize)(input)?;

    Ok((
        input,
        BTreePage {
            page_type,
            first_freeblock,
            num_cells,
            offset_to_first_cell,
            num_frag,
            right_child,
            cell_content_offset,
            cell_size,
            payload: payload.to_vec(),
        },
    ))
}

#[derive(Debug, PartialEq)]
struct Cell {
    payload_length: u16,
    header_length: u8,
    num_columns: u8,
    column_data: Vec<u8>,
    payload: Vec<u8>,
}

fn parse_cell(input: &[u8]) -> IResult<&[u8], Cell> {
    let (input, payload_length) = be_u16(input)?;
    let (input, header_length) = be_u8(input)?;
    let (input, num_columns) = be_u8(input)?;
    let (input, column_data) = take(header_length as usize)(input)?;
    let (input, payload) = take(payload_length as usize)(input)?;

    Ok((
        input,
        Cell {
            payload_length,
            header_length,
            num_columns,
            column_data: column_data.to_vec(),
            payload: payload.to_vec(),
        },
    ))
}

#[derive(Debug, PartialEq)]
struct TableBTreeLeafCell {
    rowid: u32,
    payload: Vec<u8>,
}

fn parse_table_btree_leaf_cell(input: &[u8]) -> IResult<&[u8], TableBTreeLeafCell> {
    let (input, rowid) = be_u32(input)?;
    let (input, payload) = take_while_m_n(1, 65535, |x| x != 0)(input)?;

    Ok((
        input,
        TableBTreeLeafCell {
            rowid,
            payload: payload.to_vec(),
        },
    ))
}

#[derive(Debug, PartialEq)]
struct IndexBTreeLeafCell {
    key: Vec<u8>,
    rowid: u32,
}

fn parse_index_btree_leaf_cell(input: &[u8]) -> IResult<&[u8], IndexBTreeLeafCell> {
    let (input, key) = take_while_m_n(1, 65535, |x| x != 0)(input)?;
    let (input, rowid) = be_u32(input)?;

    Ok((
        input,
        IndexBTreeLeafCell {
            key: key.to_vec(),
            rowid,
        },
    ))
}

#[derive(Debug, PartialEq)]
struct TableBTreeInteriorCell {
    left_child: u32,
    key: Vec<u8>,
}

fn parse_table_btree_interior_cell(input: &[u8]) -> IResult<&[u8], TableBTreeInteriorCell> {
    let (input, left_child) = be_u32(input)?;
    let (input, key) = take_while_m_n(1, 65535, |x| x != 0)(input)?;

    Ok((
        input,
        TableBTreeInteriorCell {
            left_child,
            key: key.to_vec(),
        },
    ))
}

#[derive(Debug, PartialEq)]
struct IndexBTreeInteriorCell {
    left_child: u32,
    key: Vec<u8>,
}

fn parse_index_btree_interior_cell(input: &[u8]) -> IResult<&[u8], IndexBTreeInteriorCell> {
    let (input, left_child) = be_u32(input)?;
    let (input, key) = take_while_m_n(1, 65535, |x| x != 0)(input)?;

    Ok((
        input,
        IndexBTreeInteriorCell {
            left_child,
            key: key.to_vec(),
        },
    ))
}

#[derive(Debug, PartialEq)]
struct PointerMap {
    page_number: u32,
    ptr_type: u8,
}

fn parse_pointer_map(input: &[u8]) -> IResult<&[u8], PointerMap> {
    let (input, page_number) = be_u32(input)?;
    let (input, ptr_type) = be_u8(input)?;

    Ok((
        input,
        PointerMap {
            page_number,
            ptr_type,
        },
    ))
}

#[derive(Debug, PartialEq)]
struct OverflowPage {
    next_overflow_page: u32,
    data: Vec<u8>,
}

fn parse_overflow_page(input: &[u8]) -> IResult<&[u8], OverflowPage> {
    let (input, next_overflow_page) = be_u32(input)?;
    let (input, data) = take_while_m_n(1, 65535, |x| x != 0)(input)?;

    Ok((
        input,
        OverflowPage {
            next_overflow_page,
            data: data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", path.display(), why),
        Ok(file) => file,
    };

    let mut input = Vec::new();
    match file.read_to_end(&mut input) {
        Err(why) => panic!("couldn't read {}: {}", path.display(), why),
        Ok(_) => (),
    }

    let (input, file_header) = parse_file_header(&input).unwrap();
    println!("File Header: {:?}", file_header);

    let (input, btree_page) = parse_btree_page(input).unwrap();
    println!("BTree Page: {:?}", btree_page);

    let (input, cell) = parse_cell(input).unwrap();
    println!("Cell: {:?}", cell);

    let (input, table_btree_leaf_cell) = parse_table_btree_leaf_cell(input).unwrap();
    println!("Table BTree Leaf Cell: {:?}", table_btree_leaf_cell);

    let (input, index_btree_leaf_cell) = parse_index_btree_leaf_cell(input).unwrap();
    println!("Index BTree Leaf Cell: {:?}", index_btree_leaf_cell);

    let (input, table_btree_interior_cell) = parse_table_btree_interior_cell(input).unwrap();
    println!("Table BTree Interior Cell: {:?}", table_btree_interior_cell);

    let (input, index_btree_interior_cell) = parse_index_btree_interior_cell(input).unwrap();
    println!("Index BTree Interior Cell: {:?}", index_btree_interior_cell);

    let (input, pointer_map) = parse_pointer_map(input).unwrap();
    println!("Pointer Map: {:?}", pointer_map);

    let (_input, overflow_page) = parse_overflow_page(input).unwrap();
    println!("Overflow Page: {:?}", overflow_page);
}