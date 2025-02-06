use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{many0, many1},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, stdin};
use std::path::Path;

// File Header
#[derive(Debug)]
struct FileHeader {
    magic: [u8; 4],
    file_format_write_version: u8,
    file_format_read_version: u8,
    reserved: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    file_change_counter: u32,
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, magic) = tag([0x53, 0x51, 0x4c, 0x69].as_ref())(input)?;
    let (input, file_format_write_version) = be_u8(input)?;
    let (input, file_format_read_version) = be_u8(input)?;
    let (input, reserved) = be_u8(input)?;
    let (input, max_embedded_payload_fraction) = be_u8(input)?;
    let (input, min_embedded_payload_fraction) = be_u8(input)?;
    let (input, file_change_counter) = be_u32(input)?;
    Ok((
        input,
        FileHeader {
            magic: [0x53, 0x51, 0x4c, 0x69],
            file_format_write_version,
            file_format_read_version,
            reserved,
            max_embedded_payload_fraction,
            min_embedded_payload_fraction,
            file_change_counter,
        },
    ))
}

// Page Header
#[derive(Debug)]
enum PageType {
    Purgable,
    NonPurgable,
    Overflow,
    PointerMap,
    Locked,
    Free,
}

#[derive(Debug)]
struct PageHeader {
    page_type: PageType,
    first_freeblock: u16,
    cell_offset: u16,
    fragment_offset: u16,
    num_cells: u16,
}

fn parse_page_header(input: &[u8]) -> IResult<&[u8], PageHeader> {
    let (input, page_type) = be_u8(input)?;
    let page_type = match page_type {
        0x00 => PageType::Purgable,
        0x01 => PageType::NonPurgable,
        0x02 => PageType::Overflow,
        0x03 => PageType::PointerMap,
        0x04 => PageType::Locked,
        0x05 => PageType::Free,
        _ => unreachable!(),
    };
    let (input, first_freeblock) = be_u16(input)?;
    let (input, cell_offset) = be_u16(input)?;
    let (input, fragment_offset) = be_u16(input)?;
    let (input, num_cells) = be_u16(input)?;
    Ok((
        input,
        PageHeader {
            page_type,
            first_freeblock,
            cell_offset,
            fragment_offset,
            num_cells,
        },
    ))
}

// B-tree Page Cell
#[derive(Debug)]
struct BTreePageCell {
    payload_length: u16,
    rowid: u32,
    payload: Vec<u8>,
}

fn parse_btree_page_cell(input: &[u8]) -> IResult<&[u8], BTreePageCell> {
    let (input, payload_length) = be_u16(input)?;
    let (input, rowid) = be_u32(input)?;
    let (input, payload) = take(payload_length)(input)?;
    Ok((
        input,
        BTreePageCell {
            payload_length,
            rowid,
            payload: payload.to_vec(),
        },
    ))
}

// B-tree Cell Key
#[derive(Debug)]
enum SerialType {
    Null,
    Int1,
    Int2,
    Int3,
    Int4,
    Int6,
    Int8,
    Float,
    String(u8),
    Blob(u8),
}

#[derive(Debug)]
struct BTreeCellKey {
    length: u8,
    serial_type: SerialType,
    value: Vec<u8>,
}

fn parse_btree_cell_key(input: &[u8]) -> IResult<&[u8], BTreeCellKey> {
    let (input, length) = be_u8(input)?;
    let (input, serial_type) = be_u8(input)?;
    let serial_type = match serial_type {
        0x00 => SerialType::Null,
        0x01 => SerialType::Int1,
        0x02 => SerialType::Int2,
        0x03 => SerialType::Int3,
        0x04 => SerialType::Int4,
        0x05 => SerialType::Int6,
        0x06 => SerialType::Int8,
        0x07 => SerialType::Float,
        0x08 => SerialType::String(0),
        0x09 => SerialType::String(1),
        0x0a => SerialType::String(2),
        0x0b => SerialType::String(3),
        0x0c => SerialType::String(4),
        0x0d => SerialType::String(6),
        0x0e => SerialType::String(8),
        0x0f => SerialType::String(10),
        0x10..=0xff => SerialType::Blob(serial_type - 0x10),
        _ => unreachable!(),
    };
    let (input, value) = take(length)(input)?;
    Ok((
        input,
        BTreeCellKey {
            length,
            serial_type,
            value: value.to_vec(),
        },
    ))
}

// Pointer Map Page
#[derive(Debug)]
struct PointerMapPage {
    pm_flags: u8,
    ptr: u32,
}

fn parse_pointer_map_page(input: &[u8]) -> IResult<&[u8], PointerMapPage> {
    let (input, pm_flags) = be_u8(input)?;
    let (input, ptr) = be_u32(input)?;
    Ok((
        input,
        PointerMapPage {
            pm_flags,
            ptr,
        },
    ))
}

// SQLite Database File
#[derive(Debug)]
struct SQLiteDBFile {
    file_header: FileHeader,
    pages: Vec<PageHeader>,
}

fn parse_sqlite_db_file(input: &[u8]) -> IResult<&[u8], SQLiteDBFile> {
    let (input, file_header) = parse_file_header(input)?;
    let (input, pages) = many1(parse_page_header)(input)?;
    Ok((
        input,
        SQLiteDBFile {
            file_header,
            pages,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let mut file = match File::open(input_file) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };
    let mut input = Vec::new();
    match file.read_to_end(&mut input) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    }
    match parse_sqlite_db_file(&input) {
        Ok((remaining, sqlite_db_file)) => {
            println!("{:?}", sqlite_db_file);
            if !remaining.is_empty() {
                eprintln!("Warning: {} bytes remaining after parsing", remaining.len());
            }
        }
        Err(err) => {
            eprintln!("Error parsing file: {:?}", err);
        }
    }
}