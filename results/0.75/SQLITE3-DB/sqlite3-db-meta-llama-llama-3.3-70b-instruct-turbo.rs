use std::env;
use std::fs::File;
use std::io::Read;
use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::map;
use nom::multi::many0;
use nom::number::complete::{be_i16, be_i32, be_i64};
use nom::sequence::tuple;
use nom::IResult;

#[derive(Debug)]
enum SQLite3PageType {
    Leaf,
    Interior,
    Overflow,
    IndexLeaf,
    IndexInterior,
}

#[derive(Debug)]
struct SQLite3PageHeader {
    page_type: u8,
    first_freeblock: u16,
    cell_count: u16,
    cell_start: u16,
    fragmented_free_bytes: u8,
}

#[derive(Debug)]
struct SQLite3Page {
    page_header: SQLite3PageHeader,
    cells: Vec<SQLite3Cell>,
}

#[derive(Debug)]
struct SQLite3Cell {
    length: u16,
    payload: Vec<u8>,
}

#[derive(Debug)]
struct SQLite3Database {
    pages: Vec<SQLite3Page>,
}

fn parse_page_type(input: &[u8]) -> IResult<&[u8], SQLite3PageType> {
    alt((map(tag([0x0d]), |_| SQLite3PageType::Leaf),
         map(tag([0x05]), |_| SQLite3PageType::Interior),
         map(tag([0x02]), |_| SQLite3PageType::Overflow),
         map(tag([0x0a]), |_| SQLite3PageType::IndexLeaf),
         map(tag([0x01]), |_| SQLite3PageType::IndexInterior)))(input)
}

fn parse_page_header(input: &[u8]) -> IResult<&[u8], SQLite3PageHeader> {
    map(tuple((be_i16, be_i16, be_i16, be_i16, take(1usize))), |(page_type, first_freeblock, cell_count, cell_start, fragmented_free_bytes)| {
        SQLite3PageHeader {
            page_type,
            first_freeblock,
            cell_count,
            cell_start,
            fragmented_free_bytes: fragmented_free_bytes[0],
        }
    })(input)
}

fn parse_cell(input: &[u8]) -> IResult<&[u8], SQLite3Cell> {
    map(tuple((be_i16, many0(take(1usize)))), |(length, payload)| {
        SQLite3Cell {
            length,
            payload,
        }
    })(input)
}

fn parse_page(input: &[u8]) -> IResult<&[u8], SQLite3Page> {
    map(tuple((parse_page_header, many0(parse_cell))), |(page_header, cells)| {
        SQLite3Page {
            page_header,
            cells,
        }
    })(input)
}

fn parse_database(input: &[u8]) -> IResult<&[u8], SQLite3Database> {
    map(many0(parse_page), |pages| {
        SQLite3Database {
            pages,
        }
    })(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <file>", args[0]);
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Failed to read file");
    let result = parse_database(&data);
    match result {
        Ok((remaining, database)) => {
            println!("{:?}", database);
            println!("Remaining bytes: {}", remaining.len());
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}