use nom::branch::alt;
use nom::bytes::complete::{take, take_while_m_n};
use nom::combinator::{map, map_res, opt};
use nom::error::{ErrorKind, ParseError};
use nom::multi::{many0, many1};
use nom::number::complete::{be_u16, be_u32, be_u64};
use nom::sequence::{delimited, preceded, tuple};
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::{Read, stdin};
use std::path::Path;

#[derive(Debug)]
enum SQLitePageType {
    Leaf,
    Internal,
    TableLeaf,
    TableInternal,
    IndexLeaf,
    IndexInternal,
}

#[derive(Debug)]
struct SQLitePageHeader {
    page_type: SQLitePageType,
    first_freeblock: u32,
    cell_count: u16,
    cell_start: u32,
    fragmented_free_bytes: u32,
    right_child: u32,
}

#[derive(Debug)]
struct SQLiteCell {
    payload_length: u64,
    payload: Vec<u8>,
}

#[derive(Debug)]
struct SQLiteBtreePage {
    page_header: SQLitePageHeader,
    cells: Vec<SQLiteCell>,
}

fn parse_sqlite_page_type(input: &[u8]) -> IResult<&[u8], SQLitePageType> {
    map_res(take(1usize), |x: &[u8]| {
        match x[0] {
            0x05 => Ok(SQLitePageType::Leaf),
            0x02 => Ok(SQLitePageType::Internal),
            0x0d => Ok(SQLitePageType::TableLeaf),
            0x05 => Ok(SQLitePageType::TableInternal),
            0x0a => Ok(SQLitePageType::IndexLeaf),
            0x02 => Ok(SQLitePageType::IndexInternal),
            _ => Err(ParseError::from_error_kind(input, ErrorKind::OneOf)),
        }
    })(input)
}

fn parse_sqlite_page_header(input: &[u8]) -> IResult<&[u8], SQLitePageHeader> {
    map(
        tuple((
            parse_sqlite_page_type,
            be_u32,
            be_u16,
            be_u32,
            be_u32,
            opt(be_u32),
        )),
        |(page_type, first_freeblock, cell_count, cell_start, fragmented_free_bytes, right_child)| {
            SQLitePageHeader {
                page_type,
                first_freeblock,
                cell_count,
                cell_start,
                fragmented_free_bytes,
                right_child: right_child.unwrap_or(0),
            }
        },
    )(input)
}

fn parse_sqlite_cell_payload(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map_res(be_u64, |payload_length| take(payload_length as usize))(input)
}

fn parse_sqlite_cell(input: &[u8]) -> IResult<&[u8], SQLiteCell> {
    map(
        tuple((be_u64, parse_sqlite_cell_payload)),
        |(payload_length, payload)| SQLiteCell {
            payload_length,
            payload,
        },
    )(input)
}

fn parse_sqlite_btree_page(input: &[u8]) -> IResult<&[u8], SQLiteBtreePage> {
    map(
        tuple((parse_sqlite_page_header, many0(parse_sqlite_cell))),
        |(page_header, cells)| SQLiteBtreePage { page_header, cells },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <file>", args[0]);
    }
    let path = Path::new(&args[1]);
    let mut file = File::open(path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");
    match parse_sqlite_btree_page(&buffer) {
        Ok((remaining, btree_page)) => {
            println!("{:?}", btree_page);
        }
        Err(err) => {
            eprintln!("Error: {:?}", err);
        }
    }
}