use nom::{
    bytes::complete::take,
    combinator::map,
    error::Error,
    number::complete::{be_u8, be_u16, be_u32, le_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct SQLiteHeader {
    magic: [u8; 16],
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_bytes: u8,
    max_embedded_payload: u8,
    min_embedded_payload: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    sqlite_version: u32,
    database_size: u32,
    first_freelist_page: u32,
    freelist_page_count: u32,
}

#[derive(Debug)]
struct BTreePage {
    page_type: u8,
    offset_rightmost_pointer: u16,
    cell_count: u16,
    cell_pointer_start: u16,
    fragmented_free_bytes: u8,
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    map(
        tuple((
            take::<usize, &[u8], Error<&[u8]>>(16),
            be_u16,
            be_u8,
            be_u8,
            be_u8,
            be_u8,
            be_u8,
            be_u8,
            be_u32,
            le_u32,
            be_u32,
            be_u32,
            be_u32
        )),
        |(magic, page_size, write_version, read_version, reserved_bytes, 
          max_embedded_payload, min_embedded_payload, leaf_payload_fraction, 
          file_change_counter, sqlite_version, database_size, 
          first_freelist_page, freelist_page_count)| {
            SQLiteHeader {
                magic: magic.try_into().unwrap(),
                page_size,
                write_version,
                read_version,
                reserved_bytes,
                max_embedded_payload,
                min_embedded_payload,
                leaf_payload_fraction,
                file_change_counter,
                sqlite_version,
                database_size,
                first_freelist_page,
                freelist_page_count,
            }
        }
    )(input)
}

fn parse_btree_page(input: &[u8]) -> IResult<&[u8], BTreePage> {
    map(
        tuple((
            be_u8,
            be_u16,
            be_u16,
            be_u16,
            be_u8
        )),
        |(page_type, offset_rightmost_pointer, cell_count, 
          cell_pointer_start, fragmented_free_bytes)| {
            BTreePage {
                page_type,
                offset_rightmost_pointer,
                cell_count,
                cell_pointer_start,
                fragmented_free_bytes,
            }
        }
    )(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <sqlite_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_sqlite_header(&buffer) {
        Ok((remaining, header)) => {
            println!("SQLite Header: {:?}", header);
            
            if let Ok((_, btree_page)) = parse_btree_page(remaining) {
                println!("BTree Page: {:?}", btree_page);
            }
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}