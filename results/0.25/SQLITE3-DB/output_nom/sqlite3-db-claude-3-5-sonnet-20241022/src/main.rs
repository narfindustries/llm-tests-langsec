use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::many0,
    number::complete::{be_u16, be_u32, be_u64, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct SQLiteHeader {
    magic: [u8; 16],
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_fraction: u8,
    min_fraction: u8,
    leaf_payload: u8,
    file_change_counter: u32,
    db_size: u32,
    first_freelist: u32,
    freelist_pages: u32,
    schema_cookie: u32,
    schema_format: u32,
    page_cache_size: u32,
    largest_root: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum: u32,
    application_id: u32,
    reserved: [u8; 20],
    version_valid: u32,
    version: u32,
}

#[derive(Debug)]
struct BTreePage {
    page_type: u8,
    first_freeblock: u16,
    cell_count: u16,
    cell_content_start: u16,
    fragmented_free_bytes: u8,
    cells: Vec<u16>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    let (input, (
        magic,
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_fraction,
        min_fraction,
        leaf_payload,
        file_change_counter,
        db_size,
        first_freelist,
        freelist_pages,
        schema_cookie,
        schema_format,
        page_cache_size,
        largest_root,
        text_encoding,
        user_version,
        incremental_vacuum,
        application_id,
        reserved,
        version_valid,
        version
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

    let mut magic_arr = [0u8; 16];
    magic_arr.copy_from_slice(magic);
    
    let mut reserved_arr = [0u8; 20];
    reserved_arr.copy_from_slice(reserved);

    Ok((input, SQLiteHeader {
        magic: magic_arr,
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_fraction,
        min_fraction,
        leaf_payload,
        file_change_counter,
        db_size,
        first_freelist,
        freelist_pages,
        schema_cookie,
        schema_format,
        page_cache_size,
        largest_root,
        text_encoding,
        user_version,
        incremental_vacuum,
        application_id,
        reserved: reserved_arr,
        version_valid,
        version,
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

    let (input, cells) = many0(be_u16)(input)?;

    Ok((input, BTreePage {
        page_type,
        first_freeblock,
        cell_count,
        cell_content_start,
        fragmented_free_bytes,
        cells,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_db_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_header(&buffer) {
        Ok((remaining, header)) => {
            println!("SQLite Header: {:#?}", header);
            
            // Parse first page after header
            if !remaining.is_empty() {
                match parse_btree_page(remaining) {
                    Ok((_, page)) => println!("First B-tree page: {:#?}", page),
                    Err(e) => eprintln!("Failed to parse B-tree page: {:?}", e),
                }
            }
        }
        Err(e) => eprintln!("Failed to parse header: {:?}", e),
    }
}