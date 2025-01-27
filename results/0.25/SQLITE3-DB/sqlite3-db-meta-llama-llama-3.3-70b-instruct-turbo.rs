use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map, opt},
    multi::{many0, many1},
    number::complete::{be_u16, be_u32, be_u64},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
struct SQLiteHeader {
    magic: [u8; 16],
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_format: u8,
    size_of_database_file: u32,
    size_of_pages_in_database: u32,
    size_of_pages_to_reserve: u32,
    size_of_pages_to_free: u32,
    number_of_pages_to_roll_back: u32,
    db_size_in_pages: u32,
    db_first_freeslot: u32,
    n_reserved: u32,
    version_valid_for: u32,
    sql_subset: u32,
    big_endian: u8,
    page_cache_size: u32,
    largest_root_btree_page: u32,
    text_encoding: u32,
    user_version: u32,
    incremental_vacuum_mode: u32,
    application_id: u32,
    version: u32,
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SQLiteHeader> {
    let (input, magic) = take(16usize)(input)?;
    let (input, page_size) = be_u16(input)?;
    let (input, write_version) = take(1usize)(input)?;
    let (input, read_version) = take(1usize)(input)?;
    let (input, reserved_space) = take(1usize)(input)?;
    let (input, max_embedded_payload_fraction) = take(1usize)(input)?;
    let (input, min_embedded_payload_fraction) = take(1usize)(input)?;
    let (input, leaf_payload_fraction) = take(1usize)(input)?;
    let (input, file_format) = take(1usize)(input)?;
    let (input, size_of_database_file) = be_u32(input)?;
    let (input, size_of_pages_in_database) = be_u32(input)?;
    let (input, size_of_pages_to_reserve) = be_u32(input)?;
    let (input, size_of_pages_to_free) = be_u32(input)?;
    let (input, number_of_pages_to_roll_back) = be_u32(input)?;
    let (input, db_size_in_pages) = be_u32(input)?;
    let (input, db_first_freeslot) = be_u32(input)?;
    let (input, n_reserved) = be_u32(input)?;
    let (input, version_valid_for) = be_u32(input)?;
    let (input, sql_subset) = be_u32(input)?;
    let (input, big_endian) = take(1usize)(input)?;
    let (input, page_cache_size) = be_u32(input)?;
    let (input, largest_root_btree_page) = be_u32(input)?;
    let (input, text_encoding) = be_u32(input)?;
    let (input, user_version) = be_u32(input)?;
    let (input, incremental_vacuum_mode) = be_u32(input)?;
    let (input, application_id) = be_u32(input)?;
    let (input, version) = be_u32(input)?;

    Ok((
        input,
        SQLiteHeader {
            magic: magic.try_into().unwrap(),
            page_size,
            write_version: write_version[0],
            read_version: read_version[0],
            reserved_space: reserved_space[0],
            max_embedded_payload_fraction: max_embedded_payload_fraction[0],
            min_embedded_payload_fraction: min_embedded_payload_fraction[0],
            leaf_payload_fraction: leaf_payload_fraction[0],
            file_format: file_format[0],
            size_of_database_file,
            size_of_pages_in_database,
            size_of_pages_to_reserve,
            size_of_pages_to_free,
            number_of_pages_to_roll_back,
            db_size_in_pages,
            db_first_freeslot,
            n_reserved,
            version_valid_for,
            sql_subset,
            big_endian: big_endian[0],
            page_cache_size,
            largest_root_btree_page,
            text_encoding,
            user_version,
            incremental_vacuum_mode,
            application_id,
            version,
        },
    ))
}

#[derive(Debug)]
struct SQLitePage {
    page_type: u8,
    first_freeblock: u16,
    number_of_cells: u16,
    cell_content_area_offset: u16,
    number_of_freed_bytes: u8,
    rightmost_pointer: u32,
}

fn parse_sqlite_page(input: &[u8]) -> IResult<&[u8], SQLitePage> {
    let (input, page_type) = take(1usize)(input)?;
    let (input, first_freeblock) = be_u16(input)?;
    let (input, number_of_cells) = be_u16(input)?;
    let (input, cell_content_area_offset) = be_u16(input)?;
    let (input, number_of_freed_bytes) = take(1usize)(input)?;
    let (input, rightmost_pointer) = be_u32(input)?;

    Ok((
        input,
        SQLitePage {
            page_type: page_type[0],
            first_freeblock,
            number_of_cells,
            cell_content_area_offset,
            number_of_freed_bytes: number_of_freed_bytes[0],
            rightmost_pointer,
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
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).unwrap();

    let (input, header) = parse_sqlite_header(&buffer).unwrap();
    println!("{:?}", header);

    let (input, page) = parse_sqlite_page(input).unwrap();
    println!("{:?}", page);
}