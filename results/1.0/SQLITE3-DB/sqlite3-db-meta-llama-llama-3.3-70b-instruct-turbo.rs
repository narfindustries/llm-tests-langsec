use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    multi::{length_data, many0, many_till},
    number::complete::{be_u16, be_u32, be_u64},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, Seek, SeekFrom},
    path::Path,
};

#[derive(Debug, Default)]
struct SqliteHeader {
    magic: Vec<u8>,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
    file_change_counter: u32,
    database_size_in_pages: u32,
    first_freelist_page: u32,
    number_of_freelist_pages: u32,
    schema_cookie: u32,
    schema_format_number: u32,
    default_page_cache_size: u32,
    largest_page_number_used: u32,
    application_id: u32,
    reserved_for_expansion: u32,
    version_valid_for_number: u32,
    sqlite_version_number: u32,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
    map(
        tuple((
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
        )),
        |(
            magic,
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_embedded_payload_fraction,
            min_embedded_payload_fraction,
            leaf_payload_fraction,
            file_change_counter,
            database_size_in_pages,
            first_freelist_page,
            number_of_freelist_pages,
            schema_cookie,
            schema_format_number,
            default_page_cache_size,
            largest_page_number_used,
            application_id,
            reserved_for_expansion,
            version_valid_for_number,
            sqlite_version_number,
        )| {
            SqliteHeader {
                magic: magic.to_vec(),
                page_size,
                write_version,
                read_version,
                reserved_space,
                max_embedded_payload_fraction,
                min_embedded_payload_fraction,
                leaf_payload_fraction,
                file_change_counter,
                database_size_in_pages,
                first_freelist_page,
                number_of_freelist_pages,
                schema_cookie,
                schema_format_number,
                default_page_cache_size,
                largest_page_number_used,
                application_id,
                reserved_for_expansion,
                version_valid_for_number,
                sqlite_version_number,
            }
        },
    )(input)
}

#[derive(Debug, Default)]
struct BtreePageHeader {
    page_type: u8,
    first_free_offset: u16,
    cell_offsetarray_length: u16,
    cell_count: u16,
    page_number: u32,
    right_child_page_number: u32,
}

fn parse_btree_page_header(input: &[u8]) -> IResult<&[u8], BtreePageHeader> {
    map(
        tuple((
            be_u8,
            be_u16,
            be_u16,
            be_u16,
            be_u32,
            be_u32,
        )),
        |(
            page_type,
            first_free_offset,
            cell_offsetarray_length,
            cell_count,
            page_number,
            right_child_page_number,
        )| {
            BtreePageHeader {
                page_type,
                first_free_offset,
                cell_offsetarray_length,
                cell_count,
                page_number,
                right_child_page_number,
            }
        },
    )(input)
}

#[derive(Debug, Default)]
struct CellHeader {
    length: u16,
    key_length: u8,
    number_of_cells: u8,
    left_child_page_number: u32,
    right_child_page_number: u32,
}

fn parse_cell_header(input: &[u8]) -> IResult<&[u8], CellHeader> {
    map(
        tuple((
            be_u16,
            be_u8,
            be_u8,
            be_u32,
            be_u32,
        )),
        |(
            length,
            key_length,
            number_of_cells,
            left_child_page_number,
            right_child_page_number,
        )| {
            CellHeader {
                length,
                key_length,
                number_of_cells,
                left_child_page_number,
                right_child_page_number,
            }
        },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }

    let file_path = Path::new(&args[1]);
    let mut file = File::open(file_path).expect("Failed to open file");
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Failed to read file");

    let header = parse_header(&data).unwrap().1;
    println!("Sqlite Header: {:?}", header);

    file.seek(SeekFrom::Start(100)).expect("Failed to seek file");
    let mut buffer = [0u8; 1024];
    file.read(&mut buffer).expect("Failed to read file");

    let btree_page_header = parse_btree_page_header(&buffer).unwrap().1;
    println!("Btree Page Header: {:?}", btree_page_header);

    let cell_header = parse_cell_header(&buffer[12..]).unwrap().1;
    println!("Cell Header: {:?}", cell_header);
}