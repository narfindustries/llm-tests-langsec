use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    number::complete::{be_u16, be_u32, le_u32},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct SqliteHeader {
    magic: [u8; 16],
    page_size: u16,
    write_version: u16,
    read_version: u16,
    reserved_space: u32,
    max_page_count: u32,
    text_encoding: u32,
    user_version: u32,
    data_version: u32,
    page_count: u32,
    checksum_flag: u32,
    free_page_count: u32,
}

#[derive(Debug)]
struct SqlitePageHeader {
    page_number: u32,
    page_type: u8,
    free_block_count: u8,
    cell_count: u16,
    free_byte_count: u16,
    right_most_child_page: u32,
}

fn sqlite_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
    let (rest, magic) = take(16usize)(input)?;
    let (rest, page_size) = be_u16(rest)?;
    let (rest, write_version) = be_u16(rest)?;
    let (rest, read_version) = be_u16(rest)?;
    let (rest, reserved_space) = be_u32(rest)?;
    let (rest, max_page_count) = be_u32(rest)?;
    let (rest, text_encoding) = be_u32(rest)?;
    let (rest, user_version) = be_u32(rest)?;
    let (rest, data_version) = be_u32(rest)?;
    let (rest, page_count) = be_u32(rest)?;
    let (rest, checksum_flag) = be_u32(rest)?;
    let (rest, free_page_count) = be_u32(rest)?;

    Ok((
        rest,
        SqliteHeader {
            magic: magic.try_into().unwrap(),
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_page_count,
            text_encoding,
            user_version,
            data_version,
            page_count,
            checksum_flag,
            free_page_count,
        },
    ))
}


fn sqlite_page_header(input: &[u8]) -> IResult<&[u8], SqlitePageHeader> {
    let (rest, page_number) = be_u32(input)?;
    let (rest, page_type) = take(1usize)(rest)?;
    let (rest, free_block_count) = take(1usize)(rest)?;
    let (rest, cell_count) = be_u16(rest)?;
    let (rest, free_byte_count) = be_u16(rest)?;
    let (rest, right_most_child_page) = be_u32(rest)?;

    Ok((
        rest,
        SqlitePageHeader {
            page_number,
            page_type: page_type[0],
            free_block_count: free_block_count[0],
            cell_count,
            free_byte_count,
            right_most_child_page,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_db_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match sqlite_header(&buffer) {
        Ok((rest, header)) => {
            println!("Header: {:?}", header);
            match sqlite_page_header(rest) {
                Ok((_, page_header)) => {
                    println!("Page Header: {:?}", page_header);
                }
                Err(e) => {
                    println!("Error parsing page header: {:?}", e);
                }
            }

        }
        Err(e) => {
            println!("Error parsing header: {:?}", e);
        }
    }
}
