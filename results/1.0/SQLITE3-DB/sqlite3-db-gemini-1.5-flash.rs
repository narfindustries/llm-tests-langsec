use nom::{
    bytes::complete::{take, tag},
    combinator::{map, map_res},
    number::complete::{le_u16, le_u32, be_u32},
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
    reserved_byte: u8,
    max_page_count: u32,
    text_encoding: u32,
    user_version: u32,
    application_id: u32,
    version_valid_for_all_changes: u32,
    page_count: u32,
}


fn sqlite_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
    let (input, magic) = take(16usize)(input)?;
    let (input, page_size) = le_u16(input)?;
    let (input, write_version) = le_u16(input)?;
    let (input, read_version) = le_u16(input)?;
    let (input, reserved_byte) = take(1usize)(input)?;
    let (input, max_page_count) = le_u32(input)?;
    let (input, text_encoding) = le_u32(input)?;
    let (input, user_version) = le_u32(input)?;
    let (input, application_id) = le_u32(input)?;
    let (input, version_valid_for_all_changes) = le_u32(input)?;
    let (input, page_count) = le_u32(input)?;

    Ok((
        input,
        SqliteHeader {
            magic: magic.try_into().unwrap(),
            page_size,
            write_version,
            read_version,
            reserved_byte: reserved_byte[0],
            max_page_count,
            text_encoding,
            user_version,
            application_id,
            version_valid_for_all_changes,
            page_count,
        },
    ))
}


fn parse_page(input: &[u8], page_size: u16) -> IResult<&[u8], ()> {
    //This is a placeholder.  Actual page parsing is significantly more complex.
    let (input, _) = take(page_size as usize)(input)?;
    Ok((input, ()))

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
        Ok((remaining, header)) => {
            println!("SQLite Header: {:?}", header);
            //Process pages.  This requires significantly more code.
            let mut offset = 100;
            while offset < buffer.len() {
                match parse_page(&buffer[offset..], header.page_size){
                    Ok((rem, _)) => {
                        offset += header.page_size as usize;
                    },
                    Err(e) => {
                        println!("Error parsing page at offset {}: {:?}", offset, e);
                        break;
                    }
                }
            }

        }
        Err(e) => {
            println!("Error parsing header: {:?}", e);
        }
    }
}
