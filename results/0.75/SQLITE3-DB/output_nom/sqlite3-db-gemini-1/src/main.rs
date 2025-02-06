use nom::{
    bytes::complete::{take, tag},
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct PageHeader {
    page_number: u32,
    page_type: u8,
    freelist_trunk_page: u32,
    cells: u16,
    first_freeblock: u16,
    page_size: u16,
    unused: Vec<u8>,
}

fn parse_page_header(input: &[u8]) -> IResult<&[u8], PageHeader> {
    let (input, page_number) = le_u32(input)?;
    let (input, page_type) = take(1usize)(input)?;
    let (input, freelist_trunk_page) = le_u32(input)?;
    let (input, cells) = le_u16(input)?;
    let (input, first_freeblock) = le_u16(input)?;
    let (input, page_size) = le_u16(input)?;
    let (input, unused) = take((page_size as usize) - 100)(input)?;

    Ok((
        input,
        PageHeader {
            page_number,
            page_type: page_type[0],
            freelist_trunk_page,
            cells,
            first_freeblock,
            page_size,
            unused: unused.to_vec(),
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

    match parse_page_header(&buffer) {
        Ok((remaining, header)) => {
            println!("Parsed Page Header: {:?}", header);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse page header: {:?}", e);
        }
    }
}
