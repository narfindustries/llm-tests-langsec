use nom::{
    bytes::complete::take,
    number::complete::{le_u16, le_u32, le_u64},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct SqliteHeader {
    magic: [u8; 16],
    page_size: u16,
    write_count: u32,
    text_encoding: u32,
    default_page_cache_size: u32,
    large_file_support: u32,
    version_number: u32,
    application_id: u64,
    unused: [u8; 60],
}

#[derive(Debug)]
struct SqlitePageHeader {
    page_number: u32,
    page_type: u8,
    free_bytes: u8,
    checksum: u16,
    // cell pointer array and cell data are complex and omitted for brevity
}


fn sqlite_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
    let (rest, magic) = take(16usize)(input)?;
    let (rest, page_size) = le_u16(rest)?;
    let (rest, write_count) = le_u32(rest)?;
    let (rest, text_encoding) = le_u32(rest)?;
    let (rest, default_page_cache_size) = le_u32(rest)?;
    let (rest, large_file_support) = le_u32(rest)?;
    let (rest, version_number) = le_u32(rest)?;
    let (rest, application_id) = le_u64(rest)?;
    let (rest, unused) = take(60usize)(rest)?;

    Ok((
        rest,
        SqliteHeader {
            magic: magic.try_into().unwrap(),
            page_size,
            write_count,
            text_encoding,
            default_page_cache_size,
            large_file_support,
            version_number,
            application_id,
            unused: unused.try_into().unwrap(),
        },
    ))
}

fn sqlite_page_header(input: &[u8]) -> IResult<&[u8], SqlitePageHeader> {
    let (rest, page_number) = le_u32(input)?;
    let (rest, page_type) = take(1usize)(rest)?;
    let (rest, free_bytes) = take(1usize)(rest)?;
    let (rest, checksum) = le_u16(rest)?;

    Ok((
        rest,
        SqlitePageHeader {
            page_number,
            page_type: page_type[0],
            free_bytes: free_bytes[0],
            checksum,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_db_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    };

    match sqlite_header(&buffer) {
        Ok((rest, header)) => {
            println!("Header: {:?}", header);
            let page_size = header.page_size as usize;
            if rest.len() >= page_size {
                match sqlite_page_header(&rest[..page_size]) {
                    Ok((_, page_header)) => println!("First Page Header: {:?}", page_header),
                    Err(e) => eprintln!("Error parsing page header: {:?}", e),
                }
            }
        }
        Err(e) => eprintln!("Error parsing header: {:?}", e),
    }
}
