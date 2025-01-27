use nom::{
    bytes::complete::{tag, take_while},
    combinator::{map, opt},
    number::complete::le_u32,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use rusqlite::{Connection, Result};


#[derive(Debug)]
struct SqliteHeader {
    magic: u32,
    page_size: u32,
    write_version: u32,
    read_version: u32,
    reserved: u32,
    max_page_count: u32,
    change_count: u32,
    sector_size: u32,
    freelist_trunk_page: u32,
    // ... other fields as needed ...
}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
    let (input, magic) = le_u32(input)?;
    let (input, page_size) = le_u32(input)?;
    let (input, write_version) = le_u32(input)?;
    let (input, read_version) = le_u32(input)?;
    let (input, reserved) = le_u32(input)?;
    let (input, max_page_count) = le_u32(input)?;
    let (input, change_count) = le_u32(input)?;
    let (input, sector_size) = le_u32(input)?;
    let (input, freelist_trunk_page) = le_u32(input)?;

    Ok((
        input,
        SqliteHeader {
            magic,
            page_size,
            write_version,
            read_version,
            reserved,
            max_page_count,
            change_count,
            sector_size,
            freelist_trunk_page,
        },
    ))
}


fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_db_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_sqlite_header(&buffer) {
        Ok((remaining, header)) => {
            println!("Parsed header: {:?}", header);
            println!("Remaining bytes: {:?}", remaining);

            //Further processing of the database file using rusqlite
            let conn = Connection::open(filename)?;
            //Example query
            let mut stmt = conn.prepare("SELECT SQLITE_VERSION()")?;
            let version = stmt.query_row([], |row| row.get(0))?;
            println!("SQLite version: {}", version);

        }
        Err(e) => {
            eprintln!("Failed to parse header: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}
