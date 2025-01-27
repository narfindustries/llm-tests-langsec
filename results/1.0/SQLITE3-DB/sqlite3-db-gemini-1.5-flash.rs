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
    first_freelist_page: u32,
    // Add other header fields as needed based on the SQLite3 file format specification.

}

fn parse_sqlite_header(input: &[u8]) -> IResult<&[u8], SqliteHeader> {
    let (rest, magic) = le_u32(input)?;
    let (rest, page_size) = le_u32(rest)?;
    let (rest, write_version) = le_u32(rest)?;
    let (rest, read_version) = le_u32(rest)?;
    let (rest, reserved) = le_u32(rest)?;
    let (rest, max_page_count) = le_u32(rest)?;
    let (rest, change_count) = le_u32(rest)?;
    let (rest, first_freelist_page) = le_u32(rest)?;


    Ok((
        rest,
        SqliteHeader {
            magic,
            page_size,
            write_version,
            read_version,
            reserved,
            max_page_count,
            change_count,
            first_freelist_page,
        },
    ))
}



fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <sqlite_db_file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");


    match parse_sqlite_header(&buffer) {
        Ok((rest, header)) => {
            println!("Parsed header: {:?}", header);
            //Further processing of the database file can be done here.  This is a stub.
            //Example: connecting to the database and running queries.
            let conn = Connection::open(filename)?;
            let mut stmt = conn.prepare("SELECT SQLITE_VERSION()")?;
            let version = stmt.query_row([], |row| row.get(0))?;
            println!("SQLite version: {}", version);

        }
        Err(e) => {
            eprintln!("Failed to parse header: {:?}", e);
        }
    }

    Ok(())
}
