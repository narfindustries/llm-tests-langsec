use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::count,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug)]
struct Sqlite3Header {
    signature: String,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved_space: u8,
    max_payload_frac: u8,
    min_payload_frac: u8,
    leaf_payload_frac: u8,
    change_counter: u32,
    in_header_db_size: u32,
    first_freelist_trunk_page: u32,
    freelist_page_count: u32,
    schema_cookie: u32,
    schema_format: u32,
    default_cache_size: u32,
    largest_root_btree_page: u32,
    text_encoding: u32,
    user_version: u32,
    increment_vacuum_mode: u32,
    application_id: u32,
    reserved: Vec<u8>,
    version_valid_for: u32,
    sqlite_version: u32,
}

fn parse_sqlite3_header(input: &[u8]) -> IResult<&[u8], Sqlite3Header> {
    map(
        tuple((
            tag(b"SQLite format 3\x00"),
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
            count(be_u8, 20),
            be_u32,
            be_u32,
        )),
        |(
            _,
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_payload_frac,
            min_payload_frac,
            leaf_payload_frac,
            change_counter,
            in_header_db_size,
            first_freelist_trunk_page,
            freelist_page_count,
            schema_cookie,
            schema_format,
            default_cache_size,
            largest_root_btree_page,
            text_encoding,
            user_version,
            increment_vacuum_mode,
            application_id,
            reserved,
            version_valid_for,
            sqlite_version,
        )| Sqlite3Header {
            signature: "SQLite format 3".into(),
            page_size,
            write_version,
            read_version,
            reserved_space,
            max_payload_frac,
            min_payload_frac,
            leaf_payload_frac,
            change_counter,
            in_header_db_size,
            first_freelist_trunk_page,
            freelist_page_count,
            schema_cookie,
            schema_format,
            default_cache_size,
            largest_root_btree_page,
            text_encoding,
            user_version,
            increment_vacuum_mode,
            application_id,
            reserved,
            version_valid_for,
            sqlite_version,
        },
    )(input)
}

fn read_sqlite_file<P: AsRef<Path>>(path: P) -> Result<Sqlite3Header, String> {
    let mut file = File::open(path).map_err(|e| e.to_string())?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).map_err(|e| e.to_string())?;
    parse_sqlite3_header(&buffer)
        .map(|(_, header)| header)
        .map_err(|_| "Failed to parse SQLite header".to_string())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: <program> <sqlite_file>");
        return;
    }

    match read_sqlite_file(&args[1]) {
        Ok(header) => println!("{:#?}", header),
        Err(err) => eprintln!("Error: {}", err),
    }
}