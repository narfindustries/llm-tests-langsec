use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    error::ErrorKind,
    number::complete::{be_u16, be_u32, be_u64, be_u8},
    multi::many1,
    IResult,
};

#[derive(Debug, PartialEq)]
enum CellType {
    Null,
    Int8,
    Int16,
    Int24,
    Int32,
    Int48,
    Int64,
    Uint8,
    Uint16,
    Uint24,
    Uint32,
    Uint48,
    Uint64,
    Text,
    Blob,
    ZeroTerminatedString,
}

impl CellType {
    fn parse(i: &[u8]) -> IResult<&[u8], CellType> {
        map(be_u8, |x| match x {
            0 => CellType::Null,
            1 => CellType::Int8,
            2 => CellType::Int16,
            3 => CellType::Int24,
            4 => CellType::Int32,
            5 => CellType::Int48,
            6 => CellType::Int64,
            7 => CellType::Uint8,
            8 => CellType::Uint16,
            9 => CellType::Uint24,
            10 => CellType::Uint32,
            11 => CellType::Uint48,
            12 => CellType::Uint64,
            13 => CellType::Text,
            14 => CellType::Blob,
            15 => CellType::ZeroTerminatedString,
            _ => unreachable!(),
        })(i)
    }
}

#[derive(Debug, PartialEq)]
struct Cell {
    payload_length: u16,
    header_length: u8,
    num_header_bytes: u8,
    child_page_number: u32,
    rowid: u64,
    payload: Vec<u8>,
}

impl Cell {
    fn parse(i: &[u8]) -> IResult<&[u8], Cell> {
        let (i, payload_length) = be_u16(i)?;
        let (i, header_length) = be_u8(i)?;
        let (i, num_header_bytes) = be_u8(i)?;
        let (i, child_page_number) = be_u32(i)?;
        let (i, rowid) = be_u64(i)?;
        let (i, payload) = take(payload_length as usize)(i)?;
        Ok((i, Cell {
            payload_length,
            header_length,
            num_header_bytes,
            child_page_number,
            rowid,
            payload: payload.to_vec(),
        }))
    }
}

#[derive(Debug, PartialEq)]
struct PageHeader {
    page_type: u8,
    first_freeblock: u16,
    cell_content_area: u16,
    fragmented_free_bytes: u8,
    num_cells: u8,
}

impl PageHeader {
    fn parse(i: &[u8]) -> IResult<&[u8], PageHeader> {
        let (i, page_type) = be_u8(i)?;
        let (i, first_freeblock) = be_u16(i)?;
        let (i, cell_content_area) = be_u16(i)?;
        let (i, fragmented_free_bytes) = be_u8(i)?;
        let (i, num_cells) = be_u8(i)?;
        Ok((i, PageHeader {
            page_type,
            first_freeblock,
            cell_content_area,
            fragmented_free_bytes,
            num_cells,
        }))
    }
}

#[derive(Debug, PartialEq)]
struct BTreePage {
    page_number: u32,
    right_child_page_number: u32,
    cells: Vec<Cell>,
}

impl BTreePage {
    fn parse(i: &[u8]) -> IResult<&[u8], BTreePage> {
        let (i, page_number) = be_u32(i)?;
        let (i, right_child_page_number) = be_u32(i)?;
        let (i, cells) = many1(Cell::parse)(i)?;
        Ok((i, BTreePage {
            page_number,
            right_child_page_number,
            cells,
        }))
    }
}

#[derive(Debug, PartialEq)]
struct MasterJournalHeader {
    magic: Vec<u8>,
    version: u32,
    pagesize: u16,
    sector_size: u16,
    file_format: u8,
    pages_in_journal: u32,
    master_journal_page_number: u32,
}

impl MasterJournalHeader {
    fn parse(i: &[u8]) -> IResult<&[u8], MasterJournalHeader> {
        let (i, magic) = take(4usize)(i)?;
        let (i, version) = be_u32(i)?;
        let (i, pagesize) = be_u16(i)?;
        let (i, sector_size) = be_u16(i)?;
        let (i, file_format) = be_u8(i)?;
        let (i, pages_in_journal) = be_u32(i)?;
        let (i, master_journal_page_number) = be_u32(i)?;
        Ok((i, MasterJournalHeader {
            magic: magic.to_vec(),
            version,
            pagesize,
            sector_size,
            file_format,
            pages_in_journal,
            master_journal_page_number,
        }))
    }
}

#[derive(Debug, PartialEq)]
struct WalHeader {
    magic: Vec<u8>,
    file_format: u32,
    pagesize: u16,
    checkpoint_sequence: u64,
    salt1: u32,
    salt2: u32,
    checksum_initial_value: u32,
}

impl WalHeader {
    fn parse(i: &[u8]) -> IResult<&[u8], WalHeader> {
        let (i, magic) = take(4usize)(i)?;
        let (i, file_format) = be_u32(i)?;
        let (i, pagesize) = be_u16(i)?;
        let (i, checkpoint_sequence) = be_u64(i)?;
        let (i, salt1) = be_u32(i)?;
        let (i, salt2) = be_u32(i)?;
        let (i, checksum_initial_value) = be_u32(i)?;
        Ok((i, WalHeader {
            magic: magic.to_vec(),
            file_format,
            pagesize,
            checkpoint_sequence,
            salt1,
            salt2,
            checksum_initial_value,
        }))
    }
}

#[derive(Debug, PartialEq)]
struct FileHeader {
    magic: Vec<u8>,
    page_size: u16,
    write_version: u8,
    read_version: u8,
    reserved: u8,
    max_embedded_payload_fraction: u8,
    min_embedded_payload_fraction: u8,
    leaf_payload_fraction: u8,
}

impl FileHeader {
    fn parse(i: &[u8]) -> IResult<&[u8], FileHeader> {
        let (i, magic) = take(4usize)(i)?;
        let (i, page_size) = be_u16(i)?;
        let (i, write_version) = be_u8(i)?;
        let (i, read_version) = be_u8(i)?;
        let (i, reserved) = be_u8(i)?;
        let (i, max_embedded_payload_fraction) = be_u8(i)?;
        let (i, min_embedded_payload_fraction) = be_u8(i)?;
        let (i, leaf_payload_fraction) = be_u8(i)?;
        Ok((i, FileHeader {
            magic: magic.to_vec(),
            page_size,
            write_version,
            read_version,
            reserved,
            max_embedded_payload_fraction,
            min_embedded_payload_fraction,
            leaf_payload_fraction,
        }))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();
    let (i, file_header) = FileHeader::parse(&data).unwrap();
    println!("File Header: {:?}", file_header);
    let (i, page_header) = PageHeader::parse(i).unwrap();
    println!("Page Header: {:?}", page_header);
    let (i, btree_page) = BTreePage::parse(i).unwrap();
    println!("BTree Page: {:?}", btree_page);
    let (i, master_journal_header) = MasterJournalHeader::parse(i).unwrap();
    println!("Master Journal Header: {:?}", master_journal_header);
    let (_i, wal_header) = WalHeader::parse(i).unwrap();
    println!("WAL Header: {:?}", wal_header);
}