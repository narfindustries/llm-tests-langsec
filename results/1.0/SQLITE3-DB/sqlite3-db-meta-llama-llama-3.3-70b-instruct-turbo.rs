use! {
    nom::{
        bytes::complete::{take},
        combinator::{map_res},
        error::{Error, ErrorKind},
        number::complete::{be_u16, be_u32, be_u64, be_u8},
        IResult,
    },
    std::env;
    std::fs::File;
    std::io::{Read};
    std::path::Path;
}

#[derive(Debug, PartialEq)]
enum ColumnType {
    Null,
    Int8,
    Int16,
    Int24,
    Int32,
    Int48,
    Int64,
    Float,
    String,
    Blob,
    ZeroTerminatedString,
}

fn column_type(input: &[u8]) -> IResult<&[u8], ColumnType> {
    let (input, typ) = be_u8(input)?;
    match typ {
        0 => Ok((input, ColumnType::Null)),
        1 => Ok((input, ColumnType::Int8)),
        2 => Ok((input, ColumnType::Int16)),
        3 => Ok((input, ColumnType::Int24)),
        4 => Ok((input, ColumnType::Int32)),
        5 => Ok((input, ColumnType::Int48)),
        6 => Ok((input, ColumnType::Int64)),
        7 => Ok((input, ColumnType::Float)),
        8 => Ok((input, ColumnType::String)),
        9 => Ok((input, ColumnType::Blob)),
        10 => Ok((input, ColumnType::ZeroTerminatedString)),
        _ => Err(nom::Err::Error(nom::error::Error::from_error_kind(input, ErrorKind::AlphaNumeric))),
    }
}

#[derive(Debug, PartialEq)]
struct Cell {
    payload_length: u64,
    header_length: u64,
    num_columns: u64,
    column_types: Vec<ColumnType>,
    payload: Vec<u8>,
}

fn cell(input: &[u8]) -> IResult<&[u8], Cell> {
    let (input, payload_length) = map_res(be_u64, |x| {
        if x > (1 << 60) - 1 {
            Err(nom::error::ErrorKind::AlphaNumeric)
        } else {
            Ok(x as u64)
        }
    })(input)?;

    let (input, header_length) = be_u64(input)?;

    let (input, num_columns) = be_u64(input)?;

    let (input, column_types) = take(num_columns as usize)(input)?;

    let mut column_types_result = Vec::new();
    for col_type in column_types.chunks(1) {
        let (_, typ) = column_type(col_type)?;
        column_types_result.push(typ);
    }

    let (input, payload) = take(payload_length as usize)(input)?;

    Ok((
        input,
        Cell {
            payload_length,
            header_length,
            num_columns,
            column_types: column_types_result,
            payload: payload.to_vec(),
        },
    ))
}

#[derive(Debug, PartialEq)]
enum PageType {
    Purgable,
    TableBTreeRoot,
    TableBTreeInterior,
    TableBTreeLeaf,
    Free,
    IndexBTreeRoot,
    IndexBTreeInterior,
    IndexBTreeLeaf,
}

fn page_type(input: &[u8]) -> IResult<&[u8], PageType> {
    let (input, typ) = be_u8(input)?;
    match typ {
        0 => Ok((input, PageType::Purgable)),
        1 => Ok((input, PageType::TableBTreeRoot)),
        2 => Ok((input, PageType::TableBTreeInterior)),
        3 => Ok((input, PageType::TableBTreeLeaf)),
        4 => Ok((input, PageType::Free)),
        5 => Ok((input, PageType::IndexBTreeRoot)),
        6 => Ok((input, PageType::IndexBTreeInterior)),
        7 => Ok((input, PageType::IndexBTreeLeaf)),
        _ => Err(nom::Err::Error(nom::error::Error::from_error_kind(input, ErrorKind::AlphaNumeric))),
    }
}

#[derive(Debug, PartialEq)]
struct PageHeader {
    page_type: PageType,
    first_freeblock: u16,
    cell_offset_array_start: u16,
    num_cells: u16,
    cell_start_offset: u16,
    free_offsets_array_length: u16,
}

fn page_header(input: &[u8]) -> IResult<&[u8], PageHeader> {
    let (input, page_type) = page_type(input)?;
    let (input, first_freeblock) = be_u16(input)?;
    let (input, cell_offset_array_start) = be_u16(input)?;
    let (input, num_cells) = be_u16(input)?;
    let (input, cell_start_offset) = be_u16(input)?;
    let (input, free_offsets_array_length) = be_u16(input)?;
    Ok((
        input,
        PageHeader {
            page_type,
            first_freeblock,
            cell_offset_array_start,
            num_cells,
            cell_start_offset,
            free_offsets_array_length,
        },
    ))
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
    file_format_write_version: u32,
    file_format_read_version: u32,
}

fn file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, magic) = take(4usize)(input)?;
    let magic = magic.to_vec();
    if &magic != &[0x53, 0x51, 0x4c, 0x69] {
        return Err(nom::Err::Error(nom::error::Error::from_error_kind(input, ErrorKind::AlphaNumeric)));
    }
    let (input, page_size) = be_u16(input)?;
    let (input, write_version) = be_u8(input)?;
    let (input, read_version) = be_u8(input)?;
    let (input, reserved) = be_u8(input)?;
    let (input, max_embedded_payload_fraction) = be_u8(input)?;
    let (input, min_embedded_payload_fraction) = be_u8(input)?;
    let (input, leaf_payload_fraction) = be_u8(input)?;
    let (input, file_format_write_version) = be_u32(input)?;
    let (input, file_format_read_version) = be_u32(input)?;
    Ok((
        input,
        FileHeader {
            magic,
            page_size,
            write_version,
            read_version,
            reserved,
            max_embedded_payload_fraction,
            min_embedded_payload_fraction,
            leaf_payload_fraction,
            file_format_write_version,
            file_format_read_version,
        },
    ))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }
    let file_path = Path::new(&args[1]);
    let mut file = match File::open(file_path) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return Ok(());
        }
    };
    let mut data = Vec::new();
    match file.read_to_end(&mut data) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return Ok(());
        }
    }
    let data = &data[..];
    match file_header(data) {
        Ok((_remaining, file_header)) => {
            println!("File Header: {:?}", file_header);
        }
        Err(err) => {
            println!("Error parsing file header: {:?}", err);
            return Ok(());
        }
    }
    let pages = (data[16..]).chunks(4096);
    for (i, page) in pages.enumerate() {
        match page_header(page) {
            Ok((_remaining, page_header)) => {
                println!("Page Header {}: {:?}", i, page_header);
            }
            Err(err) => {
                println!("Error parsing page header {}: {:?}", i, err);
                break;
            }
        }
        match cell(page) {
            Ok((_remaining, cell)) => {
                println!("Cell {}: {:?}", i, cell);
            }
            Err(err) => {
                println!("Error parsing cell {}: {:?}", i, err);
                break;
            }
        }
    }
    Ok(())
}