use nom::{
    bytes::complete::{take, take_until},
    combinator::{map_res, verify},
    multi::many1,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug, PartialEq)]
struct TiffHeader {
    byte_order: String,
    magic_number: u16,
    ifd_offset: u32,
}

#[derive(Debug, PartialEq)]
struct IFDEntry {
    tag: u16,
    typ: u16,
    count: u32,
    offset: u32,
}

#[derive(Debug)]
struct TiffData {
    header: TiffHeader,
    ifd_entries: Vec<IFDEntry>,
}

fn parse_byte_order(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take(2usize), |bytes: &[u8]| match bytes {
        b"II" => Ok("Little endian".to_string()),
        b"MM" => Ok("Big endian".to_string()),
        _ => Err("Invalid byte order"),
    })(input)
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, (byte_order, magic_number, ifd_offset)) = tuple((parse_byte_order, le_u16, le_u32))(input)?;
    Ok((input, TiffHeader {
        byte_order,
        magic_number,
        ifd_offset,
    }))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IFDEntry> {
    let (input, (tag, typ, count, offset)) = tuple((le_u16, le_u16, le_u32, le_u32))(input)?;
    Ok((input, IFDEntry { tag, typ, count, offset }))
}

fn parse_ifd_entries(input: &[u8], number_of_entries: u16) -> IResult<&[u8], Vec<IFDEntry>> {
    many1(parse_ifd_entry)(input)
}

fn read_tiff(data: &[u8]) -> IResult<&[u8], TiffData> {
    let (input, header) = parse_tiff_header(data)?;
    let (input, _) = take(header.ifd_offset - 8)(input)?;
    let (input, number_of_entries) = le_u16(input)?;
    let (input, ifd_entries) = parse_ifd_entries(input, number_of_entries)?;

    Ok((input, TiffData { header, ifd_entries }))
}

fn read_file(path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <path_to_tiff_file>", args[0]);
        return Ok(());
    }
    let path = &args[1];
    let data = read_file(path)?;
    match read_tiff(&data) {
        Ok((_remaining, tiff_data)) => {
            println!("{:?}", tiff_data);
        }
        Err(e) => eprintln!("Failed to parse TIFF: {:?}", e),
    }
    Ok(())
}