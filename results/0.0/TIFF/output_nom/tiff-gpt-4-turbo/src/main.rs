use nom::{
    bytes::complete::take,
    combinator::map,
    multi::count,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::{
    env, fs::File, io::Read, path::Path,
};

#[derive(Debug)]
struct TIFFHeader {
    byte_order: String,
    magic_number: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
struct IFDEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
struct IFD {
    entries: Vec<IFDEntry>,
}

fn parse_byte_order(input: &[u8]) -> IResult<&[u8], String> {
    map(take(2usize), |bytes: &[u8]| {
        if bytes == b"II" {
            "Little endian".to_string()
        } else {
            "Big endian".to_string()
        }
    })(input)
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TIFFHeader> {
    map(
        tuple((parse_byte_order, le_u16, le_u32)),
        |(byte_order, magic_number, ifd_offset)| TIFFHeader {
            byte_order,
            magic_number,
            ifd_offset,
        },
    )(input)
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IFDEntry> {
    map(tuple((le_u16, le_u16, le_u32, le_u32)), |(tag, field_type, count, value_offset)| IFDEntry {
        tag,
        field_type,
        count,
        value_offset,
    })(input)
}

fn parse_ifd(input: &[u8], num_entries: usize) -> IResult<&[u8], IFD> {
    map(count(parse_ifd_entry, num_entries), |entries| IFD { entries })(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <TIFF_FILE>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let (rest, header) = parse_tiff_header(&buffer)?;
    println!("TIFF Header: {:?}", header);

    let ifd_start = header.ifd_offset as usize;
    let (rest, num_entries) = le_u16::<_, nom::error::Error<&[u8]>>(&rest[ifd_start..])?;
    let (_, ifd) = parse_ifd(rest, num_entries as usize)?;
    println!("IFD: {:?}", ifd);

    Ok(())
}