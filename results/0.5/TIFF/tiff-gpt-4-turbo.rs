use nom::{
    bytes::complete::tag,
    combinator::map_res,
    multi::count,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
    error::VerboseError,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct TiffHeader {
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

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader, VerboseError<&[u8]>> {
    let (input, (byte_order, magic_number, ifd_offset)) = tuple((
        map_res(tag(b"II\x2a\x00"), |s: &[u8]| -> Result<_, io::Error> {
            Ok(String::from_utf8_lossy(s).to_string())
        }),
        le_u16,
        le_u32,
    ))(input)?;

    Ok((
        input,
        TiffHeader {
            byte_order,
            magic_number,
            ifd_offset,
        },
    ))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IFDEntry, VerboseError<&[u8]>> {
    let (input, (tag, field_type, count, value_offset)) =
        tuple((le_u16, le_u16, le_u32, le_u32))(input)?;

    Ok((
        input,
        IFDEntry {
            tag,
            field_type,
            count,
            value_offset,
        },
    ))
}

fn parse_ifd(input: &[u8], number_of_entries: usize) -> IResult<&[u8], IFD, VerboseError<&[u8]>> {
    let (input, entries) = count(parse_ifd_entry, number_of_entries)(input)?;

    Ok((input, IFD { entries }))
}

fn read_file_to_buffer(filename: &str) -> Result<Vec<u8>, io::Error> {
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err("Usage: tiff_parser <FILE>".into());
    }

    let filename = &args[1];
    let buffer = read_file_to_buffer(filename)?;

    let (_, header) = parse_tiff_header(&buffer)?;
    println!("Parsed TIFF Header: {:?}", header);
    let ifd_start = header.ifd_offset as usize;

    let (_, num_entries) = le_u16::<_, VerboseError<_>>(&buffer[ifd_start..])?;
    let (_, ifd) = parse_ifd(&buffer[ifd_start + 2..], num_entries as usize)?;
    println!("Parsed IFD: {:?}", ifd);

    Ok(())
}