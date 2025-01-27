use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, rest},
    number::complete::{be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct TiffHeader {
    byte_order: [u8; 2],
    version: u16,
    offset: u32,
}

#[derive(Debug)]
struct IfdEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

fn tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    map(
        tuple((tag(b"II"), be_u16, be_u32)),
        |(byte_order, version, offset)| TiffHeader {
            byte_order: [byte_order[0], byte_order[1]],
            version,
            offset,
        },
    )(input)
}


fn ifd_entry(input: &[u8]) -> IResult<&[u8], IfdEntry> {
    map(
        tuple((be_u16, be_u16, be_u32, be_u32)),
        |(tag, field_type, count, value_offset)| IfdEntry {
            tag,
            field_type,
            count,
            value_offset,
        },
    )(input)
}

fn ifd(input: &[u8]) -> IResult<&[u8], Vec<IfdEntry>> {
    let (input, num_entries) = be_u16(input)?;
    let (input, entries) = nom::multi::count(ifd_entry, num_entries as usize)(input)?;
    let (input, next_ifd_offset) = be_u32(input)?;
    Ok((input, entries))

}

fn tiff(input: &[u8]) -> IResult<&[u8], (TiffHeader, Vec<IfdEntry>)> {
    let (input, header) = tiff_header(input)?;
    let (input, _) = take(4usize)(input)?; //skip to IFD
    let (input, ifd_data) = ifd(input)?;
    Ok((input, (header, ifd_data)))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <tiff_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    match tiff(&buffer) {
        Ok((_, (header, ifd))) => {
            println!("Header: {:?}", header);
            println!("IFD Entries: {:?}", ifd);
        }
        Err(e) => {
            println!("Error parsing TIFF: {:?}", e);
        }
    }
}
