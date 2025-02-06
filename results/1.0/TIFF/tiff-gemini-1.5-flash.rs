use nom::{
    bytes::complete::{take},
    combinator::{map, opt},
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct TiffHeader {
    byte_order: u16,
    version: u16,
    offset: u32,
}

#[derive(Debug)]
struct TiffIFD {
    entries: Vec<TiffEntry>,
}


#[derive(Debug)]
struct TiffEntry {
    tag: u16,
    type_: u16,
    count: u32,
    value: Vec<u8>,
}


fn tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    map(
        tuple((be_u16, be_u16, be_u32)),
        |(byte_order, version, offset)| TiffHeader {
            byte_order,
            version,
            offset,
        },
    )(input)
}

fn tiff_entry(input: &[u8]) -> IResult<&[u8], TiffEntry> {
    let (input, (tag, type_, count, value)) = tuple((be_u16, be_u16, be_u32, take(4_u32)))(input)?;
    Ok((input, TiffEntry { tag, type_, count, value: value.to_vec() }))
}


fn tiff_ifd(input: &[u8]) -> IResult<&[u8], TiffIFD> {
    let (input, num_entries) = be_u16(input)?;
    let (input, entries) = (0..num_entries).fold(Ok((input, Vec::new())), |res, _| {
        res.and_then(|(input, mut acc)| {
            match tiff_entry(input) {
                Ok((i, entry)) => {
                    acc.push(entry);
                    Ok((i, acc))
                },
                Err(e) => Err(e),
            }
        })
    })?;

    let (input, next_ifd_offset) = opt(be_u32)(input)?;

    Ok((input, TiffIFD { entries }))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <tiff_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match tiff_header(&buffer) {
        Ok((_, header)) => {
            println!("TIFF Header: {:?}", header);
            let offset = header.offset as usize;
            if offset < buffer.len() {
                match tiff_ifd(&buffer[offset..]) {
                    Ok((_, ifd)) => {
                        println!("IFD Entries: {:?}", ifd);
                    }
                    Err(e) => println!("Error parsing IFD: {:?}", e),
                }
            } else {
                println!("IFD offset out of bounds");
            }

        }
        Err(e) => println!("Error parsing TIFF header: {:?}", e),
    }
}
