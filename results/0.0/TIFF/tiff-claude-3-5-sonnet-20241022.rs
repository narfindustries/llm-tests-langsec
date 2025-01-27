use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::many0,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct TiffHeader {
    byte_order: u16,
    version: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
struct IfdEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
struct Tiff {
    header: TiffHeader,
    ifds: Vec<Vec<IfdEntry>>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = verify(le_u16, |&x| x == 0x4949)(input)?;
    let (input, version) = verify(le_u16, |&x| x == 42)(input)?;
    let (input, ifd_offset) = le_u32(input)?;

    Ok((
        input,
        TiffHeader {
            byte_order,
            version,
            ifd_offset,
        },
    ))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IfdEntry> {
    let (input, (tag, field_type, count, value_offset)) =
        tuple((le_u16, le_u16, le_u32, le_u32))(input)?;

    Ok((
        input,
        IfdEntry {
            tag,
            field_type,
            count,
            value_offset,
        },
    ))
}

fn parse_ifd(input: &[u8]) -> IResult<&[u8], (Vec<IfdEntry>, u32)> {
    let (input, entry_count) = le_u16(input)?;
    let (input, entries) = many0(parse_ifd_entry)(input)?;
    let (input, next_ifd_offset) = le_u32(input)?;

    Ok((input, (entries, next_ifd_offset)))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], Tiff> {
    let (mut input, header) = parse_header(input)?;
    let mut ifds = Vec::new();
    let mut next_offset = header.ifd_offset;

    while next_offset != 0 {
        let offset = next_offset as usize;
        if offset >= input.len() {
            break;
        }
        input = &input[offset..];
        let (new_input, (entries, next_ifd_offset)) = parse_ifd(input)?;
        input = new_input;
        ifds.push(entries);
        next_offset = next_ifd_offset;
    }

    Ok((input, Tiff { header, ifds }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tiff(&buffer) {
        Ok((_, tiff)) => println!("{:#?}", tiff),
        Err(e) => eprintln!("Error parsing TIFF: {:?}", e),
    }

    Ok(())
}