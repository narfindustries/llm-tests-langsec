use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct TiffHeader {
    byte_order: u16,
    magic: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
struct IfdEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: [u8; 4],
}

#[derive(Debug)]
struct Ifd {
    num_entries: u16,
    entries: Vec<IfdEntry>,
    next_ifd_offset: u32,
}

#[derive(Debug)]
struct Tiff {
    header: TiffHeader,
    ifds: Vec<Ifd>,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, (byte_order, magic, ifd_offset)) = tuple((le_u16, le_u16, le_u32))(input)?;
    Ok((
        input,
        TiffHeader {
            byte_order,
            magic,
            ifd_offset,
        },
    ))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IfdEntry> {
    let (input, (tag, field_type, count, value_offset)) =
        tuple((le_u16, le_u16, le_u32, take(4usize)))(input)?;
    Ok((
        input,
        IfdEntry {
            tag,
            field_type,
            count,
            value_offset: value_offset.try_into().unwrap(),
        },
    ))
}

fn parse_ifd(input: &[u8]) -> IResult<&[u8], Ifd> {
    let (input, num_entries) = le_u16(input)?;
    let (input, entries) = nom::multi::count(parse_ifd_entry, num_entries as usize)(input)?;
    let (input, next_ifd_offset) = le_u32(input)?;
    Ok((
        input,
        Ifd {
            num_entries,
            entries,
            next_ifd_offset,
        },
    ))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], Tiff> {
    let (input, header) = parse_tiff_header(input)?;
    let mut ifds = Vec::new();
    let mut next_ifd_offset = header.ifd_offset;

    while next_ifd_offset != 0 {
        let (remaining_input, ifd) = parse_ifd(&input[next_ifd_offset as usize..])?;
        next_ifd_offset = ifd.next_ifd_offset;
        ifds.push(ifd);
    }

    Ok((input, Tiff { header, ifds }))
}

fn main() -> io::Result<()> {
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
        Err(e) => eprintln!("Failed to parse TIFF: {:?}", e),
    }

    Ok(())
}