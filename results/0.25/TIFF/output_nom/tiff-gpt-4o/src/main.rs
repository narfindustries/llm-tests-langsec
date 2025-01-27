use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    number::complete::{le_u16, be_u16, le_u32, be_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
enum Endian {
    Little,
    Big,
}

#[derive(Debug)]
struct TiffHeader {
    endian: Endian,
    magic_number: u16,
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
struct Ifd {
    entries: Vec<IfdEntry>,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, (endian_tag, magic_number, ifd_offset)) = tuple((
        take(2usize),
        map_res(take(2usize), |bytes: &[u8]| {
            match bytes {
                b"II" => Ok((Endian::Little, le_u16)),
                b"MM" => Ok((Endian::Big, be_u16)),
                _ => Err(()),
            }
        }),
        map_res(take(4usize), |bytes: &[u8]| {
            match bytes {
                b"II" => Ok((Endian::Little, le_u32)),
                b"MM" => Ok((Endian::Big, be_u32)),
                _ => Err(()),
            }
        }),
    ))(input)?;

    let (endian, read_u16) = magic_number?;
    let (endian, read_u32) = ifd_offset?;

    let (input, magic_number) = read_u16(input)?;
    let (input, ifd_offset) = read_u32(input)?;

    Ok((input, TiffHeader { endian, magic_number, ifd_offset }))
}

fn parse_ifd_entry(input: &[u8], endian: &Endian) -> IResult<&[u8], IfdEntry> {
    let (input, (tag, field_type, count, value_offset)) = match endian {
        Endian::Little => tuple((le_u16, le_u16, le_u32, le_u32))(input),
        Endian::Big => tuple((be_u16, be_u16, be_u32, be_u32))(input),
    }?;

    Ok((input, IfdEntry { tag, field_type, count, value_offset }))
}

fn parse_ifd(input: &[u8], endian: &Endian) -> IResult<&[u8], Ifd> {
    let (input, num_entries) = match endian {
        Endian::Little => le_u16(input),
        Endian::Big => be_u16(input),
    }?;

    let mut entries = Vec::new();
    let mut input = input;
    for _ in 0..num_entries {
        let (new_input, entry) = parse_ifd_entry(input, endian)?;
        entries.push(entry);
        input = new_input;
    }

    Ok((input, Ifd { entries }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Could not open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");

    let (_, tiff_header) = parse_tiff_header(&buffer).expect("Failed to parse TIFF header");
    println!("{:?}", tiff_header);

    let ifd_offset = tiff_header.ifd_offset as usize;
    let (_, ifd) = parse_ifd(&buffer[ifd_offset..], &tiff_header.endian).expect("Failed to parse IFD");
    println!("{:?}", ifd);
}