// Cargo.toml dependencies:
// [dependencies]
// nom = "7.1"
// clap = { version = "4.0", features = ["derive"] }
// byteorder = "1.4"

use std::fs::File;
use std::io::Read;
use std::path::Path;
use byteorder::{LittleEndian, BigEndian, ReadBytesExt};
use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::{le_u16, be_u16, le_u32, be_u32},
};
use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    /// Input TIFF file
    #[arg(short, long)]
    input: String,
}

#[derive(Debug)]
enum Endian {
    Little,
    Big,
}

#[derive(Debug)]
struct TiffHeader {
    byte_order: Endian,
    magic_number: u16,
    ifd_offset: u32,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = nom::branch::alt((tag(b"II"), tag(b"MM")))(input)?;
    let endian = if byte_order == b"II" { Endian::Little } else { Endian::Big };

    let (input, magic_number) = match endian {
        Endian::Little => le_u16(input)?,
        Endian::Big => be_u16(input)?,
    };

    if magic_number != 42 {
        return Err(nom::Err::Error((input, nom::error::ErrorKind::Tag)));
    }

    let (input, ifd_offset) = match endian {
        Endian::Little => le_u32(input)?,
        Endian::Big => be_u32(input)?,
    };

    Ok((input, TiffHeader { byte_order: endian, magic_number, ifd_offset }))
}

#[derive(Debug)]
struct TiffEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

fn parse_tiff_entry(input: &[u8], endian: &Endian) -> IResult<&[u8], TiffEntry> {
    let (input, tag) = match endian {
        Endian::Little => le_u16(input)?,
        Endian::Big => be_u16(input)?,
    };

    let (input, field_type) = match endian {
        Endian::Little => le_u16(input)?,
        Endian::Big => be_u16(input)?,
    };

    let (input, count) = match endian {
        Endian::Little => le_u32(input)?,
        Endian::Big => be_u32(input)?,
    };

    let (input, value_offset) = match endian {
        Endian::Little => le_u32(input)?,
        Endian::Big => be_u32(input)?,
    };

    Ok((input, TiffEntry { tag, field_type, count, value_offset }))
}

fn parse_ifd(input: &[u8], endian: &Endian) -> IResult<&[u8], Vec<TiffEntry>> {
    let (input, num_entries) = match endian {
        Endian::Little => le_u16(input)?,
        Endian::Big => be_u16(input)?,
    };
    let mut entries = Vec::new();
    let mut input = input;
    for _ in 0..num_entries {
        let (new_input, entry) = parse_tiff_entry(input, endian)?;
        entries.push(entry);
        input = new_input;
    }
    Ok((input, entries))
}

fn main() {
    let args = Args::parse();
    let path = Path::new(&args.input);

    let mut file = File::open(&path).expect("Could not open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");

    match parse_tiff_header(&buffer) {
        Ok((remaining, header)) => {
            println!("Parsed TIFF header: {:?}", header);
            let ifd_start = header.ifd_offset as usize;
            if ifd_start < buffer.len() {
                match parse_ifd(&buffer[ifd_start..], &header.byte_order) {
                    Ok((_, entries)) => {
                        println!("Parsed IFD entries: {:?}", entries);
                    }
                    Err(e) => eprintln!("Failed to parse IFD: {:?}", e),
                }
            } else {
                eprintln!("Invalid IFD offset");
            }
        },
        Err(e) => eprintln!("Failed to parse TIFF header: {:?}", e),
    }
}