use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt, peek, recognize},
    number::complete::be_u16,
    sequence::{preceded, tuple},
    IResult,
};

#[derive(Debug)]
enum TiffTag {
    ImageWidth,
    ImageLength,
    BitsPerSample,
    Compression,
    PhotometricInterpretation,
    StripOffsets,
    SamplesPerPixel,
    RowsPerStrip,
    StripByteCounts,
    XResolution,
    YResolution,
    ResolutionUnit,
    // Add other tags as needed...
    Unknown(u16),
}

#[derive(Debug)]
struct TiffEntry {
    tag: TiffTag,
    type_: u16,
    count: u32,
    value: Vec<u8>,
}

fn tiff_header(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag("II")(input)?;
    let (input, _) = be_u16(input)?; // 42
    Ok((input, ()))
}

fn tiff_entry(input: &[u8]) -> IResult<&[u8], TiffEntry> {
    let (input, tag) = be_u16(input)?;
    let (input, type_) = be_u16(input)?;
    let (input, count) = be_u32(input)?;
    let (input, value_offset) = be_u32(input)?;

    let value_len = type_ as usize * count as usize;
    let value = if value_len <= 4 {
        let (input, value) = take(value_len)(input)?;
        value.to_vec()
    } else {
        // Handle values larger than 4 bytes
        unimplemented!()
    };

    Ok((
        input,
        TiffEntry {
            tag: match tag {
                256 => TiffTag::ImageWidth,
                257 => TiffTag::ImageLength,
                258 => TiffTag::BitsPerSample,
                259 => TiffTag::Compression,
                262 => TiffTag::PhotometricInterpretation,
                273 => TiffTag::StripOffsets,
                277 => TiffTag::SamplesPerPixel,
                278 => TiffTag::RowsPerStrip,
                279 => TiffTag::StripByteCounts,
                282 => TiffTag::XResolution,
                283 => TiffTag::YResolution,
                296 => TiffTag::ResolutionUnit,
                _ => TiffTag::Unknown(tag),
            },
            type_,
            count,
            value,
        },
    ))
}

fn tiff_ifd(input: &[u8]) -> IResult<&[u8], Vec<TiffEntry>> {
    let (input, entry_count) = be_u16(input)?;
    let (input, entries) = (0..entry_count).fold(Ok((input, Vec::new())), |acc, _| {
        acc.and_then(|(i, mut v)| {
            tiff_entry(i).map(|(i2, entry)| (i2, { v.push(entry); v }))
        })
    })?;
    Ok((input, entries))
}

fn be_u32(input: &[u8]) -> IResult<&[u8], u32> {
    map(take(4usize), |bytes: &[u8]| {
        u32::from_be_bytes(bytes.try_into().unwrap())
    })(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match tiff_header(&buffer) {
        Ok((remaining, _)) => {
            match tiff_ifd(remaining) {
                Ok((_, entries)) => {
                    println!("TIFF Entries: {:?}", entries);
                }
                Err(e) => {
                    eprintln!("Error parsing IFD: {:?}", e);
                }
            }
        }
        Err(e) => {
            eprintln!("Error parsing header: {:?}", e);
        }
    }
}
