use nom::{
    bytes::complete::tag,
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

// Define some common TIFF Tag constants
const IMAGEWIDTH: u16 = 256;
const IMAGELENGTH: u16 = 257;
const BITSPERSAMPLE: u16 = 258;
const COMPRESSION: u16 = 259;
const PHOTOMETRICINTERPRETATION: u16 = 262;
const STRIPOFFSETS: u16 = 273;
const SAMPLESPERPIXEL: u16 = 277;
const ROWSPERSTRIP: u16 = 278;
const STRIPBYTECOUNTS: u16 = 279;
const XRESOLUTION: u16 = 282;
const YRESOLUTION: u16 = 283;
const RESOLUTIONUNIT: u16 = 296;
const SOFTWARE: u16 = 305;
const DATETIME: u16 = 306;
const ARTIST: u16 = 315;
const COPYRIGHT: u16 = 33432;

// Define a structure to hold tag entries 
#[derive(Debug)]
struct TagEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

fn parse_tag_entry(i: &[u8]) -> IResult<&[u8], TagEntry> {
    let (i, tag) = le_u16(i)?;
    let (i, field_type) = le_u16(i)?;
    let (i, count) = le_u32(i)?;
    let (i, value_offset) = le_u32(i)?;
    
    Ok((i, TagEntry { tag, field_type, count, value_offset }))
}

fn parse_tiff_header(i: &[u8]) -> IResult<&[u8], u32> {
    let (i, _) = tag(b"II")(i)?;
    let (i, _) = le_u16(i)?;
    let (i, ifd_offset) = le_u32(i)?;
    Ok((i, ifd_offset))
}

fn parse_ifd(i: &[u8]) -> IResult<&[u8], Vec<TagEntry>> {
    let (mut i, num_entries) = le_u16(i)?;
    let mut entries = Vec::new();

    for _ in 0..num_entries {
        let (i_next, entry) = parse_tag_entry(i)?;
        entries.push(entry);
        i = i_next;
    }

    Ok((i, entries))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <TIFF file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let (_, ifd_offset) = parse_tiff_header(&buffer).expect("Failed to parse TIFF header");
    
    let ifd_data = &buffer[ifd_offset as usize..];
    let (_, entries) = parse_ifd(ifd_data).expect("Failed to parse IFD");

    for entry in entries {
        println!("{:?}", entry);
    }
}