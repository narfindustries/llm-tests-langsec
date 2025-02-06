use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct NITFHeader {
    fhdr: String,
    fver: String,
    clevel: u8,
    stype: String,
    ostaid: String,
    fdt: String,
    ftitle: String,
    numi: u16,
    nums: u16,
    numx: u16,
}

#[derive(Debug)]
struct NITFImageSegment {
    im: String,
    iid1: String,
    isclas: String,
    encryp: u8,
    isorce: String,
    nrows: u32,
    ncols: u32,
}

#[derive(Debug)]
struct NITFGraphicSegment {
    sid: String,
    sname: String,
    smpv: u16,
}

#[derive(Debug)]
struct NITFTextSegment {
    textid: String,
    txtalvl: u8,
}

#[derive(Debug)]
struct NITFDataExtensionSegment {
    desid: String,
    desver: u8,
}

#[derive(Debug)]
struct NITFReservedExtensionSegment {
    res: String,
}

#[derive(Debug)]
struct NITFFile {
    header: NITFHeader,
    image_segments: Vec<NITFImageSegment>,
    graphic_segments: Vec<NITFGraphicSegment>,
    text_segments: Vec<NITFTextSegment>,
    data_extension_segments: Vec<NITFDataExtensionSegment>,
    reserved_extension_segments: Vec<NITFReservedExtensionSegment>,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, fhdr) = take(4u8)(input)?;
    let (input, fver) = take(4u8)(input)?;
    let (input, clevel) = take(1u8)(input)?;
    let (input, stype) = take(4u8)(input)?;
    let (input, ostaid) = take(10u8)(input)?;
    let (input, fdt) = take(14u8)(input)?;
    let (input, ftitle) = take(80u8)(input)?;
    let (input, numi) = be_u16(input)?;
    let (input, nums) = be_u16(input)?;
    let (input, numx) = be_u16(input)?;

    Ok((input, NITFHeader {
        fhdr: String::from_utf8_lossy(fhdr).to_string(),
        fver: String::from_utf8_lossy(fver).to_string(),
        clevel: clevel[0],
        stype: String::from_utf8_lossy(stype).to_string(),
        ostaid: String::from_utf8_lossy(ostaid).to_string(),
        fdt: String::from_utf8_lossy(fdt).to_string(),
        ftitle: String::from_utf8_lossy(ftitle).to_string(),
        numi,
        nums,
        numx,
    }))
}

fn parse_nitf_image_segment(input: &[u8]) -> IResult<&[u8], NITFImageSegment> {
    let (input, im) = take(2u8)(input)?;
    let (input, iid1) = take(10u8)(input)?;
    let (input, isclas) = take(1u8)(input)?;
    let (input, encryp) = take(1u8)(input)?;
    let (input, isorce) = take(42u8)(input)?;
    let (input, nrows) = be_u32(input)?;
    let (input, ncols) = be_u32(input)?;

    Ok((input, NITFImageSegment {
        im: String::from_utf8_lossy(im).to_string(),
        iid1: String::from_utf8_lossy(iid1).to_string(),
        isclas: String::from_utf8_lossy(isclas).to_string(),
        encryp: encryp[0],
        isorce: String::from_utf8_lossy(isorce).to_string(),
        nrows,
        ncols,
    }))
}

fn parse_nitf_graphic_segment(input: &[u8]) -> IResult<&[u8], NITFGraphicSegment> {
    let (input, sid) = take(10u8)(input)?;
    let (input, sname) = take(20u8)(input)?;
    let (input, smpv) = be_u16(input)?;

    Ok((input, NITFGraphicSegment {
        sid: String::from_utf8_lossy(sid).to_string(),
        sname: String::from_utf8_lossy(sname).to_string(),
        smpv,
    }))
}

fn parse_nitf_text_segment(input: &[u8]) -> IResult<&[u8], NITFTextSegment> {
    let (input, textid) = take(10u8)(input)?;
    let (input, txtalvl) = take(1u8)(input)?;

    Ok((input, NITFTextSegment {
        textid: String::from_utf8_lossy(textid).to_string(),
        txtalvl: txtalvl[0],
    }))
}

fn parse_nitf_data_extension_segment(input: &[u8]) -> IResult<&[u8], NITFDataExtensionSegment> {
    let (input, desid) = take(25u8)(input)?;
    let (input, desver) = take(1u8)(input)?;

    Ok((input, NITFDataExtensionSegment {
        desid: String::from_utf8_lossy(desid).to_string(),
        desver: desver[0],
    }))
}

fn parse_nitf_reserved_extension_segment(input: &[u8]) -> IResult<&[u8], NITFReservedExtensionSegment> {
    let (input, res) = take(100u8)(input)?;

    Ok((input, NITFReservedExtensionSegment {
        res: String::from_utf8_lossy(res).to_string(),
    }))
}

fn parse_nitf(input: &[u8]) -> IResult<&[u8], NITFFile> {
    let (input, header) = parse_nitf_header(input)?;

    let mut image_segments = Vec::new();
    let mut graphic_segments = Vec::new();
    let mut text_segments = Vec::new();
    let mut data_extension_segments = Vec::new();
    let mut reserved_extension_segments = Vec::new();

    let mut remaining_input = input;

    for _ in 0..header.numi {
        let (remaining, image_segment) = parse_nitf_image_segment(remaining_input)?;
        remaining_input = remaining;
        image_segments.push(image_segment);
    }

    for _ in 0..header.nums {
        let (remaining, graphic_segment) = parse_nitf_graphic_segment(remaining_input)?;
        remaining_input = remaining;
        graphic_segments.push(graphic_segment);
    }

    for _ in 0..header.numx {
        let (remaining, text_segment) = parse_nitf_text_segment(remaining_input)?;
        remaining_input = remaining;
        text_segments.push(text_segment);
    }

    for _ in 0..header.numx {
        let (remaining, data_extension_segment) = parse_nitf_data_extension_segment(remaining_input)?;
        remaining_input = remaining;
        data_extension_segments.push(data_extension_segment);
    }

    for _ in 0..header.numx {
        let (remaining, reserved_extension_segment) = parse_nitf_reserved_extension_segment(remaining_input)?;
        remaining_input = remaining;
        reserved_extension_segments.push(reserved_extension_segment);
    }

    Ok((remaining_input, NITFFile {
        header,
        image_segments,
        graphic_segments,
        text_segments,
        data_extension_segments,
        reserved_extension_segments,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <NITF file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_nitf(&buffer) {
        Ok((_, nitf_file)) => println!("{:#?}", nitf_file),
        Err(e) => eprintln!("Error parsing NITF file: {:?}", e),
    }
}