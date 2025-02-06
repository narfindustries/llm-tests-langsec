use nom::{
    bytes::complete::tag,
    number::complete::{be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct NITFHeader {
    fhdr: String,
    clevel: u16,
    stype: String,
    ostaid: String,
    fdt: String,
    ftitle: String,
    fl: u64,
    hl: u64,
    numi: u16,
    nums: u16,
    numx: u16,
    numt: u16,
    numdes: u16,
}

#[derive(Debug)]
struct ImageSegmentSubheader {
    im: String,
    iid: String,
    idatim: String,
    tgtid: String,
    isorce: String,
    nrows: u32,
    ncols: u32,
    pvtype: String,
    irep: String,
    icat: String,
}

#[derive(Debug)]
struct SymbolSegmentSubheader {
    sy: String,
    sid: String,
    sname: String,
    sloc: String,
}

#[derive(Debug)]
struct TextSegmentSubheader {
    te: String,
    textid: String,
    txtalvl: String,
    txtdt: String,
    txtitl: String,
}

#[derive(Debug)]
struct DataExtensionSegmentSubheader {
    de: String,
    desid: String,
    desver: String,
    desoflw: String,
}

#[derive(Debug)]
struct ReservedExtensionSegmentSubheader {
    re: String,
    resid: String,
    resver: String,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, (fhdr, clevel, stype, ostaid, fdt, ftitle, fl, hl, numi, nums, numx, numt, numdes)) = tuple((
        tag("NITF"),
        be_u16,
        nom::bytes::complete::take(4usize),
        nom::bytes::complete::take(10usize),
        nom::bytes::complete::take(14usize),
        nom::bytes::complete::take(80usize),
        be_u64,
        be_u64,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
    ))(input)?;

    Ok((
        input,
        NITFHeader {
            fhdr: String::from_utf8_lossy(fhdr).to_string(),
            clevel,
            stype: String::from_utf8_lossy(stype).to_string(),
            ostaid: String::from_utf8_lossy(ostaid).to_string(),
            fdt: String::from_utf8_lossy(fdt).to_string(),
            ftitle: String::from_utf8_lossy(ftitle).to_string(),
            fl,
            hl,
            numi,
            nums,
            numx,
            numt,
            numdes,
        },
    ))
}

fn parse_image_segment_subheader(input: &[u8]) -> IResult<&[u8], ImageSegmentSubheader> {
    let (input, (im, iid, idatim, tgtid, isorce, nrows, ncols, pvtype, irep, icat)) = tuple((
        tag("IM"),
        nom::bytes::complete::take(10usize),
        nom::bytes::complete::take(14usize),
        nom::bytes::complete::take(17usize),
        nom::bytes::complete::take(42usize),
        be_u32,
        be_u32,
        nom::bytes::complete::take(3usize),
        nom::bytes::complete::take(8usize),
        nom::bytes::complete::take(8usize),
    ))(input)?;

    Ok((
        input,
        ImageSegmentSubheader {
            im: String::from_utf8_lossy(im).to_string(),
            iid: String::from_utf8_lossy(iid).to_string(),
            idatim: String::from_utf8_lossy(idatim).to_string(),
            tgtid: String::from_utf8_lossy(tgtid).to_string(),
            isorce: String::from_utf8_lossy(isorce).to_string(),
            nrows,
            ncols,
            pvtype: String::from_utf8_lossy(pvtype).to_string(),
            irep: String::from_utf8_lossy(irep).to_string(),
            icat: String::from_utf8_lossy(icat).to_string(),
        },
    ))
}

fn parse_symbol_segment_subheader(input: &[u8]) -> IResult<&[u8], SymbolSegmentSubheader> {
    let (input, (sy, sid, sname, sloc)) = tuple((
        tag("SY"),
        nom::bytes::complete::take(10usize),
        nom::bytes::complete::take(20usize),
        nom::bytes::complete::take(10usize),
    ))(input)?;

    Ok((
        input,
        SymbolSegmentSubheader {
            sy: String::from_utf8_lossy(sy).to_string(),
            sid: String::from_utf8_lossy(sid).to_string(),
            sname: String::from_utf8_lossy(sname).to_string(),
            sloc: String::from_utf8_lossy(sloc).to_string(),
        },
    ))
}

fn parse_text_segment_subheader(input: &[u8]) -> IResult<&[u8], TextSegmentSubheader> {
    let (input, (te, textid, txtalvl, txtdt, txtitl)) = tuple((
        tag("TE"),
        nom::bytes::complete::take(10usize),
        nom::bytes::complete::take(2usize),
        nom::bytes::complete::take(14usize),
        nom::bytes::complete::take(80usize),
    ))(input)?;

    Ok((
        input,
        TextSegmentSubheader {
            te: String::from_utf8_lossy(te).to_string(),
            textid: String::from_utf8_lossy(textid).to_string(),
            txtalvl: String::from_utf8_lossy(txtalvl).to_string(),
            txtdt: String::from_utf8_lossy(txtdt).to_string(),
            txtitl: String::from_utf8_lossy(txtitl).to_string(),
        },
    ))
}

fn parse_data_extension_segment_subheader(input: &[u8]) -> IResult<&[u8], DataExtensionSegmentSubheader> {
    let (input, (de, desid, desver, desoflw)) = tuple((
        tag("DE"),
        nom::bytes::complete::take(25usize),
        nom::bytes::complete::take(2usize),
        nom::bytes::complete::take(6usize),
    ))(input)?;

    Ok((
        input,
        DataExtensionSegmentSubheader {
            de: String::from_utf8_lossy(de).to_string(),
            desid: String::from_utf8_lossy(desid).to_string(),
            desver: String::from_utf8_lossy(desver).to_string(),
            desoflw: String::from_utf8_lossy(desoflw).to_string(),
        },
    ))
}

fn parse_reserved_extension_segment_subheader(input: &[u8]) -> IResult<&[u8], ReservedExtensionSegmentSubheader> {
    let (input, (re, resid, resver)) = tuple((
        tag("RE"),
        nom::bytes::complete::take(25usize),
        nom::bytes::complete::take(2usize),
    ))(input)?;

    Ok((
        input,
        ReservedExtensionSegmentSubheader {
            re: String::from_utf8_lossy(re).to_string(),
            resid: String::from_utf8_lossy(resid).to_string(),
            resver: String::from_utf8_lossy(resver).to_string(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    let (input, nitf_header) = parse_nitf_header(&data).expect("Failed to parse NITF header");
    println!("{:?}", nitf_header);

    let (input, image_segment_subheader) = parse_image_segment_subheader(input).expect("Failed to parse image segment subheader");
    println!("{:?}", image_segment_subheader);

    let (input, symbol_segment_subheader) = parse_symbol_segment_subheader(input).expect("Failed to parse symbol segment subheader");
    println!("{:?}", symbol_segment_subheader);

    let (input, text_segment_subheader) = parse_text_segment_subheader(input).expect("Failed to parse text segment subheader");
    println!("{:?