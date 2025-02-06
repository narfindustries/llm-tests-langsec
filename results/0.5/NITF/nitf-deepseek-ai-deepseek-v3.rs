use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    number::complete::{be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult,
};
use std::{
    fs::File,
    io::Read,
    env,
};

#[derive(Debug)]
struct NITFHeader {
    fhdr: String,
    clevel: String,
    stype: String,
    ostaid: String,
    fdt: String,
    ftitle: String,
    security: SecurityFields,
    encrypt: u8,
    fbgc: (u8, u8, u8),
    oname: String,
    ophone: String,
    fl: u64,
    hl: u32,
    numi: u16,
    nums: u16,
    numl: u16,
    numt: u16,
    numdes: u16,
    numres: u16,
}

#[derive(Debug)]
struct SecurityFields {
    clevel: String,
    oflag: String,
    fdwng: String,
    fdevt: String,
    fscop: String,
    fscpys: String,
    fsrel: String,
    fsctlh: String,
}

#[derive(Debug)]
struct ImageSegmentHeader {
    im: String,
    security: SecurityFields,
    isorce: String,
    nrows: u32,
    ncols: u32,
    pvtype: String,
    irep: String,
    icat: String,
    abpp: u8,
    pjust: String,
    ic: String,
    comrat: String,
    nbands: u16,
    xbands: u16,
}

#[derive(Debug)]
struct SymbolSegmentHeader {
    sy: String,
    security: SecurityFields,
    sxrow: u32,
    sxcol: u32,
    snum: u16,
    scolor: (u8, u8, u8),
}

#[derive(Debug)]
struct LabelSegmentHeader {
    lb: String,
    security: SecurityFields,
    lrow: u32,
    lcol: u32,
    lcolor: (u8, u8, u8),
}

#[derive(Debug)]
struct TextSegmentHeader {
    tx: String,
    security: SecurityFields,
    txtalvl: u16,
    txtdt: String,
    txtfmt: String,
}

#[derive(Debug)]
struct DataExtensionSegmentHeader {
    des: String,
    security: SecurityFields,
    desver: String,
    desoflw: String,
}

#[derive(Debug)]
struct ReservedExtensionSegmentHeader {
    re: String,
    security: SecurityFields,
    rever: String,
    reoflw: String,
}

#[derive(Debug)]
struct NITFFile {
    header: NITFHeader,
    image_segments: Vec<ImageSegmentHeader>,
    symbol_segments: Vec<SymbolSegmentHeader>,
    label_segments: Vec<LabelSegmentHeader>,
    text_segments: Vec<TextSegmentHeader>,
    data_extension_segments: Vec<DataExtensionSegmentHeader>,
    reserved_extension_segments: Vec<ReservedExtensionSegmentHeader>,
}

fn parse_security_fields(input: &[u8]) -> IResult<&[u8], SecurityFields> {
    let (input, (clevel, oflag, fdwng, fdevt, fscop, fscpys, fsrel, fsctlh)) = tuple((
        map_res(take(1usize), |v: &[u8]| String::from_utf8(v.to_vec())),
        map_res(take(1usize), |v: &[u8]| String::from_utf8(v.to_vec())),
        map_res(take(1usize), |v: &[u8]| String::from_utf8(v.to_vec())),
        map_res(take(20usize), |v: &[u8]| String::from_utf8(v.to_vec())),
        map_res(take(20usize), |v: &[u8]| String::from_utf8(v.to_vec())),
        map_res(take(20usize), |v: &[u8]| String::from_utf8(v.to_vec())),
        map_res(take(20usize), |v: &[u8]| String::from_utf8(v.to_vec())),
        map_res(take(20usize), |v: &[u8]| String::from_utf8(v.to_vec())),
    ))(input)?;
    Ok((input, SecurityFields {
        clevel,
        oflag,
        fdwng,
        fdevt,
        fscop,
        fscpys,
        fsrel,
        fsctlh,
    }))
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, (fhdr, clevel, stype, ostaid, fdt, ftitle)) = tuple((
        map_res(take(9usize), |v: &[u8]| String::from_utf8(v.to_vec())),
        map_res(take(2usize), |v: &[u8]| String::from_utf8(v.to_vec())),
        map_res(take(4usize), |v: &[u8]| String::from_utf8(v.to_vec())),
        map_res(take(10usize), |v: &[u8]| String::from_utf8(v.to_vec())),
        map_res(take(14usize), |v: &[u8]| String::from_utf8(v.to_vec())),
        map_res(take(80usize), |v: &[u8]| String::from_utf8(v.to_vec())),
    ))(input)?;
    let (input, security) = parse_security_fields(input)?;
    let (input, encrypt) = map_res(take(1usize), |v: &[u8]| Ok::<u8, &str>(v[0]))(input)?;
    let (input, (r, g, b)) = tuple((take(1usize), take(1usize), take(1usize)))(input)?;
    let (input, oname) = map_res(take(24usize), |v: &[u8]| String::from_utf8(v.to_vec()))(input)?;
    let (input, ophone) = map_res(take(18usize), |v: &[u8]| String::from_utf8(v.to_vec()))(input)?;
    let (input, fl) = be_u64(input)?;
    let (input, hl) = be_u32(input)?;
    let (input, numi) = be_u16(input)?;
    let (input, nums) = be_u16(input)?;
    let (input, numl) = be_u16(input)?;
    let (input, numt) = be_u16(input)?;
    let (input, numdes) = be_u16(input)?;
    let (input, numres) = be_u16(input)?;
    Ok((input, NITFHeader {
        fhdr,
        clevel,
        stype,
        ostaid,
        fdt,
        ftitle,
        security,
        encrypt,
        fbgc: (r[0], g[0], b[0]),
        oname,
        ophone,
        fl,
        hl,
        numi,
        nums,
        numl,
        numt,
        numdes,
        numres,
    }))
}

fn parse_image_segment_header(input: &[u8]) -> IResult<&[u8], ImageSegmentHeader> {
    let (input, im) = map_res(take(2usize), |v: &[u8]| String::from_utf8(v.to_vec()))(input)?;
    let (input, security) = parse_security_fields(input)?;
    let (input, isorce) = map_res(take(42usize), |v: &[u8]| String::from_utf8(v.to_vec()))(input)?;
    let (input, nrows) = be_u32(input)?;
    let (input, ncols) = be_u32(input)?;
    let (input, pvtype) = map_res(take(3usize), |v: &[u8]| String::from_utf8(v.to_vec()))(input)?;
    let (input, irep) = map_res(take(8usize), |v: &[u8]| String::from_utf8(v.to_vec()))(input)?;
    let (input, icat) = map_res(take(8usize), |v: &[u8]| String::from_utf8(v.to_vec()))(input)?;
    let (input, abpp) = map_res(take(1usize), |v: &[u8]| Ok::<u8, &str>(v[0]))(input)?;
    let (input, pjust) = map_res(take(1usize), |v: &[u8]| String::from_utf8(v.to_vec()))(input)?;
    let (input,