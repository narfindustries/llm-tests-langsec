use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::count,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct NITF {
    header: NITFHeader,
    image_segments: Vec<ImageSegment>,
}

#[derive(Debug)]
struct NITFHeader {
    file_profile_name: [u8; 4],
    file_version: [u8; 5],
    complexity_level: [u8; 2],
    standard_type: [u8; 4],
    originating_station_id: [u8; 10],
    file_date_time: [u8; 14],
    file_title: [u8; 80],
    file_security_classification: u8,
    encryption: [u8; 1],
    message_copy: [u8; 1],
    message_handling: [u8; 2],
    precedence: [u8; 1],
    extended_header_length: u32,
    file_length: u32,
    header_length: u32,
    num_image_segments: u16,
}

#[derive(Debug)]
struct ImageSegment {
    im: [u8; 2],
    iid1: [u8; 10],
    idatim: [u8; 14],
    tgtid: [u8; 17],
    iid2: [u8; 80],
    security_classification: u8,
    encrypted: [u8; 1],
    isorce: [u8; 42],
    nrows: u32,
    ncols: u32,
    pvtype: [u8; 3],
    irep: [u8; 8],
    icat: [u8; 8],
    abpp: u8,
    pjust: [u8; 1],
    icords: Option<[u8; 1]>,
    igeolo: Option<[u8; 60]>,
    nicom: u8,
    ic: [u8; 2],
    comrat: Option<[u8; 4]>,
    nbands: u8,
    xbands: Option<u16>,
    image_bands: Vec<ImageBand>,
    isync: u8,
    imode: [u8; 1],
    nbpr: u8,
    nbpc: u8,
    nppbh: u16,
    nppbv: u16,
    nbpp: u8,
    idlvl: u16,
    ialvl: u16,
    iloc: u16,
    imag: [u8; 4],
}

#[derive(Debug)]
struct ImageBand {
    irepband: [u8; 2],
    isubcat: [u8; 6],
    ifc: [u8; 1],
    imflt: [u8; 3],
    nluts: u8,
    nelut: Option<u16>,
}

fn parse_fixed_length_string(input: &[u8], length: usize) -> IResult<&[u8], Vec<u8>> {
    map(take(length), |bytes: &[u8]| bytes.to_vec())(input)
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, (
        file_profile_name,
        file_version,
        complexity_level,
        standard_type,
        originating_station_id,
        file_date_time,
        file_title,
        file_security_classification,
        encryption,
        message_copy,
        message_handling,
        precedence,
        extended_header_length,
        file_length,
        header_length,
        num_image_segments,
    )) = tuple((
        take(4usize),
        take(5usize),
        take(2usize),
        take(4usize),
        take(10usize),
        take(14usize),
        take(80usize),
        be_u8,
        take(1usize),
        take(1usize),
        take(2usize),
        take(1usize),
        be_u32,
        be_u32,
        be_u32,
        be_u16,
    ))(input)?;

    Ok((input, NITFHeader {
        file_profile_name: file_profile_name.try_into().unwrap(),
        file_version: file_version.try_into().unwrap(),
        complexity_level: complexity_level.try_into().unwrap(),
        standard_type: standard_type.try_into().unwrap(),
        originating_station_id: originating_station_id.try_into().unwrap(),
        file_date_time: file_date_time.try_into().unwrap(),
        file_title: file_title.try_into().unwrap(),
        file_security_classification,
        encryption: encryption.try_into().unwrap(),
        message_copy: message_copy.try_into().unwrap(),
        message_handling: message_handling.try_into().unwrap(),
        precedence: precedence.try_into().unwrap(),
        extended_header_length,
        file_length,
        header_length,
        num_image_segments,
    }))
}

fn parse_image_band(input: &[u8]) -> IResult<&[u8], ImageBand> {
    let (input, (
        irepband,
        isubcat,
        ifc,
        imflt,
        nluts,
        nelut,
    )) = tuple((
        take(2usize),
        take(6usize),
        take(1usize),
        take(3usize),
        be_u8,
        opt(be_u16),
    ))(input)?;

    Ok((input, ImageBand {
        irepband: irepband.try_into().unwrap(),
        isubcat: isubcat.try_into().unwrap(),
        ifc: ifc.try_into().unwrap(),
        imflt: imflt.try_into().unwrap(),
        nluts,
        nelut,
    }))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, (
        im,
        iid1,
        idatim,
        tgtid,
        iid2,
        security_classification,
        encrypted,
        isorce,
        nrows,
        ncols,
        pvtype,
        irep,
        icat,
        abpp,
        pjust,
        icords,
        igeolo,
        nicom,
        ic,
        comrat,
        nbands,
        xbands,
    )) = tuple((
        take(2usize),
        take(10usize),
        take(14usize),
        take(17usize),
        take(80usize),
        be_u8,
        take(1usize),
        take(42usize),
        be_u32,
        be_u32,
        take(3usize),
        take(8usize),
        take(8usize),
        be_u8,
        take(1usize),
        opt(take(1usize)),
        opt(take(60usize)),
        be_u8,
        take(2usize),
        opt(take(4usize)),
        be_u8,
        opt(be_u16),
    ))(input)?;

    let num_bands = xbands.unwrap_or(nbands as u16);
    let (input, image_bands) = count(parse_image_band, num_bands as usize)(input)?;

    let (input, (
        isync,
        imode,
        nbpr,
        nbpc,
        nppbh,
        nppbv,
        nbpp,
        idlvl,
        ialvl,
        iloc,
        imag,
    )) = tuple((
        be_u8,
        take(1usize),
        be_u8,
        be_u8,
        be_u16,
        be_u16,
        be_u8,
        be_u16,
        be_u16,
        be_u16,
        take(4usize),
    ))(input)?;

    Ok((input, ImageSegment {
        im: im.try_into().unwrap(),
        iid1: iid1.try_into().unwrap(),
        idatim: idatim.try_into().unwrap(),
        tgtid: tgtid.try_into().unwrap(),
        iid2: iid2.try_into().unwrap(),
        security_classification,
        encrypted: encrypted.try_into().unwrap(),
        isorce: isorce.try_into().unwrap(),
        nrows,
        ncols,
        pvtype: pvtype.try_into().unwrap(),
        irep: irep.try_into().unwrap(),
        icat: icat.try_into().unwrap(),
        abpp,
        pjust: pjust.try_into().unwrap(),
        icords: icords.map(|x| x.try_into().unwrap()),
        igeolo: igeolo.map(|x| x.try_into().unwrap()),
        nicom,
        ic: ic.try_into().unwrap(),
        comrat: comrat.map(|x| x.try_into().unwrap()),
        nbands,
        xbands,
        image_bands,
        isync,
        imode: imode.try_into().unwrap(),
        nbpr,
        nbpc,
        nppbh,
        nppbv,
        nbpp,
        idlvl,
        ialvl,
        iloc,
        imag: imag.try_into().unwrap(),
    }))
}

fn parse_nitf(input: &[u8]) -> IResult<&[u8], NITF> {
    let (input, header) = parse_nitf_header(input)?;
    let (input, image_segments) = count(parse_image_segment, header.num_image_segments as usize)(input)?;

    Ok((input, NITF {
        header,
        image_segments,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <nitf_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf(&buffer) {
        Ok((_, nitf)) => println!("{:#?}", nitf),
        Err(e) => eprintln!("Error parsing NITF: {:?}", e),
    }

    Ok(())
}