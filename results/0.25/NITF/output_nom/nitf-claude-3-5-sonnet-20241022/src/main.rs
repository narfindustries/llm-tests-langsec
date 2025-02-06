use nom::{
    bytes::complete::take,
    combinator::{map, map_res},
    multi::count,
    sequence::tuple,
    IResult,
    Parser,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NitfHeader {
    fhdr: String,
    fver: String,
    clevel: u8,
    stype: String,
    ostaid: String,
    fdt: String,
    ftitle: String,
    fsclas: char,
    fsclsy: String,
    fscode: String,
    fsctlh: String,
    fsrel: String,
    fsdctp: String,
    fsdcdt: String,
    fsdcxm: String,
    fsdg: String,
    fsdgdt: String,
    fscltx: String,
    fscatp: String,
    fscaut: String,
    fscrsn: String,
    fssrdt: String,
    fsctln: String,
    fscop: String,
    fscpys: String,
    encryp: u8,
    fbkgc: [u8; 3],
    oname: String,
    ophone: String,
    fl: u64,
    hl: u64,
    numi: u16,
}

#[derive(Debug)]
struct ImageSegment {
    im: String,
    iid1: String,
    idatim: String,
    tgtid: String,
    iid2: String,
    isclas: char,
    isclsy: String,
    iscode: String,
    isctlh: String,
    isrel: String,
    isdctp: String,
    isdcdt: String,
    isdcxm: String,
    isdg: String,
    isdgdt: String,
    iscltx: String,
    iscatp: String,
    iscaut: String,
    iscrsn: String,
    issrdt: String,
    isctln: String,
    encryp: u8,
    isorce: String,
    nrows: u32,
    ncols: u32,
    pvtype: String,
    irep: String,
    icat: String,
    abpp: u8,
    pjust: char,
    icords: Option<char>,
    igeolo: Option<String>,
    nicom: u16,
    ic: String,
    comrat: Option<String>,
    nbands: u16,
    xbands: Option<u16>,
}

fn parse_fixed_length_string(length: usize) -> impl Fn(&[u8]) -> IResult<&[u8], String> {
    move |input| {
        map(take(length), |bytes: &[u8]| {
            String::from_utf8_lossy(bytes).trim().to_string()
        })(input)
    }
}

fn parse_numeric(length: usize) -> impl Fn(&[u8]) -> IResult<&[u8], u64> {
    move |input| {
        map_res(
            map(take(length), |bytes: &[u8]| {
                String::from_utf8_lossy(bytes).trim().to_string()
            }),
            |s: String| s.parse::<u64>(),
        )(input)
    }
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, (
        fhdr,
        fver,
        clevel,
        stype,
        ostaid,
        fdt,
        ftitle,
        fsclas,
        fsclsy,
        fscode,
        fsctlh,
        fsrel,
        fsdctp,
        fsdcdt,
        fsdcxm,
        fsdg,
        fsdgdt,
        fscltx,
        fscatp,
        fscaut,
        fscrsn,
        fssrdt,
        fsctln,
        fscop,
        fscpys,
        encryp,
        fbkgc,
        oname,
        ophone,
        fl,
        hl,
        numi,
    )) = tuple((
        parse_fixed_length_string(4),
        parse_fixed_length_string(5),
        map_res(take(2), |b| std::str::from_utf8(b).unwrap().parse::<u8>()),
        parse_fixed_length_string(4),
        parse_fixed_length_string(10),
        parse_fixed_length_string(14),
        parse_fixed_length_string(80),
        map(take(1), |b: &[u8]| b[0] as char),
        parse_fixed_length_string(2),
        parse_fixed_length_string(11),
        parse_fixed_length_string(2),
        parse_fixed_length_string(20),
        parse_fixed_length_string(2),
        parse_fixed_length_string(8),
        parse_fixed_length_string(4),
        parse_fixed_length_string(1),
        parse_fixed_length_string(8),
        parse_fixed_length_string(43),
        parse_fixed_length_string(1),
        parse_fixed_length_string(40),
        parse_fixed_length_string(1),
        parse_fixed_length_string(8),
        parse_fixed_length_string(15),
        parse_fixed_length_string(5),
        parse_fixed_length_string(5),
        map_res(take(1), |b| std::str::from_utf8(b).unwrap().parse::<u8>()),
        count(map_res(take(1), |b: &[u8]| Ok::<_, nom::error::Error<&[u8]>>(b[0])), 3),
        parse_fixed_length_string(24),
        parse_fixed_length_string(18),
        parse_numeric(12),
        parse_numeric(6),
        map_res(take(3), |b| std::str::from_utf8(b).unwrap().parse::<u16>()),
    ))(input)?;

    Ok((input, NitfHeader {
        fhdr,
        fver,
        clevel,
        stype,
        ostaid,
        fdt,
        ftitle,
        fsclas,
        fsclsy,
        fscode,
        fsctlh,
        fsrel,
        fsdctp,
        fsdcdt,
        fsdcxm,
        fsdg,
        fsdgdt,
        fscltx,
        fscatp,
        fscaut,
        fscrsn,
        fssrdt,
        fsctln,
        fscop,
        fscpys,
        encryp,
        fbkgc: [fbkgc[0], fbkgc[1], fbkgc[2]],
        oname,
        ophone,
        fl,
        hl,
        numi,
    }))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, (
        im,
        iid1,
        idatim,
        tgtid,
        iid2,
        isclas,
        isclsy,
        iscode,
        isctlh,
        isrel,
        isdctp,
        isdcdt,
        isdcxm,
        isdg,
        isdgdt,
        iscltx,
        iscatp,
        iscaut,
        iscrsn,
        issrdt,
        isctln,
        encryp,
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
        parse_fixed_length_string(2),
        parse_fixed_length_string(10),
        parse_fixed_length_string(14),
        parse_fixed_length_string(17),
        parse_fixed_length_string(80),
        map(take(1), |b: &[u8]| b[0] as char),
        parse_fixed_length_string(2),
        parse_fixed_length_string(11),
        parse_fixed_length_string(2),
        parse_fixed_length_string(20),
        parse_fixed_length_string(2),
        parse_fixed_length_string(8),
        parse_fixed_length_string(4),
        parse_fixed_length_string(1),
        parse_fixed_length_string(8),
        parse_fixed_length_string(43),
        parse_fixed_length_string(1),
        parse_fixed_length_string(40),
        parse_fixed_length_string(1),
        parse_fixed_length_string(8),
        parse_fixed_length_string(15),
        map_res(take(1), |b| std::str::from_utf8(b).unwrap().parse::<u8>()),
        parse_fixed_length_string(42),
        map_res(take(8), |b| std::str::from_utf8(b).unwrap().parse::<u32>()),
        map_res(take(8), |b| std::str::from_utf8(b).unwrap().parse::<u32>()),
        parse_fixed_length_string(3),
        parse_fixed_length_string(8),
        parse_fixed_length_string(8),
        map_res(take(2), |b| std::str::from_utf8(b).unwrap().parse::<u8>()),
        map(take(1), |b: &[u8]| b[0] as char),
        map(take(1), |b: &[u8]| if b[0] == b' ' { None } else { Some(b[0] as char) }),
        map(take(60), |b: &[u8]| if b[0] == b' ' { None } else { Some(String::from_utf8_lossy(b).trim().to_string()) }),
        map_res(take(1), |b| std::str::from_utf8(b).unwrap().parse::<u16>()),
        parse_fixed_length_string(2),
        map(take(4), |b: &[u8]| if b[0] == b' ' { None } else { Some(String::from_utf8_lossy(b).trim().to_string()) }),
        map_res(take(1), |b| std::str::from_utf8(b).unwrap().parse::<u16>()),
        map(take(5), |b: &[u8]| if b[0] == b' ' { None } else { Some(std::str::from_utf8(b).unwrap().parse::<u16>().unwrap()) }),
    ))(input)?;

    Ok((input, ImageSegment {
        im,
        iid1,
        idatim,
        tgtid,
        iid2,
        isclas,
        isclsy,
        iscode,
        isctlh,
        isrel,
        isdctp,
        isdcdt,
        isdcxm,
        isdg,
        isdgdt,
        iscltx,
        iscatp,
        iscaut,
        iscrsn,
        issrdt,
        isctln,
        encryp,
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
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <nitf-file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf_header(&buffer) {
        Ok((remaining, header)) => {
            println!("NITF Header: {:?}", header);
            
            if header.numi > 0 {
                let mut current_remaining = remaining;
                for i in 0..header.numi {
                    match parse_image_segment(current_remaining) {
                        Ok((new_remaining, image_segment)) => {
                            println!("Image Segment {}: {:?}", i + 1, image_segment);
                            current_remaining = new_remaining;
                        }
                        Err(e) => eprintln!("Error parsing image segment {}: {:?}", i + 1, e),
                    }
                }
            }
        }
        Err(e) => eprintln!("Error parsing NITF header: {:?}", e),
    }

    Ok(())
}