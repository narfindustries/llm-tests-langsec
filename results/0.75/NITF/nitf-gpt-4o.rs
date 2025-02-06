extern crate nom;

use nom::{
    bytes::complete::take,
    combinator::map_res,
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NITFFileHeader {
    fhdr: String,
    clevel: String,
    stype: String,
    ostaid: String,
    fdt: String,
    ftitle: String,
    fsclas: String,
    fscode: String,
    fsctlh: String,
    fsrel: String,
    fsdctp: String,
    fsdcdt: String,
    fsdcdtx: String,
    fsdcxm: String,
    fscatp: String,
    fscaut: String,
    fscrsn: String,
    fssrdt: String,
    encryp: String,
    fbkgc: String,
    oname: String,
    ophone: String,
    fl: u32,
    hl: u32,
    numi: u32,
    numg: u32,
    numt: u32,
    numdes: u32,
    numres: u32,
    udhdl: String,
    xhdl: String,
}

fn parse_string<'a>(length: usize) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], String> {
    move |input: &[u8]| map_res(take(length), |s: &[u8]| String::from_utf8(s.to_vec()))(input)
}

fn parse_u32<'a>(length: usize) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], u32> {
    move |input: &[u8]| map_res(map_res(take(length), |s: &[u8]| String::from_utf8(s.to_vec())), |s| {
        s.trim().parse::<u32>()
    })(input)
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], NITFFileHeader> {
    let (input, (
        fhdr, clevel, stype, ostaid, fdt, ftitle, fsclas, fscode, fsctlh, fsrel, fsdctp, fsdcdt, fsdcdtx, fsdcxm, fscatp, fscaut, fscrsn, fssrdt, encryp, fbkgc, oname, ophone, fl, hl, numi, numg, numt, numdes, numres, udhdl, xhdl
    )) = tuple((
        parse_string(9),  // FHDR
        parse_string(2),  // CLEVEL
        parse_string(4),  // STYPE
        parse_string(10), // OSTAID
        parse_string(14), // FDT
        parse_string(80), // FTITLE
        parse_string(1),  // FSCLAS
        parse_string(40), // FSCODE
        parse_string(40), // FSCTLH
        parse_string(40), // FSREL
        parse_string(2),  // FSDCTP
        parse_string(8),  // FSDCDT
        parse_string(4),  // FSDCDTX
        parse_string(1),  // FSDCXM
        parse_string(1),  // FSCATP
        parse_string(20), // FSCAUT
        parse_string(20), // FSCRSN
        parse_string(8),  // FSSRDT
        parse_string(1),  // ENCRYP
        parse_string(3),  // FBKGC
        parse_string(27), // ONAME
        parse_string(18), // OPHONE
        parse_u32(12),    // FL
        parse_u32(6),     // HL
        parse_u32(3),     // NUMI
        parse_u32(3),     // NUMG
        parse_u32(3),     // NUMT
        parse_u32(3),     // NUMDES
        parse_u32(3),     // NUMRES
        parse_string(5),  // UDHDL
        parse_string(3),  // XHDL
    ))(input)?;

    Ok((input, NITFFileHeader {
        fhdr,
        clevel,
        stype,
        ostaid,
        fdt,
        ftitle,
        fsclas,
        fscode,
        fsctlh,
        fsrel,
        fsdctp,
        fsdcdt,
        fsdcdtx,
        fsdcxm,
        fscatp,
        fscaut,
        fscrsn,
        fssrdt,
        encryp,
        fbkgc,
        oname,
        ophone,
        fl,
        hl,
        numi,
        numg,
        numt,
        numdes,
        numres,
        udhdl,
        xhdl,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    let mut file = File::open(file_path).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    match parse_file_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => eprintln!("Failed to parse NITF file: {:?}", e),
    }
}