use nom::{
    bytes::complete::take,
    character::complete::char,
    combinator::{map_res, opt},
    multi::count,
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::str;

#[derive(Debug)]
struct FileHeader {
    fhdr: String,
    fver: String,
    clevel: String,
    stype: String,
    ostationid: String,
    fdt: String,
    ftitle: String,
    fsclas: String,
    fsclsys: String,
    fscode: String,
    fsctlh: String,
    fsrel: String,
    fscaut: String,
    fsctln: String,
    fsdwn: String,
    fsdevt: String,
    fsorig: String,
    fsocom: String,
    fssrdt: String,
    fsctim: String,
    fsprf: String,
    fsoprg: String,
    fsisdt: String,
    fsctl: String,
    fsisct: String,
    fsrelx: String,
    fsdctx: String,
    fsdcxm: String,
    fsdg: String,
    fsdgdt: String,
    fscltx: String,
    fscatp: String,
    fscautp: String,
    fsctlnp: String,
    fscrsn: String,
    fsctara: String,
    fssrct: String,
    fsctb: String,
    fstitl: String,
    fsvinel: String,
    fsclasxt: String,
    fscomp: String,
    fsctlng: String,
    fencryp: String,
    fstdid: String,
    fostaid: String,
    fditrd: String,
    fopro: String,
}

fn parse_string(input: &[u8], len: usize) -> IResult<&[u8], String> {
    map_res(take(len), str::from_utf8)(input).map(|(next_input, result)| {
        (next_input, result.trim().to_string())
    })
}

fn parse_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, (fhdr, fver, clevel, stype, ostationid, fdt, ftitle, fsclas,
         fsclsys, fscode, fsctlh, fsrel, fscaut, fsctln, fsdwn, fsdevt, fsorig,
         fsocom, fssrdt, fsctim, fsprf, fsoprg, fsisdt, fsctl, fsisct, fsrelx,
         fsdctx, fsdcxm, fsdg, fsdgdt, fscltx, fscatp, fscautp, fsctlnp, fscrsn,
         fsctara, fssrct, fsctb, fstitl, fsvinel, fsclasxt, fscomp, fsctlng,
         fencryp, fstdid, fostaid, fditrd, fopro)) =
        tuple((        
            parse_string(2),
            parse_string(5),
            parse_string(3),
            parse_string(4),
            parse_string(10),
            parse_string(14),
            parse_string(80),
            parse_string(1),
            parse_string(2),
            parse_string(11),
            parse_string(2),
            parse_string(20),
            parse_string(20),
            parse_string(6),
            parse_string(6),
            parse_string(40),
            parse_string(27),
            parse_string(8),
            parse_string(1),
            parse_string(40),
            parse_string(40),
            parse_string(1),
            parse_string(8),
            parse_string(15),
            parse_string(5),
            parse_string(40),
            parse_string(1),
            parse_string(1),
            parse_string(8),
            parse_string(1),
            parse_string(43),
            parse_string(1),
            parse_string(8),
            parse_string(15),
            parse_string(1),
            parse_string(3),
            parse_string(40),
            parse_string(40),
            parse_string(120),
            parse_string(6),
            parse_string(2),
            parse_string(1),
            parse_string(10),
            parse_string(14),
            parse_string(80),
            parse_string(5),
            
        ))(input)?;

    Ok((input, FileHeader {
        fhdr, fver, clevel, stype, ostationid, fdt, ftitle, fsclas, fsclsys, fscode, fsctlh, fsrel, fscaut, fsctln, fsdwn,
        fsdevt, fsorig, fsocom, fssrdt, fsctim, fsprf, fsoprg, fsisdt, fsctl, fsisct, fsrelx, fsdctx, fsdcxm, fsdg,
        fsdgdt, fscltx, fscatp, fscautp, fsctlnp, fscrsn, fsctara, fssrct, fsctb, fstitl, fsvinel, fsclasxt, fscomp,
        fsctlng, fencryp, fstdid, fostaid, fditrd, fopro
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No file specified"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_header(&buffer[..]) {
        Ok((_rest, header)) => {
            println!("{:#?}", header);
        },
        Err(e) => {
            println!("Failed to parse NITF file header: {:?}", e);
        }
    }

    Ok(())
}