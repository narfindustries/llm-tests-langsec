use nom::{
    bytes::complete::take,
    combinator::{map_res, opt},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::str;

#[derive(Debug)]
struct NITFHeader {
    fhdr: String,
    fver: String,
    clevel: String,
    stype: String,
    osta_id: String,
    fdt: String,
    ftitle: String,
    fsclas: String,
    fsclsy: Option<String>,
    fscode: Option<String>,
    fsctlh: Option<String>,
    fsrel: Option<String>,
    fsdctp: Option<String>,
    fsdcdt: Option<String>,
    fsdcxm: Option<String>,
    fsorgn: Option<String>,
    fscaut: Option<String>,
    fsctln: Option<String>,
    fsi: Option<String>,
    fcop: Option<String>,
    fscpys: Option<String>,
    encryp: String,
    fbkgc: Option<String>,
    oname: Option<String>,
    ophone: Option<String>,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    map_res(
        tuple((
            take(9usize),
            take(5usize),
            take(2usize),
            take(4usize),
            take(10usize),
            take(14usize),
            take(80usize),
            take(1usize),
            opt(take(2usize)),
            opt(take(11usize)),
            opt(take(2usize)),
            opt(take(20usize)),
            opt(take(2usize)),
            opt(take(8usize)),
            opt(take(4usize)),
            opt(take(8usize)),
            opt(take(40usize)),
            opt(take(20usize)),
            opt(take(15usize)),
            opt(take(1usize)),
            opt(take(5usize)),
            take(1usize),
            opt(take(3usize)),
            opt(take(24usize)),
            opt(take(18usize)),
        )),
        |(
            fhdr,
            fver,
            clevel,
            stype,
            osta_id,
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
            fsorgn,
            fscaut,
            fsctln,
            fsi,
            fcop,
            fscpys,
            encryp,
            fbkgc,
            oname,
            ophone,
        )| {
            Ok(NITFHeader {
                fhdr: str::from_utf8(fhdr)?.to_string(),
                fver: str::from_utf8(fver)?.to_string(),
                clevel: str::from_utf8(clevel)?.to_string(),
                stype: str::from_utf8(stype)?.to_string(),
                osta_id: str::from_utf8(osta_id)?.to_string(),
                fdt: str::from_utf8(fdt)?.to_string(),
                ftitle: str::from_utf8(ftitle)?.to_string(),
                fsclas: str::from_utf8(fsclas)?.to_string(),
                fsclsy: fsclsy.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                fscode: fscode.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                fsctlh: fsctlh.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                fsrel: fsrel.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                fsdctp: fsdctp.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                fsdcdt: fsdcdt.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                fsdcxm: fsdcxm.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                fsorgn: fsorgn.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                fscaut: fscaut.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                fsctln: fsctln.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                fsi: fsi.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                fcop: fcop.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                fscpys: fscpys.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                encryp: str::from_utf8(encryp)?.to_string(),
                fbkgc: fbkgc.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                oname: oname.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
                ophone: ophone.map(|bytes| str::from_utf8(bytes).map(String::from)).transpose()?,
            })
        },
    )(input)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => println!("Failed to parse NITF header: {:?}", e),
    }

    Ok(())
}