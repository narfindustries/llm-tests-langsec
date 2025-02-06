use nom::{
    bytes::complete::take,
    combinator::map_res,
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

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
    fsclsy: String,
    fscode: String,
    fsctlh: String,
    fsrel: String,
    fsdctp: String,
    fsdcdt: String,
    fsdcxm: String,
    fsorgn: String,
    fscaut: String,
    fsctln: String,
    fscop: String,
    fscpys: String,
    encryp: String,
    fbkgc: String,
    oname: String,
    ophone: String,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    map_res(
        tuple((
            take(9usize), // FHDR
            take(5usize), // FVER
            take(2usize), // CLEVEL
            take(4usize), // STYPE
            take(10usize), // OSTAID
            take(14usize), // FDT
            take(80usize), // FTITLE
            take(1usize), // FSCLAS
            take(2usize), // FSCLSY
            take(11usize), // FSCODE
            take(2usize), // FSCTLH
            take(20usize), // FSREL
            take(2usize), // FSDCTP
            take(8usize), // FSDCDT
            take(4usize), // FSDCXM
            take(10usize), // FSORGN
            take(20usize), // FSCAUT
            take(15usize), // FSCTLN
            take(5usize), // FSCOP
            take(5usize), // FSCPYS
            take(1usize), // ENCRYP
            take(3usize), // FBKGC
            take(24usize), // ONAME
            take(18usize), // OPHONE
        )),
        |(
            fhdr, fver, clevel, stype, osta_id, fdt, ftitle, fsclas, fsclsy, fscode, fsctlh, fsrel,
            fsdctp, fsdcdt, fsdcxm, fsorgn, fscaut, fsctln, fscop, fscpys, encryp, fbkgc, oname,
            ophone,
        )| {
            Ok(NITFHeader {
                fhdr: String::from_utf8(fhdr.to_vec())?,
                fver: String::from_utf8(fver.to_vec())?,
                clevel: String::from_utf8(clevel.to_vec())?,
                stype: String::from_utf8(stype.to_vec())?,
                osta_id: String::from_utf8(osta_id.to_vec())?,
                fdt: String::from_utf8(fdt.to_vec())?,
                ftitle: String::from_utf8(ftitle.to_vec())?,
                fsclas: String::from_utf8(fsclas.to_vec())?,
                fsclsy: String::from_utf8(fsclsy.to_vec())?,
                fscode: String::from_utf8(fscode.to_vec())?,
                fsctlh: String::from_utf8(fsctlh.to_vec())?,
                fsrel: String::from_utf8(fsrel.to_vec())?,
                fsdctp: String::from_utf8(fsdctp.to_vec())?,
                fsdcdt: String::from_utf8(fsdcdt.to_vec())?,
                fsdcxm: String::from_utf8(fsdcxm.to_vec())?,
                fsorgn: String::from_utf8(fsorgn.to_vec())?,
                fscaut: String::from_utf8(fscaut.to_vec())?,
                fsctln: String::from_utf8(fsctln.to_vec())?,
                fscop: String::from_utf8(fscop.to_vec())?,
                fscpys: String::from_utf8(fscpys.to_vec())?,
                encryp: String::from_utf8(encryp.to_vec())?,
                fbkgc: String::from_utf8(fbkgc.to_vec())?,
                oname: String::from_utf8(oname.to_vec())?,
                ophone: String::from_utf8(ophone.to_vec())?,
            })
        },
    )(input)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Please provide a file path",
        ));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf_header(&buffer) {
        Ok((_, header)) => {
            println!("{:#?}", header);
        }
        Err(e) => {
            println!("Failed to parse NITF header: {:?}", e);
        }
    }

    Ok(())
}