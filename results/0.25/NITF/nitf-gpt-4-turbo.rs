use nom::{
    bytes::complete::take,
    combinator::{map_res, all_consuming},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct FileHeader {
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
    fsdgor: String,
    fscaut: String,
    fsctln: String,
    fscop: String,
    fscpys: String,
    encrypt: String,
    fbkgc: String,
    oname: String,
    ophone: String,
}

fn parse_fixed_length(input: &[u8], length: usize) -> IResult<&[u8], String> {
    map_res(take(length), std::str::from_utf8)(input).map(|(next_input, result)| (next_input, result.to_string()))
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let parser = tuple((
        parse_fixed_length(9),
        parse_fixed_length(2),
        parse_fixed_length(2),
        parse_fixed_length(4),
        parse_fixed_length(10),
        parse_fixed_length(14),
        parse_fixed_length(80),
        parse_fixed_length(1),
        parse_fixed_length(2),
        parse_fixed_length(11),
        parse_fixed_length(2),
        parse_fixed_length(20),
        parse_fixed_length(2),
        parse_fixed_length(8),
        parse_fixed_length(4),
        parse_fixed_length(1),
        parse_fixed_length(40),
        parse_fixed_length(15),
        parse_fixed_length(5),
        parse_fixed_length(5),
        parse_fixed_length(1),
        parse_fixed_length(3),
        parse_fixed_length(24),
        parse_fixed_length(18),
    ));
    let (input, (fhdr, fver, clevel, stype, osta_id, fdt, ftitle, fsclas, fsclsy, fscode, fsctlh, fsrel, fsdctp, fsdcdt, fsdcxm, fsdgor, fscaut, fsctln, fscop, fscpys, encrypt, fbkgc, oname, ophone)) = all_consuming(parser)(input)?;

    Ok((input, FileHeader {
        fhdr, fver, clevel, stype, osta_id, fdt, ftitle, fsclas, fsclsy, fscode, fsctlh, fsrel, fsdctp, fsdcdt, fsdcxm, fsdgor, fscaut, fsctln, fscop, fscpys, encrypt, fbkgc, oname, ophone,
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_file_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => println!("Failed to parse NITF file header: {:?}", e),
    }

    Ok(())
}