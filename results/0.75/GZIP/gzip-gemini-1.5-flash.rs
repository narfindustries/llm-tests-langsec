use std::env;
use std::fs;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    number::complete::le_u32,
    sequence::{tuple, preceded},
    IResult,
};
use flate2::read::GzDecoder;


fn gzip_header(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(b"\x1f\x8b")(input)?;
    let (input, compression_method) = le_u32(input)?;
    if compression_method != 8 {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Custom(1))));
    }
    let (input, flags) = le_u32(input)?;
    let (input, mtime) = le_u32(input)?;
    let (input, xflags) = opt(le_u32)(input)?;
    let (input, os) = le_u32(input)?;

    //Process optional fields based on flags

    Ok((input, ()))

}

fn gzip_footer(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(b"\x00")(input)?;
    Ok((input, ()))
}

fn gzip_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(take(input.len()), |x: &[u8]| x.to_vec())(input)
}



fn parse_gzip(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = gzip_header(input)?;
    let (input, data) = gzip_data(input)?;
    let (input, _) = gzip_footer(input)?;
    Ok((input, data))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let contents = fs::read(filename).expect("Failed to read file");

    match parse_gzip(&contents) {
        Ok((_, data)) => {
            let mut decoder = GzDecoder::new(&data[..]);
            let mut uncompressed_data = Vec::new();
            decoder.read_to_end(&mut uncompressed_data).unwrap();
            println!("Uncompressed data: {:?}", uncompressed_data);


        }
        Err(e) => {
            eprintln!("Error parsing gzip file: {:?}", e);
            std::process::exit(1);
        }
    }
}
