use std::env;
use std::fs;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    number::complete::be_u32,
    sequence::tuple,
    IResult,
};
use flate2::read::GzDecoder;

fn gzip_header(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(b"\x1f\x8b")(input)?;
    let (input, compression_method) = be_u32(input)?;
    if compression_method != 8 {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Custom(1))));
    }
    let (input, flags) = be_u32(input)?;
    let (input, mtime) = be_u32(input)?;
    let (input, extra_flags) = if (flags & 0b00000001) != 0 {
        let (input, len) = be_u32(input)?;
        let (input, extra) = take(len as usize)(input)?;
        (input, extra)
    } else {
        (input, &[][..])
    };
    let (input, os) = be_u32(input)?;

    Ok((input, ()))
}

fn gzip_footer(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = take(8usize)(input)?;
    Ok((input, ()))
}

fn gzip_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, data) = take(input.len())(input)?;
    Ok((input, data.to_vec()))
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
            let mut decompressed = Vec::new();
            decoder.read_to_end(&mut decompressed).unwrap();
            println!("Decompressed data: {:?}", decompressed);

        },
        Err(e) => {
            eprintln!("Error parsing gzip file: {:?}", e);
            std::process::exit(1);
        }
    }
}
