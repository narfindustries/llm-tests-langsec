use std::env;
use std::fs;
use std::io::Read;
use nom::{
    bytes::complete::take,
    combinator::map,
    error::ErrorKind,
    multi::count,
    number::complete::be_u32,
    IResult,
};
use flate2::read::GzDecoder;


fn gzip_header(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = take(2usize)(input)?; // ID1, ID2
    let (input, cm) = be_u32(input)?;
    let (input, flg) = be_u32(input)?;
    let (input, mtime) = be_u32(input)?;
    let (input, xfl) = take(1usize)(input)?;
    let (input, os) = take(1usize)(input)?;

    //Basic checks, more robust checks could be implemented.
    if cm != 8 { return Err(nom::Err::Error(nom::error::Error { input, code: ErrorKind::Custom(1) })); }

    Ok((input, ()))
}

fn gzip_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut decoder = GzDecoder::new(input);
    let mut data = Vec::new();
    decoder.read_to_end(&mut data).unwrap();
    Ok(("", data))
}


fn gzip_trailer(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = take(8usize)(input)?; // CRC32 and ISIZE
    Ok((input, ()))
}


fn parse_gzip(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, ()) = gzip_header(input)?;
    let (input, data) = gzip_data(input)?;
    let (input, ()) = gzip_trailer(input)?;
    Ok((input, data))
}



fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: gzip_parser <filename>");
        return;
    }

    let filename = &args[1];
    let contents = fs::read(filename).expect("Something went wrong reading the file");

    match parse_gzip(&contents) {
        Ok((_, data)) => {
            println!("Decompressed data: {:?}", data);
        }
        Err(e) => {
            println!("Error parsing gzip file: {:?}", e);
        }
    }
}
