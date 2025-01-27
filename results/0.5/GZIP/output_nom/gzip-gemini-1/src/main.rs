use std::env;
use std::fs;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    number::complete::be_u32,
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
    let (input, extra_flags) = be_u32(input)?;
    let (input, os) = be_u32(input)?;

    if flags & 0b00000001 != 0 {
        let (input, extra_field_len) = be_u32(input)?;
        let (input, _) = take(extra_field_len as usize)(input)?;
    }
    if flags & 0b00000010 != 0 {
        let (input, filename_len) = be_u32(input)?;
        let (input, _) = take(filename_len as usize)(input)?;
    }
    if flags & 0b00000100 != 0 {
        let (input, comment_len) = be_u32(input)?;
        let (input, _) = take(comment_len as usize)(input)?;
    }
    if flags & 0b00001000 != 0 {
        let (input, _) = be_u32(input)?; // Header CRC32
    }
    Ok((input, ()))
}

fn gzip_footer(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = take(8usize)(input)?; // CRC32 and ISIZE
    Ok((input, ()))
}


fn parse_gzip(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = gzip_header(input)?;
    let (input, compressed_data) = take_until_gzip_footer(input)?;
    let (input, _) = gzip_footer(input)?;
    Ok((input, compressed_data.to_vec()))
}

fn take_until_gzip_footer(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let mut i = 0;
    while i < input.len() - 7 {
        if input[i..i+8].ends_with(&[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) {
            return Ok((&input[i+8..], &input[..i]));
        }
        i += 1;
    }
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Custom(2))))

}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return;
    }

    let filename = &args[1];
    let contents = fs::read(filename).expect("Something went wrong reading the file");

    match parse_gzip(&contents) {
        Ok((_, compressed_data)) => {
            let mut decoder = GzDecoder::new(&compressed_data[..]);
            let mut decompressed = Vec::new();
            decoder.read_to_end(&mut decompressed).unwrap();
            println!("Decompressed data: {:?}", decompressed);
        }
        Err(e) => {
            eprintln!("Error parsing GZIP: {:?}", e);
        }
    }
}
