use nom::{
    bytes::complete::{tag, take},
    number::complete::le_u32,
    IResult,
    error::{Error, ErrorKind},
};
use std::env;
use std::fs::File;
use std::io::Read;

fn gzip_header(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag([0x1f, 0x8b])(input)?;
    let (input, cm) = take(1usize)(input)?;
    if cm[0] != 8 {
        return Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)));
    }
    let (input, flg) = take(1usize)(input)?;
    let (input, mtime) = le_u32(input)?;
    let (input, xfl) = take(1usize)(input)?;
    let (input, os) = take(1usize)(input)?;

    if (flg[0] & 0b00000100) != 0 {
        // FEXTRA
        todo!("Handle extra fields");
    }

    if (flg[0] & 0b000000100) != 0 {
        // FNAME
        todo!("Handle filename");
    }

    if (flg[0] & 0b000000010) != 0 {
        // FCOMMENT
        todo!("Handle comment");
    }

    Ok((input, ()))
}

fn gzip_trailer(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _crc32) = le_u32(input)?;
    let (input, _isize) = le_u32(input)?;
    Ok((input, ()))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip_file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match gzip_header(&buffer) {
        Ok((remaining, _)) => {
            println!("Header parsed successfully!");
            match gzip_trailer(remaining) {
                Ok((_,_)) => println!("Trailer parsed successfully!"),
                Err(e) => println!("Error parsing GZIP trailer: {:?}", e),
            }
        }
        Err(e) => {
            println!("Error parsing GZIP header: {:?}", e);
        }
    }

    Ok(())
}
