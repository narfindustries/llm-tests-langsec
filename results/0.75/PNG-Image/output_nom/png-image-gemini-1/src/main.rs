use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, verify},
    error::ErrorKind,
    number::complete::be_u32,
    sequence::{tuple, preceded},
    IResult,
};

#[derive(Debug)]
struct PngHeader {
    signature: [u8; 8],
    ihdr: IHDR,
    idat: Vec<u8>,
    iend: [u8; 4],
}


#[derive(Debug)]
struct IHDR {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

fn png_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    map(take(8usize), |bytes: &[u8]| {
        let mut arr = [0; 8];
        arr.copy_from_slice(bytes);
        arr
    })(input)
}

fn chunk(input: &[u8]) -> IResult<&[u8], (u32, &[u8], [u8; 4])> {
    let (input, length) = be_u32(input)?;
    let (input, type_bytes) = take(4usize)(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = take(4usize)(input)?;
    let mut crc_arr = [0;4];
    crc_arr.copy_from_slice(crc);
    Ok((input, (length, data, crc_arr)))
}

fn ihdr(input: &[u8]) -> IResult<&[u8], IHDR> {
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) = 
        preceded(tag(b"IHDR"), tuple((be_u32, be_u32, map(take(1usize), |b| b[0]), map(take(1usize), |b| b[0]), map(take(1usize), |b| b[0]), map(take(1usize), |b| b[0]), map(take(1usize), |b| b[0]))))(input)?;
    Ok((input, IHDR { width, height, bit_depth, color_type, compression_method, filter_method, interlace_method }))
}

fn idat(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, (length, data, _crc)) = preceded(tag(b"IDAT"), chunk)(input)?;
    Ok((input, data.to_vec()))
}


fn iend(input: &[u8]) -> IResult<&[u8], [u8; 4]> {
    map(preceded(tag(b"IEND"), take(4usize)), |bytes: &[u8]| {
        let mut arr = [0; 4];
        arr.copy_from_slice(bytes);
        arr
    })(input)
}

fn png(input: &[u8]) -> IResult<&[u8], PngHeader> {
    let (input, signature) = png_signature(input)?;
    let (input, ihdr) = ihdr(input)?;
    let (input, idat) = idat(input)?;
    let (input, iend) = iend(input)?;
    Ok((input, PngHeader { signature, ihdr, idat, iend }))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <filename>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    match png(&buffer) {
        Ok((_, header)) => println!("PNG Header: {:?}", header),
        Err(e) => println!("Error parsing PNG: {:?}", e),
    }
}
