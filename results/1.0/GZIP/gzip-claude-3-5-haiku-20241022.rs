use nom::{
    bytes::complete::{take_till, take_while},
    combinator::map,
    multi::many0,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;
use std::io::Read;

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: u8,
    mod_time: u32,
    extra_flags: u8,
    operating_system: u8,
    extra_fields: Option<Vec<ExtraField>>,
    original_filename: Option<String>,
    file_comment: Option<String>,
    header_crc16: Option<u16>,
}

#[derive(Debug)]
struct ExtraField {
    subfield_id: String,
    subfield_length: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    compressed_data: Vec<u8>,
    crc32: u32,
    input_size: u32,
}

fn parse_extra_field(input: &[u8]) -> IResult<&[u8], ExtraField> {
    let (input, (subfield_id, subfield_length)) = tuple((
        map(take_while(|b: u8| b.is_ascii()), |id: &[u8]| String::from_utf8_lossy(id).into_owned()),
        le_u16,
    ))(input)?;

    let (input, data) = take_while(|_| true)(input)?;
    let data = data[..subfield_length as usize].to_vec();

    Ok((input, ExtraField {
        subfield_id,
        subfield_length,
        data,
    }))
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, compression_method, flags, mod_time, extra_flags, operating_system)) = tuple((
        le_u8, le_u8, le_u8, le_u8, le_u32, le_u8, le_u8,
    ))(input)?;

    let (input, extra_fields) = if flags & 0b100 != 0 {
        let (input, fields) = many0(parse_extra_field)(input)?;
        (input, Some(fields))
    } else {
        (input, None)
    };

    let (input, original_filename) = if flags & 0b1000 != 0 {
        let (input, filename) = take_till(|b| b == 0)(input)?;
        (input, Some(String::from_utf8_lossy(filename).into_owned()))
    } else {
        (input, None)
    };

    let (input, file_comment) = if flags & 0b10000 != 0 {
        let (input, comment) = take_till(|b| b == 0)(input)?;
        (input, Some(String::from_utf8_lossy(comment).into_owned()))
    } else {
        (input, None)
    };

    let (input, header_crc16) = if flags & 0b10 != 0 {
        let (input, crc) = le_u16(input)?;
        (input, Some(crc))
    } else {
        (input, None)
    };

    Ok((input, GzipHeader {
        id1,
        id2,
        compression_method,
        flags,
        mod_time,
        extra_flags,
        operating_system,
        extra_fields,
        original_filename,
        file_comment,
        header_crc16,
    }))
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;

    let (input, compressed_data) = take_while(|_| true)(input)?;
    let compressed_data = compressed_data.to_vec();

    let (input, crc32) = le_u32(input)?;
    let (input, input_size) = le_u32(input)?;

    Ok((input, GzipFile {
        header,
        compressed_data,
        crc32,
        input_size,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = fs::File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_file(&buffer) {
        Ok((_, gzip_file)) => {
            println!("{:#?}", gzip_file);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing GZIP file: {:?}", e);
            std::process::exit(1);
        }
    }
}