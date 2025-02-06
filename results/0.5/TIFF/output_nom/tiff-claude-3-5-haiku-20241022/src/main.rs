use nom::{
    bytes::complete::take,
    multi::count,
    number::complete::{le_u16, le_u32, be_u16, be_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone)]
enum ByteOrder {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
struct TiffHeader {
    byte_order: ByteOrder,
    magic_number: u16,
    first_ifd_offset: u32,
}

#[derive(Debug)]
enum TagType {
    Byte,
    Ascii,
    Short,
    Long,
    Rational,
    SByte,
    Undefined,
    SShort,
    SLong,
    SRational,
    Float,
    Double,
}

#[derive(Debug)]
struct IFDTag {
    tag_id: u16,
    tag_type: TagType,
    count: u32,
    value_or_offset: u32,
}

#[derive(Debug)]
struct ImageFileDirectory {
    num_tags: u16,
    tags: Vec<IFDTag>,
    next_ifd_offset: u32,
}

fn parse_byte_order<'a>(input: &'a [u8]) -> IResult<&'a [u8], ByteOrder> {
    let (input, order_bytes) = take(2usize)(input)?;
    let byte_order = match order_bytes {
        b"II" => ByteOrder::LittleEndian,
        b"MM" => ByteOrder::BigEndian,
        _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    };
    Ok((input, byte_order))
}

fn parse_tiff_header<'a>(input: &'a [u8]) -> IResult<&'a [u8], TiffHeader> {
    let (input, byte_order) = parse_byte_order(input)?;
    let (input, magic_number) = match byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?,
    };
    let (input, first_ifd_offset) = match byte_order {
        ByteOrder::LittleEndian => le_u32(input)?,
        ByteOrder::BigEndian => be_u32(input)?,
    };

    Ok((input, TiffHeader {
        byte_order,
        magic_number,
        first_ifd_offset,
    }))
}

fn parse_tag_type(tag_type_num: u16) -> TagType {
    match tag_type_num {
        1 => TagType::Byte,
        2 => TagType::Ascii,
        3 => TagType::Short,
        4 => TagType::Long,
        5 => TagType::Rational,
        6 => TagType::SByte,
        7 => TagType::Undefined,
        8 => TagType::SShort,
        9 => TagType::SLong,
        10 => TagType::SRational,
        11 => TagType::Float,
        12 => TagType::Double,
        _ => panic!("Unknown tag type"),
    }
}

fn parse_ifd_tag<'a>(input: &'a [u8], byte_order: &ByteOrder) -> IResult<&'a [u8], IFDTag> {
    let (input, tag_id) = match byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?,
    };
    let (input, tag_type_num) = match byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?,
    };
    let (input, count) = match byte_order {
        ByteOrder::LittleEndian => le_u32(input)?,
        ByteOrder::BigEndian => be_u32(input)?,
    };
    let (input, value_or_offset) = match byte_order {
        ByteOrder::LittleEndian => le_u32(input)?,
        ByteOrder::BigEndian => be_u32(input)?,
    };

    Ok((input, IFDTag {
        tag_id,
        tag_type: parse_tag_type(tag_type_num),
        count,
        value_or_offset,
    }))
}

fn parse_image_file_directory<'a>(input: &'a [u8], byte_order: &ByteOrder) -> IResult<&'a [u8], ImageFileDirectory> {
    let (input, num_tags) = match byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?,
    };

    let (input, tags) = count(
        |i| parse_ifd_tag(i, byte_order),
        num_tags as usize
    )(input)?;

    let (input, next_ifd_offset) = match byte_order {
        ByteOrder::LittleEndian => le_u32(input)?,
        ByteOrder::BigEndian => be_u32(input)?,
    };

    Ok((input, ImageFileDirectory {
        num_tags,
        tags,
        next_ifd_offset,
    }))
}

fn parse_tiff<'a>(input: &'a [u8]) -> IResult<&'a [u8], (TiffHeader, Vec<ImageFileDirectory>)> {
    let (input, header) = parse_tiff_header(input)?;
    
    let mut current_input = input;
    let mut directories = Vec::new();

    let mut current_offset = header.first_ifd_offset as usize;
    while current_offset < input.len() {
        current_input = &input[current_offset..];
        let (remaining, ifd) = parse_image_file_directory(current_input, &header.byte_order)?;
        directories.push(ifd);
        
        if directories.last().unwrap().next_ifd_offset == 0 {
            break;
        }
        current_offset += remaining.len() + 4;
    }

    Ok((input, (header, directories)))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tiff(&buffer) {
        Ok((_, (header, directories))) => {
            println!("TIFF Header: {:?}", header);
            for (i, dir) in directories.iter().enumerate() {
                println!("Image File Directory {}: {:?}", i, dir);
            }
        },
        Err(e) => eprintln!("Parsing error: {:?}", e),
    }

    Ok(())
}