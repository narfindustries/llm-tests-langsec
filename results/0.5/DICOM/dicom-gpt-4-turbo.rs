use nom::{
    bytes::complete::{take, take_while},
    number::complete::{le_u16, le_u32, le_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

struct DicomHeader {
    group_number: u16,
    element_number: u16,
    vr: Option<String>,
    length: u32,
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], String> {
    let (input, vr) = take(2usize)(input)?;
    Ok((input, String::from_utf8_lossy(vr).to_string()))
}

fn parse_tag(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, group_number) = le_u16(input)?;
    let (input, element_number) = le_u16(input)?;
    let (input, vr) = take(2usize)(input)?;
    let vr = String::from_utf8_lossy(vr).to_string();
    let (input, length) = if ["OB", "OW", "OF", "SQ", "UT", "UN"].contains(&vr.as_str()) {
        let (input, _) = take(2usize)(input)?;
        let (input, length) = le_u32(input)?;
        (input, length)
    } else {
        let (input, length) = le_u16(input)?;
        (input, length as u32)
    };

    Ok((
        input,
        DicomHeader {
            group_number,
            element_number,
            vr: Some(vr),
            length,
        },
    ))
}

fn parse_dicom(input: &[u8]) -> IResult<&[u8], Vec<DicomHeader>> {
    let mut headers = Vec::new();
    let mut remaining = input;

    while !remaining.is_empty() {
        let result = parse_tag(remaining);
        match result {
            Ok((input, header)) => {
                headers.push(header);
                remaining = input;
            }
            Err(_) => break,
        }
    }

    Ok((remaining, headers))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "No file specified",
        ));
    }
    let file_path = &args[1];
    let mut file = File::open(file_path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dicom(&buffer) {
        Ok((_, headers)) => {
            for header in headers {
                println!(
                    "Group: {:04X}, Element: {:04X}, VR: {:?}, Length: {}",
                    header.group_number, header.element_number, header.vr, header.length
                );
            }
        }
        Err(e) => println!("Error parsing DICOM: {:?}", e),
    }

    Ok(())
}