use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct DicomHeader {
    group_number: u16,
    element_number: u16,
    vr: String,
    length: u32,
}

fn parse_dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, (group_number, element_number, vr, length)) = tuple((
        le_u16, // Group Number
        le_u16, // Element Number
        map_res(take(2usize), |bytes: &[u8]| {
            std::str::from_utf8(bytes).map(String::from)
        }), // Value Representation
        le_u32, // Length
    ))(input)?;

    Ok((
        input,
        DicomHeader {
            group_number,
            element_number,
            vr,
            length,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dicom-file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dicom_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => eprintln!("Error parsing DICOM file: {:?}", e),
    }

    Ok(())
}