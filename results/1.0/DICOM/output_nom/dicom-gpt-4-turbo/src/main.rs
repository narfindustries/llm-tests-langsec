use nom::{
    bytes::complete::take,
    combinator::{map_res, map_parser},
    number::complete::le_u32,
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::{self, Read}, str::Utf8Error};

#[derive(Debug)]
struct DicomHeader {
    group_length: u32,
    transfer_syntax_uid: String,
}

fn parse_transfer_syntax_uid(input: &[u8], length: usize) -> IResult<&[u8], String> {
    map_res(take(length), std::str::from_utf8)(input).map(|(next_input, result)| (next_input, result.to_string()))
}

fn parse_dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, (group_length, _)) = tuple((le_u32, take(4usize)))(input)?;
    let (input, transfer_syntax_uid) = parse_transfer_syntax_uid(input, group_length as usize)?;
    Ok((input, DicomHeader { group_length, transfer_syntax_uid }))
}

fn read_file_to_vec(file_path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(file_path)?;
    let mut file_content = Vec::new();
    file.read_to_end(&mut file_content)?;
    Ok(file_content)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        std::process::exit(1);
    }

    let dicom_file_content = read_file_to_vec(&args[1]).unwrap_or_else(|err| {
        eprintln!("Error reading the DICOM file: {}", err);
        std::process::exit(1);
    });

    match parse_dicom_header(&dicom_file_content) {
        Ok((remaining_input, dicom_header)) => {
            println!("{:?}", dicom_header);
            println!("Remaining data length: {}", remaining_input.len());
        },
        Err(e) => eprintln!("Failed to parse DICOM header: {:?}", e),
    }
}