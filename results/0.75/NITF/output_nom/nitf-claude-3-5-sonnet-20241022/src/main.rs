use std::fs::File;
use std::io::Read;
use std::path::Path;
use nom::{
    bytes::complete::{tag, take},
    character::complete::{char, digit1},
    combinator::{map, map_res},
    sequence::tuple,
    IResult,
};

#[derive(Debug)]
struct NitfHeader {
    file_header: String,      // FHDR - 4 chars
    file_version: String,     // FVER - 5 chars
    complexity_level: u8,     // CLEVEL - 2 chars
    standard_type: String,    // STYPE - 4 chars
    originating_station: String, // OSTAID - 10 chars
    file_date_time: String,   // FDT - 14 chars
    file_title: String,       // FTITLE - 80 chars
    file_security: FileSecurity,
    copy_number: String,      // FSCOP - 5 chars
    num_copies: String,       // FSCPYS - 5 chars
    encrypted: char,          // ENCRYP
    background_color: String, // FBKGC - 3 chars
    originator_name: String,  // ONAME - 24 chars
    originator_phone: String, // OPHONE - 18 chars
    num_image_segments: u16,  // NUMI - 3 chars
    num_graphic_segments: u16,// NUMS - 3 chars
    reserved1: String,        // NUMX - 3 chars
    num_text_segments: u16,   // NUMT - 3 chars
    num_data_extensions: u16, // NUMDES - 3 chars
    num_reserved_extensions: u16, // NUMRES - 3 chars
}

#[derive(Debug)]
struct FileSecurity {
    classification: String,   // FSCLAS - 1 char
    system: String,          // FSCLSY - 2 chars
    codewords: String,       // FSCODE - 11 chars
    control_and_handling: String, // FSCTLH - 2 chars
    release_instructions: String, // FSREL - 20 chars
    declass_type: String,    // FSDCTP - 2 chars
    declass_date: String,    // FSDCDT - 8 chars
    declass_exemption: String, // FSDCXM - 4 chars
    downgrade: String,       // FSDG - 1 char
    downgrade_date: String,  // FSDGDT - 8 chars
    classification_text: String, // FSCLTX - 43 chars
    classification_authority_type: String, // FSCATP - 1 char
    classification_authority: String,     // FSCAUT - 40 chars
    classification_reason: String,        // FSCRSN - 1 char
    security_source_date: String,        // FSSRDT - 8 chars
    security_control_number: String,     // FSCTLN - 15 chars
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, file_header) = map(take(4usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, file_version) = map(take(5usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, complexity_level) = map_res(take(2usize), |s: &[u8]| {
        String::from_utf8_lossy(s).parse::<u8>()
    })(input)?;
    let (input, standard_type) = map(take(4usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, originating_station) = map(take(10usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, file_date_time) = map(take(14usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, file_title) = map(take(80usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    
    let (input, file_security) = parse_file_security(input)?;
    
    let (input, copy_number) = map(take(5usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, num_copies) = map(take(5usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, encrypted) = map(take(1usize), |s: &[u8]| s[0] as char)(input)?;
    let (input, background_color) = map(take(3usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, originator_name) = map(take(24usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, originator_phone) = map(take(18usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    
    let (input, num_image_segments) = map_res(take(3usize), |s: &[u8]| {
        String::from_utf8_lossy(s).parse::<u16>()
    })(input)?;
    let (input, num_graphic_segments) = map_res(take(3usize), |s: &[u8]| {
        String::from_utf8_lossy(s).parse::<u16>()
    })(input)?;
    let (input, reserved1) = map(take(3usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, num_text_segments) = map_res(take(3usize), |s: &[u8]| {
        String::from_utf8_lossy(s).parse::<u16>()
    })(input)?;
    let (input, num_data_extensions) = map_res(take(3usize), |s: &[u8]| {
        String::from_utf8_lossy(s).parse::<u16>()
    })(input)?;
    let (input, num_reserved_extensions) = map_res(take(3usize), |s: &[u8]| {
        String::from_utf8_lossy(s).parse::<u16>()
    })(input)?;

    Ok((input, NitfHeader {
        file_header,
        file_version,
        complexity_level,
        standard_type,
        originating_station,
        file_date_time,
        file_title,
        file_security,
        copy_number,
        num_copies,
        encrypted,
        background_color,
        originator_name,
        originator_phone,
        num_image_segments,
        num_graphic_segments,
        reserved1,
        num_text_segments,
        num_data_extensions,
        num_reserved_extensions,
    }))
}

fn parse_file_security(input: &[u8]) -> IResult<&[u8], FileSecurity> {
    let (input, classification) = map(take(1usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, system) = map(take(2usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, codewords) = map(take(11usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, control_and_handling) = map(take(2usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, release_instructions) = map(take(20usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, declass_type) = map(take(2usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, declass_date) = map(take(8usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, declass_exemption) = map(take(4usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, downgrade) = map(take(1usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, downgrade_date) = map(take(8usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, classification_text) = map(take(43usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, classification_authority_type) = map(take(1usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, classification_authority) = map(take(40usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, classification_reason) = map(take(1usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, security_source_date) = map(take(8usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;
    let (input, security_control_number) = map(take(15usize), |s: &[u8]| String::from_utf8_lossy(s).to_string())(input)?;

    Ok((input, FileSecurity {
        classification,
        system,
        codewords,
        control_and_handling,
        release_instructions,
        declass_type,
        declass_date,
        declass_exemption,
        downgrade,
        downgrade_date,
        classification_text,
        classification_authority_type,
        classification_authority,
        classification_reason,
        security_source_date,
        security_control_number,
    }))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <nitf_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_nitf_header(&buffer) {
        Ok((remaining, header)) => {
            println!("Successfully parsed NITF header:");
            println!("{:#?}", header);
        }
        Err(e) => {
            eprintln!("Failed to parse NITF file: {:?}", e);
            std::process::exit(1);
        }
    }
}