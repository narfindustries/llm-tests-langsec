use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct DicomHeader {
    preamble: Vec<u8>,
    prefix: Vec<u8>,
    meta_group: u16,
    meta_element: u16,
    meta_length: u32,
    file_meta_info: FileMetaInfo,
}

#[derive(Debug)]
struct FileMetaInfo {
    file_meta_info_version: Vec<u8>,
    media_storage_sop_class_uid: String,
    media_storage_sop_instance_uid: String,
    transfer_syntax_uid: String,
    implementation_class_uid: String,
    implementation_version_name: Option<String>,
}

fn parse_preamble(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, preamble) = take(128usize)(input)?;
    Ok((input, preamble.to_vec()))
}

fn parse_prefix(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, prefix) = tag("DICM")(input)?;
    Ok((input, prefix.to_vec()))
}

fn parse_meta_header(input: &[u8]) -> IResult<&[u8], (u16, u16, u32)> {
    let (input, (group, element, length)) = tuple((be_u16, be_u16, be_u32))(input)?;
    Ok((input, (group, element, length)))
}

fn parse_vr_string(input: &[u8], length: usize) -> IResult<&[u8], String> {
    let (input, data) = take(length)(input)?;
    Ok((input, String::from_utf8_lossy(data).trim().to_string()))
}

fn parse_file_meta_info(input: &[u8]) -> IResult<&[u8], FileMetaInfo> {
    let (input, version) = take(2usize)(input)?;
    
    let (input, (_, _, uid_length)) = parse_meta_header(input)?;
    let (input, sop_class_uid) = parse_vr_string(input, uid_length as usize)?;
    
    let (input, (_, _, uid_length)) = parse_meta_header(input)?;
    let (input, sop_instance_uid) = parse_vr_string(input, uid_length as usize)?;
    
    let (input, (_, _, uid_length)) = parse_meta_header(input)?;
    let (input, transfer_syntax_uid) = parse_vr_string(input, uid_length as usize)?;
    
    let (input, (_, _, uid_length)) = parse_meta_header(input)?;
    let (input, implementation_class_uid) = parse_vr_string(input, uid_length as usize)?;
    
    let (input, implementation_version_name) = if !input.is_empty() {
        let (input, (_, _, length)) = parse_meta_header(input)?;
        let (input, name) = parse_vr_string(input, length as usize)?;
        (input, Some(name))
    } else {
        (input, None)
    };

    Ok((
        input,
        FileMetaInfo {
            file_meta_info_version: version.to_vec(),
            media_storage_sop_class_uid: sop_class_uid,
            media_storage_sop_instance_uid: sop_instance_uid,
            transfer_syntax_uid: transfer_syntax_uid,
            implementation_class_uid: implementation_class_uid,
            implementation_version_name: implementation_version_name,
        },
    ))
}

fn parse_dicom(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = parse_preamble(input)?;
    let (input, prefix) = parse_prefix(input)?;
    let (input, (meta_group, meta_element, meta_length)) = parse_meta_header(input)?;
    let (input, file_meta_info) = parse_file_meta_info(input)?;

    Ok((
        input,
        DicomHeader {
            preamble,
            prefix,
            meta_group,
            meta_element,
            meta_length,
            file_meta_info,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_dicom(&buffer) {
        Ok((_, dicom)) => println!("{:#?}", dicom),
        Err(e) => eprintln!("Failed to parse DICOM: {:?}", e),
    }
}