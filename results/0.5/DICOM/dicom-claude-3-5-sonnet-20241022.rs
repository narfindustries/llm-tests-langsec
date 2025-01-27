use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct DicomHeader {
    preamble: Vec<u8>,
    prefix: Vec<u8>,
    meta_group: u16,
    meta_element: u16,
    meta_length: u32,
    file_meta_version: Vec<u8>,
    media_storage_sop_class_uid: String,
    media_storage_sop_instance_uid: String,
    transfer_syntax_uid: String,
    implementation_class_uid: String,
    implementation_version_name: String,
}

fn parse_dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = take(128usize)(input)?;
    let (input, prefix) = tag("DICM")(input)?;
    let (input, meta_group) = be_u16(input)?;
    let (input, meta_element) = be_u16(input)?;
    let (input, meta_length) = be_u32(input)?;
    let (input, file_meta_version) = take(2usize)(input)?;
    
    // Parse Media Storage SOP Class UID
    let (input, (_, _, uid_length)) = tuple((be_u16, be_u16, be_u32))(input)?;
    let (input, uid_bytes) = take(uid_length)(input)?;
    let media_storage_sop_class_uid = String::from_utf8_lossy(uid_bytes).to_string();

    // Parse Media Storage SOP Instance UID
    let (input, (_, _, uid_length)) = tuple((be_u16, be_u16, be_u32))(input)?;
    let (input, uid_bytes) = take(uid_length)(input)?;
    let media_storage_sop_instance_uid = String::from_utf8_lossy(uid_bytes).to_string();

    // Parse Transfer Syntax UID
    let (input, (_, _, uid_length)) = tuple((be_u16, be_u16, be_u32))(input)?;
    let (input, uid_bytes) = take(uid_length)(input)?;
    let transfer_syntax_uid = String::from_utf8_lossy(uid_bytes).to_string();

    // Parse Implementation Class UID
    let (input, (_, _, uid_length)) = tuple((be_u16, be_u16, be_u32))(input)?;
    let (input, uid_bytes) = take(uid_length)(input)?;
    let implementation_class_uid = String::from_utf8_lossy(uid_bytes).to_string();

    // Parse Implementation Version Name
    let (input, (_, _, name_length)) = tuple((be_u16, be_u16, be_u32))(input)?;
    let (input, name_bytes) = take(name_length)(input)?;
    let implementation_version_name = String::from_utf8_lossy(name_bytes).to_string();

    Ok((
        input,
        DicomHeader {
            preamble: preamble.to_vec(),
            prefix: prefix.to_vec(),
            meta_group,
            meta_element,
            meta_length,
            file_meta_version: file_meta_version.to_vec(),
            media_storage_sop_class_uid,
            media_storage_sop_instance_uid,
            transfer_syntax_uid,
            implementation_class_uid,
            implementation_version_name,
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

    match parse_dicom_header(&buffer) {
        Ok((_, header)) => println!("Parsed DICOM header: {:?}", header),
        Err(e) => eprintln!("Failed to parse DICOM header: {:?}", e),
    }
}