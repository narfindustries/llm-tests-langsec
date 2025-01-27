use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::many0,
    number::complete::{be_u16, be_u32, le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct DicomHeader {
    preamble: Vec<u8>,
    prefix: Vec<u8>,
    meta_info: MetaInformation,
    data_set: Vec<DataElement>,
}

#[derive(Debug)]
struct MetaInformation {
    group_length: u32,
    file_meta_info_version: Vec<u8>,
    media_storage_sop_class_uid: String,
    media_storage_sop_instance_uid: String,
    transfer_syntax_uid: String,
    implementation_class_uid: String,
    implementation_version_name: Option<String>,
}

#[derive(Debug)]
struct DataElement {
    tag: (u16, u16),
    vr: Option<String>,
    length: u32,
    value: Vec<u8>,
}

fn parse_dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = take(128usize)(input)?;
    let (input, prefix) = tag(b"DICM")(input)?;
    let (input, meta_info) = parse_meta_information(input)?;
    let (input, data_set) = many0(parse_data_element)(input)?;

    Ok((
        input,
        DicomHeader {
            preamble: preamble.to_vec(),
            prefix: prefix.to_vec(),
            meta_info,
            data_set,
        },
    ))
}

fn parse_meta_information(input: &[u8]) -> IResult<&[u8], MetaInformation> {
    let (input, group_length) = parse_group_length(input)?;
    let (input, file_meta_info_version) = parse_file_meta_info_version(input)?;
    let (input, media_storage_sop_class_uid) = parse_media_storage_sop_class_uid(input)?;
    let (input, media_storage_sop_instance_uid) = parse_media_storage_sop_instance_uid(input)?;
    let (input, transfer_syntax_uid) = parse_transfer_syntax_uid(input)?;
    let (input, implementation_class_uid) = parse_implementation_class_uid(input)?;
    let (input, implementation_version_name) = opt(parse_implementation_version_name)(input)?;

    Ok((
        input,
        MetaInformation {
            group_length,
            file_meta_info_version,
            media_storage_sop_class_uid,
            media_storage_sop_instance_uid,
            transfer_syntax_uid,
            implementation_class_uid,
            implementation_version_name,
        },
    ))
}

fn parse_group_length(input: &[u8]) -> IResult<&[u8], u32> {
    let (input, _) = tag(&[0x02, 0x00, 0x00, 0x00])(input)?;
    let (input, _) = tag(b"UL")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, value) = be_u32(input)?;
    Ok((input, value))
}

fn parse_file_meta_info_version(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = tag(&[0x02, 0x00, 0x01, 0x00])(input)?;
    let (input, _) = tag(b"OB")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, value) = take(length as usize)(input)?;
    Ok((input, value.to_vec()))
}

fn parse_media_storage_sop_class_uid(input: &[u8]) -> IResult<&[u8], String> {
    let (input, _) = tag(&[0x02, 0x00, 0x02, 0x00])(input)?;
    let (input, _) = tag(b"UI")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, value) = take(length as usize)(input)?;
    Ok((input, String::from_utf8_lossy(value).into_owned()))
}

fn parse_media_storage_sop_instance_uid(input: &[u8]) -> IResult<&[u8], String> {
    let (input, _) = tag(&[0x02, 0x00, 0x03, 0x00])(input)?;
    let (input, _) = tag(b"UI")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, value) = take(length as usize)(input)?;
    Ok((input, String::from_utf8_lossy(value).into_owned()))
}

fn parse_transfer_syntax_uid(input: &[u8]) -> IResult<&[u8], String> {
    let (input, _) = tag(&[0x02, 0x00, 0x10, 0x00])(input)?;
    let (input, _) = tag(b"UI")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, value) = take(length as usize)(input)?;
    Ok((input, String::from_utf8_lossy(value).into_owned()))
}

fn parse_implementation_class_uid(input: &[u8]) -> IResult<&[u8], String> {
    let (input, _) = tag(&[0x02, 0x00, 0x12, 0x00])(input)?;
    let (input, _) = tag(b"UI")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, value) = take(length as usize)(input)?;
    Ok((input, String::from_utf8_lossy(value).into_owned()))
}

fn parse_implementation_version_name(input: &[u8]) -> IResult<&[u8], String> {
    let (input, _) = tag(&[0x02, 0x00, 0x13, 0x00])(input)?;
    let (input, _) = tag(b"SH")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, value) = take(length as usize)(input)?;
    Ok((input, String::from_utf8_lossy(value).into_owned()))
}

fn parse_data_element(input: &[u8]) -> IResult<&[u8], DataElement> {
    let (input, tag) = tuple((be_u16, be_u16))(input)?;
    let (input, vr) = opt(map(take(2usize), |v: &[u8]| {
        String::from_utf8_lossy(v).into_owned()
    }))(input)?;
    let (input, length) = if let Some(ref vr) = vr {
        match vr.as_str() {
            "OB" | "OW" | "OF" | "SQ" | "UT" | "UN" => {
                let (input, _) = take(2usize)(input)?;
                let (input, length) = be_u32(input)?;
                (input, length)
            }
            _ => {
                let (input, length) = be_u16(input)?;
                (input, length as u32)
            }
        }
    } else {
        let (input, length) = be_u32(input)?;
        (input, length)
    };
    let (input, value) = take(length as usize)(input)?;

    Ok((
        input,
        DataElement {
            tag,
            vr,
            length,
            value: value.to_vec(),
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
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Error parsing DICOM file: {:?}", e),
    }
}