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
    meta_info: DicomMetaInfo,
    data_set: Vec<DataElement>,
}

#[derive(Debug)]
struct DicomMetaInfo {
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

fn parse_preamble(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(take(128usize), |bytes: &[u8]| bytes.to_vec())(input)
}

fn parse_prefix(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(tag("DICM"), |bytes: &[u8]| bytes.to_vec())(input)
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], String> {
    map(take(2usize), |bytes: &[u8]| {
        String::from_utf8_lossy(bytes).to_string()
    })(input)
}

fn parse_data_element_explicit(input: &[u8]) -> IResult<&[u8], DataElement> {
    let (input, tag) = tuple((be_u16, be_u16))(input)?;
    let (input, vr) = parse_vr(input)?;
    let (input, length) = match vr.as_str() {
        "OB" | "OW" | "OF" | "SQ" | "UT" | "UN" => {
            let (input, _) = take(2usize)(input)?;
            let (input, length) = be_u32(input)?;
            (input, length)
        }
        _ => {
            let (input, length) = be_u16(input)?;
            (input, length as u32)
        }
    };
    let (input, value) = take(length as usize)(input)?;
    Ok((
        input,
        DataElement {
            tag,
            vr: Some(vr),
            length,
            value: value.to_vec(),
        },
    ))
}

fn parse_data_element_implicit(input: &[u8]) -> IResult<&[u8], DataElement> {
    let (input, tag) = tuple((le_u16, le_u16))(input)?;
    let (input, length) = le_u32(input)?;
    let (input, value) = take(length as usize)(input)?;
    Ok((
        input,
        DataElement {
            tag,
            vr: None,
            length,
            value: value.to_vec(),
        },
    ))
}

fn parse_meta_info(input: &[u8]) -> IResult<&[u8], DicomMetaInfo> {
    let (input, elements) = many0(parse_data_element_explicit)(input)?;
    let mut meta_info = DicomMetaInfo {
        group_length: 0,
        file_meta_info_version: vec![],
        media_storage_sop_class_uid: String::new(),
        media_storage_sop_instance_uid: String::new(),
        transfer_syntax_uid: String::new(),
        implementation_class_uid: String::new(),
        implementation_version_name: None,
    };

    for element in elements {
        match element.tag {
            (0x0002, 0x0000) => meta_info.group_length = be_u32(&element.value[..])?,
            (0x0002, 0x0001) => meta_info.file_meta_info_version = element.value,
            (0x0002, 0x0002) => {
                meta_info.media_storage_sop_class_uid =
                    String::from_utf8_lossy(&element.value).to_string()
            }
            (0x0002, 0x0003) => {
                meta_info.media_storage_sop_instance_uid =
                    String::from_utf8_lossy(&element.value).to_string()
            }
            (0x0002, 0x0010) => {
                meta_info.transfer_syntax_uid = String::from_utf8_lossy(&element.value).to_string()
            }
            (0x0002, 0x0012) => {
                meta_info.implementation_class_uid =
                    String::from_utf8_lossy(&element.value).to_string()
            }
            (0x0002, 0x0013) => {
                meta_info.implementation_version_name =
                    Some(String::from_utf8_lossy(&element.value).to_string())
            }
            _ => {}
        }
    }

    Ok((input, meta_info))
}

fn parse_dicom(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = parse_preamble(input)?;
    let (input, prefix) = parse_prefix(input)?;
    let (input, meta_info) = parse_meta_info(input)?;
    let (input, data_set) = many0(parse_data_element_implicit)(input)?;

    Ok((
        input,
        DicomHeader {
            preamble,
            prefix,
            meta_info,
            data_set,
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
    file.read_to_end(&mut buffer)
        .expect("Failed to read file contents");

    match parse_dicom(&buffer) {
        Ok((_, dicom)) => println!("{:#?}", dicom),
        Err(e) => eprintln!("Failed to parse DICOM file: {:?}", e),
    }
}