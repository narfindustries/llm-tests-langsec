use std::env;
use std::fs;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};

#[derive(Debug)]
struct DicomHeader {
    preamble: Vec<u8>,
    prefix: Vec<u8>,
    meta_group: MetaInformationGroup,
}

#[derive(Debug)]
struct MetaInformationGroup {
    group_length: u32,
    file_meta_information_version: Vec<u8>,
    media_storage_sop_class_uid: String,
    media_storage_sop_instance_uid: String,
    transfer_syntax_uid: String,
    implementation_class_uid: Option<String>,
    implementation_version_name: Option<String>,
    source_application_entity_title: Option<String>,
    sending_application_entity_title: Option<String>,
    receiving_application_entity_title: Option<String>,
    private_information_creator_uid: Option<String>,
    private_information: Option<Vec<u8>>,
}

fn parse_preamble(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(take(128usize), |bytes: &[u8]| bytes.to_vec())(input)
}

fn parse_prefix(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(tag("DICM"), |bytes: &[u8]| bytes.to_vec())(input)
}

fn parse_vr_string(input: &[u8], length: usize) -> IResult<&[u8], String> {
    map(take(length), |bytes: &[u8]| {
        String::from_utf8_lossy(bytes).trim().to_string()
    })(input)
}

fn parse_meta_information_group(input: &[u8]) -> IResult<&[u8], MetaInformationGroup> {
    let (input, (
        group_length,
        file_meta_information_version,
        media_storage_sop_class_uid,
        media_storage_sop_instance_uid,
        transfer_syntax_uid,
        implementation_class_uid,
        implementation_version_name,
        source_application_entity_title,
        sending_application_entity_title,
        receiving_application_entity_title,
        private_information_creator_uid,
        private_information,
    )) = tuple((
        le_u32,
        map(take(2usize), |bytes: &[u8]| bytes.to_vec()),
        map(take(64usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).trim().to_string()),
        map(take(64usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).trim().to_string()),
        map(take(64usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).trim().to_string()),
        opt(map(take(64usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).trim().to_string())),
        opt(map(take(16usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).trim().to_string())),
        opt(map(take(16usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).trim().to_string())),
        opt(map(take(16usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).trim().to_string())),
        opt(map(take(16usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).trim().to_string())),
        opt(map(take(64usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).trim().to_string())),
        opt(map(take(le_u16), |bytes: &[u8]| bytes.to_vec())),
    ))(input)?;

    Ok((input, MetaInformationGroup {
        group_length,
        file_meta_information_version,
        media_storage_sop_class_uid,
        media_storage_sop_instance_uid,
        transfer_syntax_uid,
        implementation_class_uid,
        implementation_version_name,
        source_application_entity_title,
        sending_application_entity_title,
        receiving_application_entity_title,
        private_information_creator_uid,
        private_information,
    }))
}

fn parse_dicom(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, (preamble, prefix, meta_group)) = tuple((
        parse_preamble,
        parse_prefix,
        parse_meta_information_group,
    ))(input)?;

    Ok((input, DicomHeader {
        preamble,
        prefix,
        meta_group,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let data = fs::read(filename)?;
    
    match parse_dicom(&data) {
        Ok((_, dicom)) => println!("{:#?}", dicom),
        Err(e) => eprintln!("Error parsing DICOM: {:?}", e),
    }

    Ok(())
}