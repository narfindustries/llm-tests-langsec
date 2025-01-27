use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{many0, many1},
    number::complete::{be_i16, be_i32, be_u16, be_u32},
    IResult,
};
use std::{
    env, fs,
    io::{Read, Result},
    path::Path,
};

// Define the TAG type
#[derive(Debug, PartialEq)]
struct Tag(u16, u16);

impl Tag {
    fn new(group: u16, element: u16) -> Self {
        Tag(group, element)
    }
}

// Define the Element type
#[derive(Debug)]
enum Element {
    UI(Vec<u8>),
    OB(Vec<u8>),
    OD(Vec<u8>),
    OF(Vec<u8>),
    OW(Vec<u8>),
    UL(u32),
    SL(i32),
    SS(i16),
    US(u16),
    // Add other data types as needed
}

// Define the Dataset type
#[derive(Debug)]
struct Dataset {
    elements: Vec<(Tag, Element)>,
}

// Define the FileMetaInfo type
#[derive(Debug)]
struct FileMetaInfo {
    transfer_syntax_uid: Vec<u8>,
    media_storage_sop_instance_uid: Vec<u8>,
    implementation_class_uid: Vec<u8>,
    transfer_syntax_uid_explicit: bool,
}

// Define the Preamble type
#[derive(Debug)]
struct Preamble {
    prefix: Vec<u8>,
    dicom: Vec<u8>,
}

// Define the DICOM type
#[derive(Debug)]
struct Dicom {
    preamble: Preamble,
    file_meta_info: FileMetaInfo,
    dataset: Dataset,
}

// Parse a TAG
fn parse_tag(input: &[u8]) -> IResult<&[u8], Tag> {
    map(be_u16, |group| {
        map(be_u16, move |element| Tag::new(group, element))
    })(input)
}

// Parse an Element
fn parse_element(input: &[u8]) -> IResult<&[u8], (Tag, Element)> {
    let (input, tag) = parse_tag(input)?;
    let (input, vr) = take(2u8)(input)?;
    let (input, length) = be_u32(input)?;
    let (input, value) = take(length as usize)(input)?;

    let element = match vr {
        b"UI" => Element::UI(value.to_vec()),
        b"OB" => Element::OB(value.to_vec()),
        b"OD" => Element::OD(value.to_vec()),
        b"OF" => Element::OF(value.to_vec()),
        b"OW" => Element::OW(value.to_vec()),
        b"UL" => Element::UL(be_u32(value).unwrap().1),
        b"SL" => Element::SL(be_i32(value).unwrap().1),
        b"SS" => Element::SS(be_i16(value).unwrap().1),
        b"US" => Element::US(be_u16(value).unwrap().1),
        _ => panic!("Unsupported VR: {:?}", vr),
    };

    Ok((input, (tag, element)))
}

// Parse a Dataset
fn parse_dataset(input: &[u8]) -> IResult<&[u8], Dataset> {
    let (input, elements) = many0(parse_element)(input)?;
    Ok((input, Dataset { elements }))
}

// Parse a FileMetaInfo
fn parse_file_meta_info(input: &[u8]) -> IResult<&[u8], FileMetaInfo> {
    let (input, transfer_syntax_uid) = take(64)(input)?;
    let (input, media_storage_sop_instance_uid) = take(64)(input)?;
    let (input, implementation_class_uid) = take(64)(input)?;
    let (input, transfer_syntax_uid_explicit) = tag(b"\x01")(input).is_ok();

    Ok((
        input,
        FileMetaInfo {
            transfer_syntax_uid: transfer_syntax_uid.to_vec(),
            media_storage_sop_instance_uid: media_storage_sop_instance_uid.to_vec(),
            implementation_class_uid: implementation_class_uid.to_vec(),
            transfer_syntax_uid_explicit,
        },
    ))
}

// Parse a Preamble
fn parse_preamble(input: &[u8]) -> IResult<&[u8], Preamble> {
    let (input, prefix) = take(128)(input)?;
    let (input, dicom) = tag(b"DICM")(input)?;

    Ok((input, Preamble { prefix: prefix.to_vec(), dicom: dicom.to_vec() }))
}

// Parse a DICOM
fn parse_dicom(input: &[u8]) -> IResult<&[u8], Dicom> {
    let (input, preamble) = parse_preamble(input)?;
    let (input, file_meta_info) = parse_file_meta_info(input)?;
    let (input, dataset) = parse_dataset(input)?;

    Ok((input, Dicom { preamble, file_meta_info, dataset }))
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    let file_path = Path::new(&args[1]);

    let mut file = fs::File::open(file_path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let result = parse_dicom(&buffer);
    match result {
        Ok((_, dicom)) => println!("{:?}", dicom),
        Err(e) => println!("Error parsing DICOM: {:?}", e),
    }

    Ok(())
}