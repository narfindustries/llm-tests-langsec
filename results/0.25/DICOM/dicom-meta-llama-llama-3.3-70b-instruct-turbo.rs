use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::many0,
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum DicomElement {
    ItemDelimitationItem,
    SequenceDelimitationItem,
    DataElement(DicomDataElement),
}

#[derive(Debug, PartialEq)]
struct DicomDataElement {
    tag: (u16, u16),
    vr: String,
    length: u32,
    value: Vec<u8>,
}

fn parse_dcm_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    alt((
        map(tag([0xFF, 0xFE, 0x00, 0x00]), |_| DicomElement::ItemDelimitationItem),
        map(
            tag([0xFF, 0xFE, 0x00, 0x01]),
            |_| DicomElement::SequenceDelimitationItem,
        ),
        map(parse_data_element, DicomElement::DataElement),
    ))(input)
}

fn parse_data_element(input: &[u8]) -> IResult<&[u8], DicomDataElement> {
    let (input, tag) = parse_tag(input)?;
    let (input, vr) = take(2u8)(input)?;
    let (input, length) = be_u32(input)?;
    let (input, value) = take(length as usize)(input)?;
    Ok((input, DicomDataElement { tag, vr: String::from_utf8_lossy(vr).into_owned(), length, value: value.to_vec() }))
}

fn parse_tag(input: &[u8]) -> IResult<&[u8], (u16, u16)> {
    let (input, group) = be_u16(input)?;
    let (input, element) = be_u16(input)?;
    Ok((input, (group, element)))
}

fn alt<I, O, E: nom::error::ParseError<I>>(parsers: Vec<impl nom::Parser<I, O, E>>) -> impl nom::Parser<I, O, E> {
    nom::branch::alt(parsers)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let mut file = File::open(filename).expect("Unable to open file!");
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Unable to read file!");
    let result = many0(parse_dcm_element)(&data);
    match result {
        Ok((_, elements)) => {
            for element in elements {
                match element {
                    DicomElement::ItemDelimitationItem => println!("ItemDelimitationItem"),
                    DicomElement::SequenceDelimitationItem => println!("SequenceDelimitationItem"),
                    DicomElement::DataElement(element) => println!("DataElement: {:?} {:?} {:?}", element.tag, element.vr, element.length),
                }
            }
        }
        Err(err) => println!("Error: {:?}", err),
    }
}