use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    number::complete::{be_u16, be_u32, be_i32},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct DicomElement {
    group: u16,
    element: u16,
    length: u32,
    value: Vec<u8>,
}

fn dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, group) = be_u16(input)?;
    let (input, element) = be_u16(input)?;
    let (input, length) = be_u32(input)?;
    let (input, value) = take(length)(input)?;
    Ok((input, DicomElement { group, element, length: length as u32, value: value.to_vec() }))
}


fn dicom_header(input: &[u8]) -> IResult<&[u8], Vec<DicomElement>> {
    let (input, _) = tag("DICM")(input)?;
    let (input, elements) = many0(dicom_element)(input)?;
    Ok((input, elements))

}

fn many0<I, O, E, F>(f: F) -> impl Fn(I) -> IResult<I, Vec<O>, E>
where
    F: Fn(I) -> IResult<I, O, E>,
    I: Clone + std::fmt::Debug,
{
    move |i: I| {
        let mut res = Vec::new();
        let mut input = i.clone();
        loop {
            match f(input.clone()) {
                Ok((i1, o)) => {
                    res.push(o);
                    input = i1;
                }
                Err(nom::Err::Incomplete(_)) => return Err(nom::Err::Incomplete(_)),
                Err(e) => return Err(e),
                _ => break,
            }
        }
        Ok((input, res))
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <dicom_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    match dicom_header(&buffer) {
        Ok((_, elements)) => {
            println!("DICOM elements:");
            for element in elements {
                println!("Group: {:04X}, Element: {:04X}, Length: {}, Value: {:?}", element.group, element.element, element.length, element.value);
            }
        }
        Err(err) => {
            println!("Error parsing DICOM file: {:?}", err);
        }
    }
}
