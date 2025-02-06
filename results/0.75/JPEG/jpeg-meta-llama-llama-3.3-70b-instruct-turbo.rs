use nom::{
    bytes::complete::{tag, take, take_while_m_n},
    combinator::{map, opt},
    number::complete::{be_u16, be_u8},
    sequence::{tuple},
    IResult,
    error::ErrorKind,
};
use std::{env, fs, result};

#[derive(Debug, PartialEq)]
enum Marker {
    SOI,
    APP0,
    APP1,
    DQT,
    DHT,
    SOF,
    DRI,
    SOS,
    RST(u8),
    EOI,
}

fn marker(input: &[u8]) -> IResult<&[u8], Marker> {
    let (input, _) = tag(&[0xFF])(input)?;
    let (input, marker) = be_u8(input)?;
    match marker {
        0xD8 => Ok((input, Marker::SOI)),
        0xE0 => Ok((input, Marker::APP0)),
        0xE1 => Ok((input, Marker::APP1)),
        0xDB => Ok((input, Marker::DQT)),
        0xC4 => Ok((input, Marker::DHT)),
        0xC0 => Ok((input, Marker::SOF)),
        0xDD => Ok((input, Marker::DRI)),
        0xDA => Ok((input, Marker::SOS)),
        0xD0..=0xD7 => Ok((input, Marker::RST(marker - 0xD0))),
        0xD9 => Ok((input, Marker::EOI)),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, ErrorKind::AlphaNumeric))),
    }
}

fn app0(input: &[u8]) -> IResult<&[u8], (&[u8], u8, u8, u8, u8, u8)> {
    let (input, _) = tag(&[0xFF, 0xE0])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5u8)(input)?;
    let (input, version) = tuple((be_u8, be_u8))(input)?;
    let (input, units) = be_u8(input)?;
    let (input, _x_density) = be_u8(input)?;
    let (input, _y_density) = be_u8(input)?;
    let (input, x_thumbnail) = be_u8(input)?;
    let (input, y_thumbnail) = be_u8(input)?;
    Ok((
        &input[length as usize - 8..],
        (
            identifier,
            version.0,
            version.1,
            units,
            x_thumbnail,
            y_thumbnail,
        ),
    ))
}

fn dqt(input: &[u8]) -> IResult<&[u8], (u8, u8, Vec<u8>)> {
    let (input, _) = tag(&[0xFF, 0xDB])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, table_number) = be_u8(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, table_data) = take(length - 3)(input)?;
    Ok((
        &input[length as usize - 3..],
        (table_number, precision, table_data.to_vec()),
    ))
}

fn dht(input: &[u8]) -> IResult<&[u8], (u8, u8, Vec<u8>)> {
    let (input, _) = tag(&[0xFF, 0xC4])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, table_class) = be_u8(input)?;
    let (input, table_destination_identifier) = be_u8(input)?;
    let (input, _number_of_codes) = be_u8(input)?;
    let (input, code_lengths) = take(length - 3)(input)?;
    Ok((
        &input[length as usize - 3..],
        (
            table_class,
            table_destination_identifier,
            code_lengths.to_vec(),
        ),
    ))
}

fn sof(input: &[u8]) -> IResult<&[u8], (u8, u16, u16, u8)> {
    let (input, _) = tag(&[0xFF, 0xC0])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, image_height) = be_u16(input)?;
    let (input, image_width) = be_u16(input)?;
    let (input, number_of_components) = be_u8(input)?;
    Ok((
        &input[length as usize - 8..],
        (precision, image_height, image_width, number_of_components),
    ))
}

fn dri(input: &[u8]) -> IResult<&[u8], u16> {
    let (input, _) = tag(&[0xFF, 0xDD])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, restart_interval) = be_u16(input)?;
    Ok((&input[length as usize - 2..], restart_interval))
}

fn sos(input: &[u8]) -> IResult<&[u8], (u8, Vec<u8>)> {
    let (input, _) = tag(&[0xFF, 0xDA])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, number_of_components) = be_u8(input)?;
    let (input, component_identifiers) = take(length - 3)(input)?;
    Ok((
        &input[length as usize - 3..],
        (number_of_components, component_identifiers.to_vec()),
    ))
}

fn rst(input: &[u8]) -> IResult<&[u8], u8> {
    let (input, _) = tag(&[0xFF, 0xD0])(input)?;
    let (input, rst_number) = be_u8(input)?;
    Ok((&input[1..], rst_number - 0xD0))
}

fn eoi(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0xFF, 0xD9])(input)?;
    Ok((input, ()))
}

fn main() -> result::Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Err("Invalid arguments".into());
    }
    let input_file = &args[1];
    let data = match fs::read(input_file) {
        Ok(data) => data,
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return Err(err.into());
        }
    };
    let mut input = &data[..];
    loop {
        match marker(input) {
            Ok((new_input, marker)) => {
                input = new_input;
                match marker {
                    Marker::SOI => println!("SOI"),
                    Marker::APP0 => {
                        let (new_input, app0) = match app0(input) {
                            Ok(result) => result,
                            Err(err) => {
                                eprintln!("Error parsing APP0: {:?}", err);
                                break;
                            }
                        };
                        input = new_input;
                        println!("APP0 {:?}", app0);
                    }
                    Marker::APP1 => {
                        let (new_input, _) = take_while_m_n::<_, &[u8], nom::error::Error<&[u8]>>(2, 65535, |c| c != 0xFF)(input)?;
                        input = new_input;
                    }
                    Marker::DQT => {
                        let (new_input, dqt) = match dqt(input) {
                            Ok(result) => result,
                            Err(err) => {
                                eprintln!("Error parsing DQT: {:?}", err);
                                break;
                            }
                        };
                        input = new_input;
                        println!("DQT {:?}", dqt);
                    }
                    Marker::DHT => {
                        let (new_input, dht) = match dht(input) {
                            Ok(result) => result,
                            Err(err) => {
                                eprintln!("Error parsing DHT: {:?}", err);
                                break;
                            }
                        };
                        input = new_input;
                        println!("DHT {:?}", dht);
                    }
                    Marker::SOF => {
                        let (new_input, sof) = match sof(input) {
                            Ok(result) => result,
                            Err(err) => {
                                eprintln!("Error parsing SOF: {:?}", err);
                                break;
                            }
                        };
                        input = new_input;
                        println!("SOF {:?}", sof);
                    }
                    Marker::DRI => {
                        let (new_input, dri) = match dri(input) {
                            Ok(result) => result,
                            Err(err) => {
                                eprintln!("Error parsing DRI: {:?}", err);
                                break;
                            }
                        };
                        input = new_input;
                        println!("DRI {:?}", dri);
                    }
                    Marker::SOS => {
                        let (new_input, sos) = match sos(input) {
                            Ok(result) => result,
                            Err(err) => {
                                eprintln!("Error parsing SOS: {:?}", err);
                                break;
                            }
                        };
                        input = new_input;
                        println!("SOS {:?}", sos);
                    }
                    Marker::RST(rst_number) => {
                        let (new_input, _) = rst(input)?;
                        input = new_input;
                        println!("RST {:?}", rst_number);
                    }
                    Marker::EOI => {
                        let (new_input, _) = eoi(input)?;
                        input = new_input;
                        println!("EOI");
                        break;
                    }
                }
            }
            Err(err) => {
                eprintln!("Error parsing marker: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}