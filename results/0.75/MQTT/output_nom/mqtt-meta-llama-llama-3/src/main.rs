use nom::{
    bytes::complete::{take_while_m_n},
    combinator::{map, map_res},
    error::{ErrorKind},
    multi::{many0},
    number::complete::{be_u16, be_u8},
    sequence::{tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, Result},
    str,
};

#[derive(Debug, PartialEq)]
enum HeaderType {
    Connect,
    Connack,
    Publish,
    Puback,
    Pubrec,
    Pubrel,
    Pubcomp,
    Subscribe,
    Suback,
    Unsubscribe,
    Unsuback,
    Pingreq,
    Pingresp,
    Disconnect,
}

impl From<u8> for HeaderType {
    fn from(value: u8) -> Self {
        match value {
            0x01 => HeaderType::Connect,
            0x02 => HeaderType::Connack,
            0x03 => HeaderType::Publish,
            0x04 => HeaderType::Puback,
            0x05 => HeaderType::Pubrec,
            0x06 => HeaderType::Pubrel,
            0x07 => HeaderType::Pubcomp,
            0x08 => HeaderType::Subscribe,
            0x09 => HeaderType::Suback,
            0x0A => HeaderType::Unsubscribe,
            0x0B => HeaderType::Unsuback,
            0x0C => HeaderType::Pingreq,
            0x0D => HeaderType::Pingresp,
            0x0E => HeaderType::Disconnect,
            _ => panic!("Invalid header type"),
        }
    }
}

fn header_type(input: &[u8]) -> IResult<&[u8], HeaderType> {
    map_res(be_u8, |x| {
        let header_type = (x >> 4) & 0x0F;
        match header_type {
            0x01 => Ok(HeaderType::Connect),
            0x02 => Ok(HeaderType::Connack),
            0x03 => Ok(HeaderType::Publish),
            0x04 => Ok(HeaderType::Puback),
            0x05 => Ok(HeaderType::Pubrec),
            0x06 => Ok(HeaderType::Pubrel),
            0x07 => Ok(HeaderType::Pubcomp),
            0x08 => Ok(HeaderType::Subscribe),
            0x09 => Ok(HeaderType::Suback),
            0x0A => Ok(HeaderType::Unsubscribe),
            0x0B => Ok(HeaderType::Unsuback),
            0x0C => Ok(HeaderType::Pingreq),
            0x0D => Ok(HeaderType::Pingresp),
            0x0E => Ok(HeaderType::Disconnect),
            _ => Err(nom::Err::Error((input, ErrorKind::AlphaNumeric))),
        }
    })(input)
}

fn connect_flags(input: &[u8]) -> IResult<&[u8], (bool, bool, u8, bool, bool, bool)> {
    map(be_u8, |flags: u8| {
        let clean_start = (flags & 0x01) != 0;
        let will_flag = (flags & 0x02) != 0;
        let will_qos = (flags & 0x1C) >> 2;
        let will_retain = (flags & 0x20) != 0;
        let password_flag = (flags & 0x40) != 0;
        let username_flag = (flags & 0x80) != 0;
        (
            clean_start,
            will_flag,
            will_qos,
            will_retain,
            password_flag,
            username_flag,
        )
    })(input)
}

fn string(input: &[u8]) -> IResult<&[u8], String> {
    map(take_while_m_n(1, 1, |c| c != 0), |s: &[u8]| {
        str::from_utf8(s).unwrap().to_string()
    })(input)
}

fn connect(input: &[u8]) -> IResult<&[u8], (String, u8, (bool, bool, u8, bool, bool, bool), u16)> {
    tuple((
        string,
        be_u8,
        connect_flags,
        be_u16,
    ))(input)
}

fn connack(input: &[u8]) -> IResult<&[u8], (u8, u8)> {
    tuple((be_u8, be_u8))(input)
}

fn publish_flags(input: &[u8]) -> IResult<&[u8], (bool, u8, bool)> {
    map(be_u8, |flags: u8| {
        let dup = (flags & 0x08) != 0;
        let qos = (flags & 0x06) >> 1;
        let retain = (flags & 0x01) != 0;
        (dup, qos, retain)
    })(input)
}

fn publish(input: &[u8]) -> IResult<&[u8], ((bool, u8, bool), Vec<u8>, u16)> {
    tuple((publish_flags, map(take_while_m_n(1, 1, |c| c != 0), |x: &[u8]| x.to_vec()), be_u16))(input)
}

fn suback(input: &[u8]) -> IResult<&[u8], (u16, Vec<Vec<u8>>)> {
    tuple((be_u16, many0(map(take_while_m_n(1, 1, |c| c != 0), |x: &[u8]| x.to_vec()))))(input)
}

fn unsubscribe(input: &[u8]) -> IResult<&[u8], (u16, Vec<Vec<u8>>)> {
    tuple((be_u16, many0(map(take_while_m_n(1, 1, |c| c != 0), |x: &[u8]| x.to_vec()))))(input)
}

fn main() -> Result<()> {
    let mut file = File::open(env::args().nth(1).unwrap()).unwrap();
    let mut contents = Vec::new();
    file.read_to_end(&mut contents).unwrap();

    let input = contents.as_slice();
    let (input, header_type) = header_type(input).unwrap();

    match header_type {
        HeaderType::Connect => {
            let (_input, connect_data) = connect(input).unwrap();
            println!("Connect: {:?}", connect_data);
        }
        HeaderType::Connack => {
            let (_input, connack_data) = connack(input).unwrap();
            println!("Connack: {:?}", connack_data);
        }
        HeaderType::Publish => {
            let (_input, publish_data) = publish(input).unwrap();
            println!("Publish: {:?}", publish_data);
        }
        HeaderType::Suback => {
            let (_input, suback_data) = suback(input).unwrap();
            println!("Suback: {:?}", suback_data);
        }
        HeaderType::Unsubscribe => {
            let (_input, unsubscribe_data) = unsubscribe(input).unwrap();
            println!("Unsubscribe: {:?}", unsubscribe_data);
        }
        _ => {}
    }

    Ok(())
}