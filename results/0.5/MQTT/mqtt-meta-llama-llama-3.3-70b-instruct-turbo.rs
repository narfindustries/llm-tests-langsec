use nom::{
    bytes::complete::{take, take_while},
    number::complete::{be_u16},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};
use std::convert::TryFrom;

#[derive(Debug, PartialEq)]
enum QoS {
    AtMostOnce,
    AtLeastOnce,
    ExactlyOnce,
}

impl TryFrom<u8> for QoS {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(QoS::AtMostOnce),
            1 => Ok(QoS::AtLeastOnce),
            2 => Ok(QoS::ExactlyOnce),
            _ => Err(format!("Invalid QoS value: {}", value)),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ConnectReturnCode {
    ConnectionAccepted,
    UnacceptableProtocolVersion,
    IdentifierRejected,
    ServerUnavailable,
    BadUsernameOrPassword,
    NotAuthorized,
}

impl TryFrom<u8> for ConnectReturnCode {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(ConnectReturnCode::ConnectionAccepted),
            1 => Ok(ConnectReturnCode::UnacceptableProtocolVersion),
            2 => Ok(ConnectReturnCode::IdentifierRejected),
            3 => Ok(ConnectReturnCode::ServerUnavailable),
            4 => Ok(ConnectReturnCode::BadUsernameOrPassword),
            5 => Ok(ConnectReturnCode::NotAuthorized),
            _ => Err(format!("Invalid Connect Return Code value: {}", value)),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ReasonCode {
    Success,
    NormalDisconnection,
    DisconnectWithWill,
    ContinueAuthentication,
    ReAuthenticate,
}

impl TryFrom<u8> for ReasonCode {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(ReasonCode::Success),
            1 => Ok(ReasonCode::NormalDisconnection),
            2 => Ok(ReasonCode::DisconnectWithWill),
            18 => Ok(ReasonCode::ContinueAuthentication),
            19 => Ok(ReasonCode::ReAuthenticate),
            _ => Err(format!("Invalid Reason Code value: {}", value)),
        }
    }
}

fn variable_integer_length(input: &[u8]) -> IResult<&[u8], u32> {
    let (input, first_byte) = take(1u8)(input)?;
    let mut multiplier = 1;
    let mut value = 0;
    let mut remaining = 1;
    while remaining > 0 {
        value += ((first_byte[remaining - 1] & 0x7F) as u32) * multiplier;
        if first_byte[remaining - 1] & 0x80 == 0 {
            break;
        }
        multiplier *= 128;
        remaining += 1;
        if remaining > input.len() {
            return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Eof)));
        }
    }
    Ok((&input[remaining..], value))
}

fn mqtt_fixed_header(input: &[u8]) -> IResult<&[u8], (u8, QoS, bool, bool, bool)> {
    let (input, first_byte) = take(1u8)(input)?;
    let qos = ((first_byte[0] >> 1) & 0x03).try_into().unwrap();
    let dup = ((first_byte[0] >> 3) & 0x01) != 0;
    let retain = ((first_byte[0] >> 0) & 0x01) != 0;
    let packet_type = first_byte[0] & 0x0F;
    Ok((input, (packet_type, qos, dup, retain, packet_type == 1)))
}

fn mqtt_connect_flags(input: &[u8]) -> IResult<&[u8], (bool, bool, QoS, bool, bool, bool)> {
    let (input, flags) = take(1u8)(input)?;
    let clean_start = (flags[0] & 0x02) != 0;
    let will_flag = (flags[0] & 0x04) != 0;
    let will_qos = ((flags[0] & 0x18) >> 3).try_into().unwrap();
    let will_retain = (flags[0] & 0x20) != 0;
    let password_flag = (flags[0] & 0x40) != 0;
    let username_flag = (flags[0] & 0x80) != 0;
    Ok((input, (clean_start, will_flag, will_qos, will_retain, password_flag, username_flag)))
}

fn mqtt_connect(input: &[u8]) -> IResult<&[u8], (String, u8, (bool, bool, QoS, bool, bool, bool), u16)> {
    let (input, protocol_name) = take_while(|c| c != 0)(input)?;
    let protocol_name = String::from_utf8_lossy(protocol_name).into_owned();
    let (input, protocol_version) = take(1u8)(input)?;
    let (input, (clean_start, will_flag, will_qos, will_retain, password_flag, username_flag)) =
        mqtt_connect_flags(input)?;
    let (input, keep_alive) = be_u16(input)?;
    Ok((input, (protocol_name, protocol_version[0], (clean_start, will_flag, will_qos, will_retain, password_flag, username_flag), keep_alive)))
}

fn mqtt_publish(input: &[u8]) -> IResult<&[u8], (String, Option<u16>, QoS, bool, bool)> {
    let (input, topic_name) = take_while(|c| c != 0)(input)?;
    let topic_name = String::from_utf8_lossy(topic_name).into_owned();
    let (input, packet_identifier) = be_u16(input)?;
    let qos = QoS::try_from(0).unwrap();
    let dup = false;
    let retain = false;
    Ok((input, (topic_name, Some(packet_identifier), qos, dup, retain)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (input, _) = mqtt_fixed_header(&input).unwrap();
    let (input, (protocol_name, protocol_version, (clean_start, will_flag, will_qos, will_retain, password_flag, username_flag), keep_alive)) =
        mqtt_connect(input).unwrap();
    println!("Protocol Name: {}", protocol_name);
    println!("Protocol Version: {}", protocol_version);
    println!("Clean Start: {}", clean_start);
    println!("Will Flag: {}", will_flag);
    println!("Will QoS: {:?}", will_qos);
    println!("Will Retain: {}", will_retain);
    println!("Password Flag: {}", password_flag);
    println!("Username Flag: {}", username_flag);
    println!("Keep Alive: {}", keep_alive);
    let (_input, (topic_name, packet_identifier, qos, dup, retain)) = mqtt_publish(input).unwrap();
    println!("Topic Name: {}", topic_name);
    println!("Packet Identifier: {:?}", packet_identifier);
    println!("QoS: {:?}", qos);
    println!("Dup: {}", dup);
    println!("Retain: {}", retain);
}