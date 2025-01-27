use nom::{
    bits::{bits, complete::take},
    bytes::complete::{take as take_bytes, take_while},
    combinator::{map_res, opt},
    multi::many0,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
enum ControlPacketType {
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
    Auth,
}

#[derive(Debug)]
struct FixedHeader {
    packet_type: ControlPacketType,
    flags: u8,
    remaining_length: usize,
}

#[derive(Debug)]
struct ConnectPacket {
    protocol_name: String,
    protocol_level: u8,
    connect_flags: u8,
    keep_alive: u16,
    client_id: String,
    will_topic: Option<String>,
    will_message: Option<Vec<u8>>,
    username: Option<String>,
    password: Option<Vec<u8>>,
}

#[derive(Debug)]
enum MQTTPacket {
    Connect(ConnectPacket),
    Other,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse MQTT packet: {:?}", e),
    }
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MQTTPacket> {
    let (input, fixed_header) = parse_fixed_header(input)?;
    match fixed_header.packet_type {
        ControlPacketType::Connect => {
            let (input, connect_packet) = parse_connect_packet(input)?;
            Ok((input, MQTTPacket::Connect(connect_packet)))
        }
        _ => Ok((input, MQTTPacket::Other)),
    }
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (input, (packet_type, flags)) = bits::<_, _, nom::error::Error<_>, _, _>(|(input, size)| {
        let (input, packet_type) = take::<u8, _, _>(4usize)(input)?;
        let (input, flags) = take::<u8, _, _>(4usize)(input)?;
        Ok((input, (packet_type, flags)))
    })(input)?;
    let packet_type = match packet_type {
        1 => ControlPacketType::Connect,
        2 => ControlPacketType::Connack,
        3 => ControlPacketType::Publish,
        4 => ControlPacketType::Puback,
        5 => ControlPacketType::Pubrec,
        6 => ControlPacketType::Pubrel,
        7 => ControlPacketType::Pubcomp,
        8 => ControlPacketType::Subscribe,
        9 => ControlPacketType::Suback,
        10 => ControlPacketType::Unsubscribe,
        11 => ControlPacketType::Unsuback,
        12 => ControlPacketType::Pingreq,
        13 => ControlPacketType::Pingresp,
        14 => ControlPacketType::Disconnect,
        15 => ControlPacketType::Auth,
        _ => return Err(nom::Err::Failure(nom::error::Error::from_error_kind(input, nom::error::ErrorKind::Tag))),
    };
    let (input, remaining_length) = parse_variable_byte_integer(input)?;
    Ok((
        input,
        FixedHeader {
            packet_type,
            flags,
            remaining_length,
        },
    ))
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, protocol_name) = parse_utf8_encoded_string(input)?;
    let (input, protocol_level) = be_u8(input)?;
    let (input, connect_flags) = be_u8(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, client_id) = parse_utf8_encoded_string(input)?;
    let (input, will_topic) = if connect_flags & 0b0000_0100 > 0 {
        map_res(
            opt(parse_utf8_encoded_string),
            |wt: Option<_>| -> Result<_, nom::error::Error<_>> { Ok(wt) },
        )(input)?
    } else {
        (input, None)
    };
    let (input, will_message) = if connect_flags & 0b0000_0100 > 0 {
        map_res(
            opt(parse_binary_data),
            |wm: Option<_>| -> Result<_, nom::error::Error<_>> { Ok(wm) },
        )(input)?
    } else {
        (input, None)
    };
    let (input, username) = if connect_flags & 0b1000_0000 > 0 {
        map_res(
            opt(parse_utf8_encoded_string),
            |un: Option<_>| -> Result<_, nom::error::Error<_>> { Ok(un) },
        )(input)?
    } else {
        (input, None)
    };
    let (input, password) = if connect_flags & 0b0100_0000 > 0 {
        map_res(
            opt(parse_binary_data),
            |pw: Option<_>| -> Result<_, nom::error::Error<_>> { Ok(pw) },
        )(input)?
    } else {
        (input, None)
    };

    Ok((
        input,
        ConnectPacket {
            protocol_name,
            protocol_level,
            connect_flags,
            keep_alive,
            client_id,
            will_topic,
            will_message,
            username,
            password,
        },
    ))
}

fn parse_utf8_encoded_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, length) = be_u16(input)?;
    let (input, raw_bytes) = take_bytes(length as usize)(input)?;
    match String::from_utf8(raw_bytes.to_vec()) {
        Ok(s) => Ok((input, s)),
        Err(_) => Err(nom::Err::Error(nom::error::Error::from_error_kind(
            input,
            nom::error::ErrorKind::Fail,
        ))),
    }
}

fn parse_binary_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u16(input)?;
    let (input, data) = take_bytes(length as usize)(input)?;
    Ok((input, data.to_vec()))
}

fn parse_variable_byte_integer(input: &[u8]) -> IResult<&[u8], usize> {
    let mut value = 0usize;
    let mut multiplier = 1usize;
    let mut bytes_read = 0usize;
    let mut new_input = input;
    let mut finished = false;
    while !finished {
        let (input, byte) = be_u8(new_input)?;
        new_input = input;
        value += ((byte & 127) as usize) * multiplier;
        if byte & 128 == 0 {
            finished = true;
        }
        multiplier *= 128;
        bytes_read += 1;
        if bytes_read > 4 {
            return Err(nom::Err::Error(nom::error::Error::from_error_kind(
                input,
                nom::error::ErrorKind::TooLarge,
            )));
        }
    }
    Ok((new_input, value))
}