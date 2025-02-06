use nom::{
    number::streaming::{be_u8, be_u16, be_u32},
    combinator::{map},
    error::{ErrorKind},
    sequence::{tuple},
    IResult, Err,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone, PartialEq)]
enum PacketType {
    Connect,
    ConnAck,
    Publish,
    PubAck,
    PubRec,
    PubRel,
    PubComp,
    Subscribe,
    SubAck,
    Unsubscribe,
    UnsubAck,
    PingReq,
    PingResp,
    Disconnect,
    Auth,
}

#[derive(Debug, Clone)]
struct MqttPacket {
    packet_type: PacketType,
    flags: u8,
    remaining_length: usize,
    payload: Vec<u8>,
}

#[derive(Debug, Clone)]
struct ConnectPacket {
    protocol_name: String,
    protocol_version: u8,
    connect_flags: u8,
    keep_alive: u16,
    properties: Vec<Property>,
    client_id: String,
    will_topic: Option<String>,
    will_message: Option<Vec<u8>>,
    username: Option<String>,
    password: Option<String>,
}

#[derive(Debug, Clone)]
enum Property {
    SessionExpiryInterval(u32),
    ReceiveMaximum(u16),
    MaximumPacketSize(u32),
    TopicAliasMaximum(u16),
    UserProperty(String, String),
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, first_byte) = be_u8(input)?;
    let packet_type = match first_byte >> 4 {
        1 => PacketType::Connect,
        2 => PacketType::ConnAck,
        3 => PacketType::Publish,
        4 => PacketType::PubAck,
        5 => PacketType::PubRec,
        6 => PacketType::PubRel,
        7 => PacketType::PubComp,
        8 => PacketType::Subscribe,
        9 => PacketType::SubAck,
        10 => PacketType::Unsubscribe,
        11 => PacketType::UnsubAck,
        12 => PacketType::PingReq,
        13 => PacketType::PingResp,
        14 => PacketType::Disconnect,
        15 => PacketType::Auth,
        _ => return Err(Err::Error(nom::error::Error::new(input, ErrorKind::Fail))),
    };

    let flags = first_byte & 0x0F;

    let (input, remaining_length) = parse_variable_length(input)?;

    let (input, payload) = nom::bytes::streaming::take(remaining_length)(input)?;

    Ok((input, MqttPacket {
        packet_type,
        flags,
        remaining_length,
        payload: payload.to_vec(),
    }))
}

fn parse_variable_length(input: &[u8]) -> IResult<&[u8], usize> {
    let mut multiplier = 1;
    let mut value = 0;
    let mut bytes_read = 0;

    for (index, &byte) in input.iter().enumerate() {
        value += (byte & 0x7F) as usize * multiplier;
        multiplier *= 0x80;
        bytes_read += 1;

        if byte & 0x80 == 0 {
            return Ok((&input[bytes_read..], value));
        }

        if index >= 4 {
            return Err(Err::Error(nom::error::Error::new(input, ErrorKind::TooLarge)));
        }
    }

    Err(Err::Incomplete(nom::Needed::new(1)))
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, protocol_name) = parse_utf8_string(input)?;
    let (input, protocol_version) = be_u8(input)?;
    let (input, connect_flags) = be_u8(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, properties) = parse_properties(input)?;
    let (input, client_id) = parse_utf8_string(input)?;

    let will_topic = if connect_flags & 0x04 != 0 {
        let (_input, topic) = parse_utf8_string(input)?;
        Some(topic)
    } else {
        None
    };

    let will_message = if connect_flags & 0x04 != 0 {
        let (_input, message) = parse_binary_data(input)?;
        Some(message)
    } else {
        None
    };

    let username = if connect_flags & 0x80 != 0 {
        let (_input, user) = parse_utf8_string(input)?;
        Some(user)
    } else {
        None
    };

    let password = if connect_flags & 0x40 != 0 {
        let (_input, pass) = parse_utf8_string(input)?;
        Some(pass)
    } else {
        None
    };

    Ok((input, ConnectPacket {
        protocol_name,
        protocol_version,
        connect_flags,
        keep_alive,
        properties,
        client_id,
        will_topic,
        will_message,
        username,
        password,
    }))
}

fn parse_utf8_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, length) = be_u16(input)?;
    let (input, string_bytes) = nom::bytes::streaming::take(length)(input)?;
    let string = String::from_utf8_lossy(string_bytes).into_owned();
    Ok((input, string))
}

fn parse_binary_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u16(input)?;
    let (input, data) = nom::bytes::streaming::take(length)(input)?;
    Ok((input, data.to_vec()))
}

fn parse_properties(input: &[u8]) -> IResult<&[u8], Vec<Property>> {
    let (input, length) = parse_variable_length(input)?;
    let (input, properties) = nom::bytes::streaming::take(length)(input)?;
    let mut parsed_properties = Vec::new();

    let mut prop_input = properties;
    while !prop_input.is_empty() {
        let (remaining, property) = parse_single_property(prop_input)?;
        parsed_properties.push(property);
        prop_input = remaining;
    }

    Ok((input, parsed_properties))
}

fn parse_single_property(input: &[u8]) -> IResult<&[u8], Property> {
    let (input, prop_id) = be_u8(input)?;
    match prop_id {
        17 => {
            let (input, value) = be_u32(input)?;
            Ok((input, Property::SessionExpiryInterval(value)))
        },
        33 => {
            let (input, value) = be_u16(input)?;
            Ok((input, Property::ReceiveMaximum(value)))
        },
        39 => {
            let (input, value) = be_u32(input)?;
            Ok((input, Property::MaximumPacketSize(value)))
        },
        34 => {
            let (input, value) = be_u16(input)?;
            Ok((input, Property::TopicAliasMaximum(value)))
        },
        38 => {
            let (input, key) = parse_utf8_string(input)?;
            let (input, value) = parse_utf8_string(input)?;
            Ok((input, Property::UserProperty(key, value)))
        },
        _ => Err(Err::Error(nom::error::Error::new(input, ErrorKind::Fail))),
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <mqtt_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => {
            println!("Parsed MQTT Packet: {:?}", packet);
            if packet.packet_type == PacketType::Connect {
                match parse_connect_packet(&packet.payload) {
                    Ok((_, connect_packet)) => {
                        println!("Connect Packet Details: {:?}", connect_packet);
                    }
                    Err(e) => eprintln!("Failed to parse Connect Packet: {:?}", e),
                }
            }
        }
        Err(e) => eprintln!("Failed to parse MQTT Packet: {:?}", e),
    }

    Ok(())
}