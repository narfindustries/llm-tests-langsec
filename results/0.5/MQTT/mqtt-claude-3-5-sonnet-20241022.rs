use nom::{
    bits::complete::take as take_bits,
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::length_data,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
enum MqttPacketType {
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
}

#[derive(Debug)]
struct FixedHeader {
    packet_type: MqttPacketType,
    dup_flag: bool,
    qos_level: u8,
    retain: bool,
    remaining_length: u32,
}

#[derive(Debug)]
struct ConnectFlags {
    username_flag: bool,
    password_flag: bool,
    will_retain: bool,
    will_qos: u8,
    will_flag: bool,
    clean_session: bool,
}

#[derive(Debug)]
struct ConnectPacket {
    protocol_name: String,
    protocol_level: u8,
    connect_flags: ConnectFlags,
    keep_alive: u16,
    client_id: String,
    will_topic: Option<String>,
    will_message: Option<Vec<u8>>,
    username: Option<String>,
    password: Option<Vec<u8>>,
}

#[derive(Debug)]
struct PublishPacket {
    topic_name: String,
    packet_identifier: Option<u16>,
    payload: Vec<u8>,
}

#[derive(Debug)]
enum MqttPacket {
    Connect(ConnectPacket),
    Publish(PublishPacket),
    // Add other packet types as needed
}

fn parse_packet_type(input: &[u8]) -> IResult<&[u8], (MqttPacketType, bool, u8, bool)> {
    let (input, byte) = be_u8(input)?;
    let packet_type = match byte >> 4 {
        1 => MqttPacketType::Connect,
        2 => MqttPacketType::ConnAck,
        3 => MqttPacketType::Publish,
        4 => MqttPacketType::PubAck,
        5 => MqttPacketType::PubRec,
        6 => MqttPacketType::PubRel,
        7 => MqttPacketType::PubComp,
        8 => MqttPacketType::Subscribe,
        9 => MqttPacketType::SubAck,
        10 => MqttPacketType::Unsubscribe,
        11 => MqttPacketType::UnsubAck,
        12 => MqttPacketType::PingReq,
        13 => MqttPacketType::PingResp,
        14 => MqttPacketType::Disconnect,
        _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    };
    let dup_flag = (byte & 0b00001000) != 0;
    let qos_level = (byte & 0b00000110) >> 1;
    let retain = (byte & 0b00000001) != 0;
    Ok((input, (packet_type, dup_flag, qos_level, retain)))
}

fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], u32> {
    let mut multiplier = 1;
    let mut value = 0u32;
    let mut current_input = input;

    loop {
        let (next_input, byte) = be_u8(current_input)?;
        value += ((byte & 0x7F) as u32) * multiplier;
        multiplier *= 128;
        current_input = next_input;

        if byte & 0x80 == 0 {
            break;
        }
        if multiplier > 128 * 128 * 128 {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::TooLarge,
            )));
        }
    }

    Ok((current_input, value))
}

fn parse_mqtt_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, length) = be_u16(input)?;
    let (input, string_bytes) = take(length)(input)?;
    Ok((
        input,
        String::from_utf8(string_bytes.to_vec()).unwrap_or_default(),
    ))
}

fn parse_connect_flags(input: &[u8]) -> IResult<&[u8], ConnectFlags> {
    let (input, flags_byte) = be_u8(input)?;
    Ok((
        input,
        ConnectFlags {
            username_flag: (flags_byte & 0x80) != 0,
            password_flag: (flags_byte & 0x40) != 0,
            will_retain: (flags_byte & 0x20) != 0,
            will_qos: (flags_byte & 0x18) >> 3,
            will_flag: (flags_byte & 0x04) != 0,
            clean_session: (flags_byte & 0x02) != 0,
        },
    ))
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, protocol_name) = parse_mqtt_string(input)?;
    let (input, protocol_level) = be_u8(input)?;
    let (input, connect_flags) = parse_connect_flags(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, client_id) = parse_mqtt_string(input)?;

    let (input, will_topic, will_message) = if connect_flags.will_flag {
        let (input, topic) = parse_mqtt_string(input)?;
        let (input, length) = be_u16(input)?;
        let (input, message) = take(length)(input)?;
        (input, Some(topic), Some(message.to_vec()))
    } else {
        (input, None, None)
    };

    let (input, username) = if connect_flags.username_flag {
        let (input, username) = parse_mqtt_string(input)?;
        (input, Some(username))
    } else {
        (input, None)
    };

    let (input, password) = if connect_flags.password_flag {
        let (input, length) = be_u16(input)?;
        let (input, password) = take(length)(input)?;
        (input, Some(password.to_vec()))
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

fn parse_publish_packet(
    dup: bool,
    qos: u8,
    retain: bool,
    remaining_length: u32,
    input: &[u8],
) -> IResult<&[u8], PublishPacket> {
    let (input, topic_name) = parse_mqtt_string(input)?;
    
    let (input, packet_identifier) = if qos > 0 {
        let (input, id) = be_u16(input)?;
        (input, Some(id))
    } else {
        (input, None)
    };

    let payload_length = remaining_length as usize
        - (2 + topic_name.len())
        - if qos > 0 { 2 } else { 0 };
    let (input, payload) = take(payload_length)(input)?;

    Ok((
        input,
        PublishPacket {
            topic_name,
            packet_identifier,
            payload: payload.to_vec(),
        },
    ))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (packet_type, dup_flag, qos_level, retain)) = parse_packet_type(input)?;
    let (input, remaining_length) = parse_remaining_length(input)?;

    match packet_type {
        MqttPacketType::Connect => {
            let (input, connect_packet) = parse_connect_packet(input)?;
            Ok((input, MqttPacket::Connect(connect_packet)))
        }
        MqttPacketType::Publish => {
            let (input, publish_packet) =
                parse_publish_packet(dup_flag, qos_level, retain, remaining_length, input)?;
            Ok((input, MqttPacket::Publish(publish_packet)))
        }
        // Add other packet types here
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed packet: {:?}", packet);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Error parsing MQTT packet: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}