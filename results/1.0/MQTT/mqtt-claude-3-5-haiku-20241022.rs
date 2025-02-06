use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{take},
    combinator::{map, verify},
    multi::count,
    number::complete::{le_u8, le_u16},
    sequence::preceded,
    IResult,
};

#[derive(Debug)]
enum MQTTPacket {
    Connect(ConnectPacket),
    ConnAck(ConnAckPacket),
    Publish(PublishPacket),
    PubAck(PubAckPacket),
    PubRec(PubRecPacket),
    PubRel(PubRelPacket),
    PubComp(PubCompPacket),
    Subscribe(SubscribePacket),
    SubAck(SubAckPacket),
    Unsubscribe(UnsubscribePacket),
    UnsubAck(UnsubAckPacket),
    PingReq,
    PingResp,
    Disconnect(DisconnectPacket),
    Auth(AuthPacket),
}

#[derive(Debug)]
struct Property {
    identifier: u8,
    value: Vec<u8>,
}

#[derive(Debug)]
struct ConnectPacket {
    protocol_name: String,
    protocol_version: u8,
    connect_flags: u8,
    keep_alive: u16,
    properties: Vec<Property>,
    client_id: String,
}

#[derive(Debug)]
struct ConnAckPacket {
    session_present: bool,
    reason_code: u8,
    properties: Vec<Property>,
}

#[derive(Debug)]
struct PublishPacket {
    topic_name: String,
    packet_id: Option<u16>,
    qos: u8,
    retain: bool,
    duplicate: bool,
    properties: Vec<Property>,
    payload: Vec<u8>,
}

#[derive(Debug)]
struct PubAckPacket {
    packet_id: u16,
    reason_code: u8,
    properties: Vec<Property>,
}

#[derive(Debug)]
struct PubRecPacket {
    packet_id: u16,
    reason_code: u8,
    properties: Vec<Property>,
}

#[derive(Debug)]
struct PubRelPacket {
    packet_id: u16,
    reason_code: u8,
    properties: Vec<Property>,
}

#[derive(Debug)]
struct PubCompPacket {
    packet_id: u16,
    reason_code: u8,
    properties: Vec<Property>,
}

#[derive(Debug)]
struct SubscribePacket {
    packet_id: u16,
    properties: Vec<Property>,
    topic_filters: Vec<(String, u8)>,
}

#[derive(Debug)]
struct SubAckPacket {
    packet_id: u16,
    reason_codes: Vec<u8>,
    properties: Vec<Property>,
}

#[derive(Debug)]
struct UnsubscribePacket {
    packet_id: u16,
    properties: Vec<Property>,
    topic_filters: Vec<String>,
}

#[derive(Debug)]
struct UnsubAckPacket {
    packet_id: u16,
    reason_codes: Vec<u8>,
    properties: Vec<Property>,
}

#[derive(Debug)]
struct DisconnectPacket {
    reason_code: u8,
    properties: Vec<Property>,
}

#[derive(Debug)]
struct AuthPacket {
    reason_code: u8,
    properties: Vec<Property>,
}

fn parse_variable_length_integer(mut input: &[u8]) -> IResult<&[u8], u32> {
    let mut multiplier = 1;
    let mut value = 0;
    let mut bytes_read = 0;

    loop {
        let (next_input, byte) = le_u8(input)?;
        input = next_input;
        value += ((byte & 0x7F) as u32) * multiplier;
        multiplier *= 128;
        bytes_read += 1;

        if byte & 0x80 == 0 || bytes_read > 4 {
            break;
        }
    }

    Ok((input, value))
}

fn parse_property(input: &[u8]) -> IResult<&[u8], Property> {
    let (input, identifier) = le_u8(input)?;
    let (input, value_length) = parse_variable_length_integer(input)?;
    let (input, value) = take(value_length as usize)(input)?;
    
    Ok((input, Property {
        identifier,
        value: value.to_vec(),
    }))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MQTTPacket> {
    let (input, first_byte) = le_u8(input)?;
    let packet_type = first_byte >> 4;
    let _flags = first_byte & 0x0F;

    let (input, _remaining_length) = parse_variable_length_integer(input)?;

    match packet_type {
        1 => parse_connect_packet(input),
        2 => parse_connack_packet(input),
        3 => parse_publish_packet(input),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], MQTTPacket> {
    let (input, protocol_bytes) = preceded(take(2_usize), take(4_usize))(input)?;
    let protocol_name = if protocol_bytes == b"MQTT" {
        String::from_utf8_lossy(protocol_bytes).into_owned()
    } else {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)));
    };

    let (input, protocol_version) = le_u8(input)?;
    let (input, connect_flags) = le_u8(input)?;
    let (input, keep_alive) = le_u16(input)?;

    let (input, properties_length) = parse_variable_length_integer(input)?;
    let (input, properties) = count(parse_property, properties_length as usize)(input)?;

    let (input, client_id_length) = le_u16(input)?;
    let (input, client_id) = map(take(client_id_length as usize), |bytes| String::from_utf8_lossy(bytes).into_owned())(input)?;

    Ok((input, MQTTPacket::Connect(ConnectPacket {
        protocol_name,
        protocol_version,
        connect_flags,
        keep_alive,
        properties,
        client_id,
    })))
}

fn parse_connack_packet(input: &[u8]) -> IResult<&[u8], MQTTPacket> {
    let (input, session_present) = map(le_u8, |byte| byte & 0x01 == 1)(input)?;
    let (input, reason_code) = le_u8(input)?;

    let (input, properties_length) = parse_variable_length_integer(input)?;
    let (input, properties) = count(parse_property, properties_length as usize)(input)?;

    Ok((input, MQTTPacket::ConnAck(ConnAckPacket {
        session_present,
        reason_code,
        properties,
    })))
}

fn parse_publish_packet(input: &[u8]) -> IResult<&[u8], MQTTPacket> {
    let (input, topic_name_length) = le_u16(input)?;
    let (input, topic_name) = map(take(topic_name_length as usize), |bytes| String::from_utf8_lossy(bytes).into_owned())(input)?;

    let (input, payload) = take(input.len())(input)?;

    Ok((input, MQTTPacket::Publish(PublishPacket {
        topic_name,
        packet_id: None,
        qos: 0,
        retain: false,
        duplicate: false,
        properties: Vec::new(),
        payload: payload.to_vec(),
    })))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
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
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}