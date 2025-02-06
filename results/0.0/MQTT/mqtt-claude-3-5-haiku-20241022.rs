use nom::{
    bytes::streaming::{take, tag},
    combinator::{map, opt},
    error::{ErrorKind, ParseError},
    multi::many0,
    number::streaming::{be_u8, be_u16, be_u32},
    IResult, Err,
};
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone)]
enum MqttPacketType {
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

#[derive(Debug, Clone)]
struct ConnectPacket {
    protocol_name: String,
    protocol_level: u8,
    connect_flags: ConnectFlags,
    keep_alive: u16,
    properties: Option<ConnectProperties>,
    client_id: String,
    will_properties: Option<WillProperties>,
    will_topic: Option<String>,
    will_payload: Option<Vec<u8>>,
    username: Option<String>,
    password: Option<String>,
}

#[derive(Debug, Clone)]
struct PublishPacket {
    topic_name: String,
    packet_identifier: Option<u16>,
    properties: Option<PublishProperties>,
    payload: Vec<u8>,
}

#[derive(Debug, Clone)]
struct PubAckPacket {
    packet_identifier: u16,
    reason_code: u8,
    properties: Option<PubAckProperties>,
}

#[derive(Debug, Clone)]
struct PubRecPacket {
    packet_identifier: u16,
    reason_code: u8,
    properties: Option<PubRecProperties>,
}

#[derive(Debug, Clone)]
struct PubRelPacket {
    packet_identifier: u16,
    reason_code: u8,
    properties: Option<PubRelProperties>,
}

#[derive(Debug, Clone)]
struct PubCompPacket {
    packet_identifier: u16,
    reason_code: u8,
    properties: Option<PubCompProperties>,
}

#[derive(Debug, Clone)]
struct SubscribePacket {
    packet_identifier: u16,
    properties: Option<SubscribeProperties>,
    topic_filters: Vec<(String, u8)>,
}

#[derive(Debug, Clone)]
struct SubAckPacket {
    packet_identifier: u16,
    reason_codes: Vec<u8>,
    properties: Option<SubAckProperties>,
}

#[derive(Debug, Clone)]
struct UnsubscribePacket {
    packet_identifier: u16,
    properties: Option<UnsubscribeProperties>,
    topic_filters: Vec<String>,
}

#[derive(Debug, Clone)]
struct UnsubAckPacket {
    packet_identifier: u16,
    reason_codes: Vec<u8>,
    properties: Option<UnsubAckProperties>,
}

#[derive(Debug, Clone)]
struct DisconnectPacket {
    reason_code: u8,
    properties: Option<DisconnectProperties>,
}

#[derive(Debug, Clone)]
struct AuthPacket {
    reason_code: u8,
    properties: Option<AuthProperties>,
}

#[derive(Debug, Clone)]
struct ConnectFlags {
    username_flag: bool,
    password_flag: bool,
    will_retain: bool,
    will_qos: u8,
    will_flag: bool,
    clean_start: bool,
}

#[derive(Debug, Clone)]
struct ConnectProperties {
    session_expiry_interval: Option<u32>,
    authentication_method: Option<String>,
    authentication_data: Option<Vec<u8>>,
    request_problem_information: Option<u8>,
    request_response_information: Option<u8>,
    receive_maximum: Option<u16>,
    topic_alias_maximum: Option<u16>,
    maximum_packet_size: Option<u32>,
    user_properties: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
struct WillProperties {
    will_delay_interval: Option<u32>,
    payload_format_indicator: Option<u8>,
    message_expiry_interval: Option<u32>,
    content_type: Option<String>,
    response_topic: Option<String>,
    correlation_data: Option<Vec<u8>>,
    user_properties: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
struct ConnAckPacket {
    connect_acknowledge_flags: u8,
    reason_code: u8,
    properties: Option<ConnAckProperties>,
}

#[derive(Debug, Clone)]
struct PublishProperties {
    payload_format_indicator: Option<u8>,
    message_expiry_interval: Option<u32>,
    topic_alias: Option<u16>,
    response_topic: Option<String>,
    correlation_data: Option<Vec<u8>>,
    user_properties: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
struct PubAckProperties {
    reason_string: Option<String>,
    user_properties: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
struct PubRecProperties {
    reason_string: Option<String>,
    user_properties: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
struct PubRelProperties {
    reason_string: Option<String>,
    user_properties: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
struct PubCompProperties {
    reason_string: Option<String>,
    user_properties: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
struct SubscribeProperties {
    subscription_identifier: Option<u32>,
    user_properties: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
struct SubAckProperties {
    reason_string: Option<String>,
    user_properties: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
struct UnsubscribeProperties {
    user_properties: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
struct UnsubAckProperties {
    reason_string: Option<String>,
    user_properties: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
struct ConnAckProperties {
    session_expiry_interval: Option<u32>,
    assigned_client_identifier: Option<String>,
    server_keep_alive: Option<u16>,
    authentication_method: Option<String>,
    authentication_data: Option<Vec<u8>>,
    response_information: Option<String>,
    server_reference: Option<String>,
    reason_string: Option<String>,
    user_properties: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
struct DisconnectProperties {
    session_expiry_interval: Option<u32>,
    reason_string: Option<String>,
    server_reference: Option<String>,
    user_properties: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
struct AuthProperties {
    authentication_method: Option<String>,
    authentication_data: Option<Vec<u8>>,
    reason_string: Option<String>,
    user_properties: Option<HashMap<String, String>>,
}

fn parse_variable_byte_integer(input: &[u8]) -> IResult<&[u8], u32> {
    let mut multiplier = 1;
    let mut value = 0;
    let mut bytes_read = 0;

    loop {
        let (remaining, byte) = be_u8(input)?;
        value += (byte & 0x7F) as u32 * multiplier;
        multiplier *= 128;
        bytes_read += 1;

        if byte & 0x80 == 0 || bytes_read > 4 {
            return Ok((remaining, value));
        }
    }
}

fn parse_utf8_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, length) = be_u16(input)?;
    let (input, string_bytes) = take(length)(input)?;
    
    match String::from_utf8(string_bytes.to_vec()) {
        Ok(s) => Ok((input, s)),
        Err(_) => Err(Err::Error(ParseError::from_error_kind(input, ErrorKind::Verify))),
    }
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, protocol_name) = parse_utf8_string(input)?;
    let (input, protocol_level) = be_u8(input)?;
    let (input, connect_flags_byte) = be_u8(input)?;
    let connect_flags = ConnectFlags {
        username_flag: (connect_flags_byte & 0x80) != 0,
        password_flag: (connect_flags_byte & 0x40) != 0,
        will_retain: (connect_flags_byte & 0x20) != 0,
        will_qos: (connect_flags_byte & 0x18) >> 3,
        will_flag: (connect_flags_byte & 0x04) != 0,
        clean_start: (connect_flags_byte & 0x02) != 0,
    };
    let (input, keep_alive) = be_u16(input)?;
    
    Ok((input, ConnectPacket {
        protocol_name,
        protocol_level,
        connect_flags,
        keep_alive,
        properties: None,
        client_id: String::new(),
        will_properties: None,
        will_topic: None,
        will_payload: None,
        username: None,
        password: None,
    }))
}

fn parse_publish_packet(input: &[u8]) -> IResult<&[u8], PublishPacket> {
    let (input, topic_name) = parse_utf8_string(input)?;
    
    Ok((input, PublishPacket {
        topic_name,
        packet_identifier: None,
        properties: None,
        payload: Vec::new(),
    }))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacketType> {
    let (input, first_byte) = be_u8(input)?;
    let packet_type = first_byte >> 4;
    let _flags = first_byte & 0x0F;

    let (input, _length) = parse_variable_byte_integer(input)?;

    match packet_type {
        1 => map(parse_connect_packet, MqttPacketType::Connect)(input),
        2 => map(|i| Ok((i, ConnAckPacket { 
            connect_acknowledge_flags: 0, 
            reason_code: 0, 
            properties: None 
        })), MqttPacketType::ConnAck)(input),
        3 => map(parse_publish_packet, MqttPacketType::Publish)(input),
        14 => map(|i| Ok((i, DisconnectPacket { 
            reason_code: 0, 
            properties: None 
        })), MqttPacketType::Disconnect)(input),
        _ => Err(Err::Error(ParseError::from_error_kind(input, ErrorKind::Alt))),
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
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}