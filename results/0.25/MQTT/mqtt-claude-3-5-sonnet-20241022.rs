use nom::{
    bits::complete::take as take_bits,
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::{length_data, many0},
    number::complete::{be_u16, be_u8},
    sequence::{pair, preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub enum MqttPacket {
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
    Disconnect,
}

#[derive(Debug)]
pub struct ConnectPacket {
    protocol_name: String,
    protocol_level: u8,
    clean_session: bool,
    will_flag: bool,
    will_qos: u8,
    will_retain: bool,
    username_flag: bool,
    password_flag: bool,
    keep_alive: u16,
    client_id: String,
    will_topic: Option<String>,
    will_message: Option<Vec<u8>>,
    username: Option<String>,
    password: Option<Vec<u8>>,
}

#[derive(Debug)]
pub struct ConnAckPacket {
    session_present: bool,
    return_code: u8,
}

#[derive(Debug)]
pub struct PublishPacket {
    dup: bool,
    qos: u8,
    retain: bool,
    topic_name: String,
    packet_id: Option<u16>,
    payload: Vec<u8>,
}

#[derive(Debug)]
pub struct PubAckPacket {
    packet_id: u16,
}

#[derive(Debug)]
pub struct PubRecPacket {
    packet_id: u16,
}

#[derive(Debug)]
pub struct PubRelPacket {
    packet_id: u16,
}

#[derive(Debug)]
pub struct PubCompPacket {
    packet_id: u16,
}

#[derive(Debug)]
pub struct SubscribePacket {
    packet_id: u16,
    topics: Vec<(String, u8)>,
}

#[derive(Debug)]
pub struct SubAckPacket {
    packet_id: u16,
    return_codes: Vec<u8>,
}

#[derive(Debug)]
pub struct UnsubscribePacket {
    packet_id: u16,
    topics: Vec<String>,
}

#[derive(Debug)]
pub struct UnsubAckPacket {
    packet_id: u16,
}

fn parse_mqtt_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, len) = be_u16(input)?;
    let (input, string_bytes) = take(len)(input)?;
    Ok((
        input,
        String::from_utf8(string_bytes.to_vec()).unwrap_or_default(),
    ))
}

fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], usize> {
    let mut multiplier = 1;
    let mut value = 0;
    let mut current_input = input;
    let mut bytes_read = 0;

    loop {
        let (new_input, byte) = be_u8(current_input)?;
        current_input = new_input;
        bytes_read += 1;

        value += (byte & 0x7F) as usize * multiplier;
        multiplier *= 128;

        if byte & 0x80 == 0 {
            break;
        }

        if bytes_read >= 4 {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::TooLarge,
            )));
        }
    }

    Ok((&input[bytes_read..], value))
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], (u8, u8, usize)> {
    let (input, first_byte) = be_u8(input)?;
    let packet_type = first_byte >> 4;
    let flags = first_byte & 0x0F;
    let (input, remaining_length) = parse_remaining_length(input)?;
    Ok((input, (packet_type, flags, remaining_length)))
}

fn parse_connect(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, protocol_name) = parse_mqtt_string(input)?;
    let (input, protocol_level) = be_u8(input)?;
    let (input, connect_flags) = be_u8(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, client_id) = parse_mqtt_string(input)?;

    let username_flag = (connect_flags & 0x80) != 0;
    let password_flag = (connect_flags & 0x40) != 0;
    let will_retain = (connect_flags & 0x20) != 0;
    let will_qos = (connect_flags & 0x18) >> 3;
    let will_flag = (connect_flags & 0x04) != 0;
    let clean_session = (connect_flags & 0x02) != 0;

    let mut current_input = input;
    let mut will_topic = None;
    let mut will_message = None;
    let mut username = None;
    let mut password = None;

    if will_flag {
        let (new_input, topic) = parse_mqtt_string(current_input)?;
        current_input = new_input;
        will_topic = Some(topic);

        let (new_input, message_len) = be_u16(current_input)?;
        let (new_input, message) = take(message_len)(new_input)?;
        current_input = new_input;
        will_message = Some(message.to_vec());
    }

    if username_flag {
        let (new_input, user) = parse_mqtt_string(current_input)?;
        current_input = new_input;
        username = Some(user);
    }

    if password_flag {
        let (new_input, pass_len) = be_u16(current_input)?;
        let (new_input, pass) = take(pass_len)(new_input)?;
        current_input = new_input;
        password = Some(pass.to_vec());
    }

    Ok((
        current_input,
        ConnectPacket {
            protocol_name,
            protocol_level,
            clean_session,
            will_flag,
            will_qos,
            will_retain,
            username_flag,
            password_flag,
            keep_alive,
            client_id,
            will_topic,
            will_message,
            username,
            password,
        },
    ))
}

fn parse_connack(input: &[u8]) -> IResult<&[u8], ConnAckPacket> {
    let (input, flags) = be_u8(input)?;
    let (input, return_code) = be_u8(input)?;
    Ok((
        input,
        ConnAckPacket {
            session_present: (flags & 0x01) != 0,
            return_code,
        },
    ))
}

fn parse_publish(input: &[u8], flags: u8, remaining_length: usize) -> IResult<&[u8], PublishPacket> {
    let dup = (flags & 0x08) != 0;
    let qos = (flags & 0x06) >> 1;
    let retain = (flags & 0x01) != 0;

    let (input, topic_name) = parse_mqtt_string(input)?;
    
    let (input, packet_id) = if qos > 0 {
        let (input, id) = be_u16(input)?;
        (input, Some(id))
    } else {
        (input, None)
    };

    let payload_length = remaining_length
        - (2 + topic_name.len() + if qos > 0 { 2 } else { 0 });
    let (input, payload) = take(payload_length)(input)?;

    Ok((
        input,
        PublishPacket {
            dup,
            qos,
            retain,
            topic_name,
            packet_id,
            payload: payload.to_vec(),
        },
    ))
}

fn parse_packet_id(input: &[u8]) -> IResult<&[u8], u16> {
    be_u16(input)
}

fn parse_subscribe(input: &[u8]) -> IResult<&[u8], SubscribePacket> {
    let (input, packet_id) = be_u16(input)?;
    let mut topics = Vec::new();
    let mut current_input = input;

    while !current_input.is_empty() {
        let (new_input, topic) = parse_mqtt_string(current_input)?;
        let (new_input, qos) = be_u8(new_input)?;
        topics.push((topic, qos));
        current_input = new_input;
    }

    Ok((input, SubscribePacket { packet_id, topics }))
}

fn parse_suback(input: &[u8]) -> IResult<&[u8], SubAckPacket> {
    let (input, packet_id) = be_u16(input)?;
    let (input, return_codes) = many0(be_u8)(input)?;
    Ok((
        input,
        SubAckPacket {
            packet_id,
            return_codes,
        },
    ))
}

fn parse_unsubscribe(input: &[u8]) -> IResult<&[u8], UnsubscribePacket> {
    let (input, packet_id) = be_u16(input)?;
    let mut topics = Vec::new();
    let mut current_input = input;

    while !current_input.is_empty() {
        let (new_input, topic) = parse_mqtt_string(current_input)?;
        topics.push(topic);
        current_input = new_input;
    }

    Ok((input, UnsubscribePacket { packet_id, topics }))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (packet_type, flags, remaining_length)) = parse_fixed_header(input)?;
    let (input, packet) = match packet_type {
        1 => map(parse_connect, MqttPacket::Connect)(input)?,
        2 => map(parse_connack, MqttPacket::ConnAck)(input)?,
        3 => map(|input| parse_publish(input, flags, remaining_length), MqttPacket::Publish)(input)?,
        4 => map(parse_packet_id, |id| MqttPacket::PubAck(PubAckPacket { packet_id: id }))(input)?,
        5 => map(parse_packet_id, |id| MqttPacket::PubRec(PubRecPacket { packet_id: id }))(input)?,
        6 => map(parse_packet_id, |id| MqttPacket::PubRel(PubRelPacket { packet_id: id }))(input)?,
        7 => map(parse_packet_id, |id| MqttPacket::PubComp(PubCompPacket { packet_id: id }))(input)?,
        8 => map(parse_subscribe, MqttPacket::Subscribe)(input)?,
        9 => map(parse_suback, MqttPacket::SubAck)(input)?,
        10 => map(parse_unsubscribe, MqttPacket::Unsubscribe)(input)?,
        11 => map(parse_packet_id, |id| MqttPacket::UnsubAck(UnsubAckPacket { packet_id: id }))(input)?,
        12 => (input, MqttPacket::PingReq),
        13 => (input, MqttPacket::PingResp),
        14 => (input, MqttPacket::Disconnect),
        _ => return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    };

    Ok((input, packet))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_mqtt_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed MQTT packet: {:?}", packet);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining", remaining.len());
            }
        }
        Err(e) => eprintln!("Failed to parse MQTT packet: {:?}", e),
    }
}