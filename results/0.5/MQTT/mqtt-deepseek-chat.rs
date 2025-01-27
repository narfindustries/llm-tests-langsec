use nom::{
    bits::complete::take,
    bytes::complete::{tag, take as take_bytes},
    combinator::{map, map_res},
    multi::length_data,
    number::complete::{be_u16, be_u8},
    sequence::{pair, tuple},
    IResult,
};
use std::{env, fs, io::Read};

#[derive(Debug)]
struct MqttFixedHeader {
    packet_type: u8,
    flags: u8,
    remaining_length: u32,
}

#[derive(Debug)]
struct MqttConnect {
    protocol_name: String,
    protocol_level: u8,
    connect_flags: u8,
    keep_alive: u16,
    client_id: String,
    will_topic: Option<String>,
    will_message: Option<String>,
    username: Option<String>,
    password: Option<String>,
}

#[derive(Debug)]
struct MqttConnack {
    session_present: bool,
    return_code: u8,
}

#[derive(Debug)]
struct MqttPublish {
    topic_name: String,
    packet_id: Option<u16>,
    payload: Vec<u8>,
}

#[derive(Debug)]
struct MqttPuback {
    packet_id: u16,
}

#[derive(Debug)]
struct MqttPubrec {
    packet_id: u16,
}

#[derive(Debug)]
struct MqttPubrel {
    packet_id: u16,
}

#[derive(Debug)]
struct MqttPubcomp {
    packet_id: u16,
}

#[derive(Debug)]
struct MqttSubscribe {
    packet_id: u16,
    topic_filters: Vec<(String, u8)>,
}

#[derive(Debug)]
struct MqttSuback {
    packet_id: u16,
    return_codes: Vec<u8>,
}

#[derive(Debug)]
struct MqttUnsubscribe {
    packet_id: u16,
    topic_filters: Vec<String>,
}

#[derive(Debug)]
struct MqttUnsuback {
    packet_id: u16,
}

#[derive(Debug)]
struct MqttPingreq;

#[derive(Debug)]
struct MqttPingresp;

#[derive(Debug)]
struct MqttDisconnect;

#[derive(Debug)]
enum MqttPacket {
    Connect(MqttConnect),
    Connack(MqttConnack),
    Publish(MqttPublish),
    Puback(MqttPuback),
    Pubrec(MqttPubrec),
    Pubrel(MqttPubrel),
    Pubcomp(MqttPubcomp),
    Subscribe(MqttSubscribe),
    Suback(MqttSuback),
    Unsubscribe(MqttUnsubscribe),
    Unsuback(MqttUnsuback),
    Pingreq(MqttPingreq),
    Pingresp(MqttPingresp),
    Disconnect(MqttDisconnect),
}

fn parse_mqtt_fixed_header(input: &[u8]) -> IResult<&[u8], MqttFixedHeader> {
    let (input, (packet_type_flags, remaining_length)) = tuple((be_u8, parse_remaining_length))(input)?;
    let packet_type = packet_type_flags >> 4;
    let flags = packet_type_flags & 0x0F;
    Ok((input, MqttFixedHeader { packet_type, flags, remaining_length }))
}

fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], u32> {
    let mut value = 0;
    let mut shift = 0;
    let mut input = input;
    loop {
        let (i, byte) = be_u8(input)?;
        value |= ((byte & 0x7F) as u32) << shift;
        shift += 7;
        input = i;
        if byte & 0x80 == 0 {
            break;
        }
    }
    Ok((input, value))
}

fn parse_mqtt_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, len) = be_u16(input)?;
    let (input, bytes) = take_bytes(len as usize)(input)?;
    Ok((input, String::from_utf8_lossy(bytes).into_owned()))
}

fn parse_mqtt_connect(input: &[u8]) -> IResult<&[u8], MqttConnect> {
    let (input, protocol_name) = parse_mqtt_string(input)?;
    let (input, protocol_level) = be_u8(input)?;
    let (input, connect_flags) = be_u8(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, client_id) = parse_mqtt_string(input)?;

    let will_topic = if connect_flags & 0x04 != 0 {
        let (input, topic) = parse_mqtt_string(input)?;
        Some(topic)
    } else {
        None
    };

    let will_message = if connect_flags & 0x04 != 0 {
        let (input, message) = parse_mqtt_string(input)?;
        Some(message)
    } else {
        None
    };

    let username = if connect_flags & 0x80 != 0 {
        let (input, user) = parse_mqtt_string(input)?;
        Some(user)
    } else {
        None
    };

    let password = if connect_flags & 0x40 != 0 {
        let (input, pass) = parse_mqtt_string(input)?;
        Some(pass)
    } else {
        None
    };

    Ok((input, MqttConnect {
        protocol_name,
        protocol_level,
        connect_flags,
        keep_alive,
        client_id,
        will_topic,
        will_message,
        username,
        password,
    }))
}

fn parse_mqtt_connack(input: &[u8]) -> IResult<&[u8], MqttConnack> {
    let (input, flags) = be_u8(input)?;
    let (input, return_code) = be_u8(input)?;
    let session_present = flags & 0x01 != 0;
    Ok((input, MqttConnack { session_present, return_code }))
}

fn parse_mqtt_publish(input: &[u8], flags: u8) -> IResult<&[u8], MqttPublish> {
    let (input, topic_name) = parse_mqtt_string(input)?;
    let (input, packet_id) = if flags & 0x06 != 0 {
        let (input, id) = be_u16(input)?;
        (input, Some(id))
    } else {
        (input, None)
    };
    let (input, payload) = take_bytes(input.len())(input)?;
    Ok((input, MqttPublish { topic_name, packet_id, payload: payload.to_vec() }))
}

fn parse_mqtt_puback(input: &[u8]) -> IResult<&[u8], MqttPuback> {
    let (input, packet_id) = be_u16(input)?;
    Ok((input, MqttPuback { packet_id }))
}

fn parse_mqtt_pubrec(input: &[u8]) -> IResult<&[u8], MqttPubrec> {
    let (input, packet_id) = be_u16(input)?;
    Ok((input, MqttPubrec { packet_id }))
}

fn parse_mqtt_pubrel(input: &[u8]) -> IResult<&[u8], MqttPubrel> {
    let (input, packet_id) = be_u16(input)?;
    Ok((input, MqttPubrel { packet_id }))
}

fn parse_mqtt_pubcomp(input: &[u8]) -> IResult<&[u8], MqttPubcomp> {
    let (input, packet_id) = be_u16(input)?;
    Ok((input, MqttPubcomp { packet_id }))
}

fn parse_mqtt_subscribe(input: &[u8]) -> IResult<&[u8], MqttSubscribe> {
    let (input, packet_id) = be_u16(input)?;
    let (input, topic_filters) = nom::multi::many1(pair(parse_mqtt_string, be_u8))(input)?;
    Ok((input, MqttSubscribe { packet_id, topic_filters }))
}

fn parse_mqtt_suback(input: &[u8]) -> IResult<&[u8], MqttSuback> {
    let (input, packet_id) = be_u16(input)?;
    let (input, return_codes) = nom::multi::many1(be_u8)(input)?;
    Ok((input, MqttSuback { packet_id, return_codes }))
}

fn parse_mqtt_unsubscribe(input: &[u8]) -> IResult<&[u8], MqttUnsubscribe> {
    let (input, packet_id) = be_u16(input)?;
    let (input, topic_filters) = nom::multi::many1(parse_mqtt_string)(input)?;
    Ok((input, MqttUnsubscribe { packet_id, topic_filters }))
}

fn parse_mqtt_unsuback(input: &[u8]) -> IResult<&[u8], MqttUnsuback> {
    let (input, packet_id) = be_u16(input)?;
    Ok((input, MqttUnsuback { packet_id }))
}

fn parse_mqtt_pingreq(input: &[u8]) -> IResult<&[u8], MqttPingreq> {
    Ok((input, MqttPingreq))
}

fn parse_mqtt_pingresp(input: &[u8]) -> IResult<&[u8], MqttPingresp> {
    Ok((input, MqttPingresp))
}

fn parse_mqtt_disconnect(input: &[u8]) -> IResult<&[u8], MqttDisconnect> {
    Ok((input, MqttDisconnect))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, fixed_header) = parse_mqtt_fixed_header(input)?;
    match fixed_header.packet_type {
        1 => map(parse_mqtt_connect, MqttPacket::Connect)(input),
        2 => map(parse_mqtt_connack, MqttPacket::Connack)(input),
        3 => map(|i| parse_mqtt_publish(i, fixed_header.flags), MqttPacket::Publish)(input),
        4 => map(parse_mqtt_puback, MqttPacket::Puback)(input),
        5 => map(parse_mqtt_pubrec, MqttPacket::Pubrec)(input),
        6 => map(parse_mqtt_pubrel, MqttPacket::Pubrel)(input),
        7 => map(parse_mqtt_pubcomp, MqttPacket::Pubcomp)(input),
        8 => map(parse_mqtt_subscribe, MqttPacket::Subscribe)(input),
        9 => map(parse_mqtt_suback, MqttPacket::Suback)(input),
        10 => map(parse_mqtt_unsubscribe, MqttPacket::Unsubscribe)(input),
        11 => map(parse_mqtt_unsuback, MqttPacket::Unsuback)(input),
        12 => map(parse_mqtt_pingreq, MqttPacket::Pingreq)(input),
        13 => map(parse_mqtt_pingresp, MqttPacket::Pingresp)(input),
        14 => map(parse_mqtt_disconnect, MqttPacket::Disconnect)(input),
        _ => panic!("Unknown packet type"),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let mut file = fs::File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse MQTT packet: {:?}", e),
    }
}