use nom::{
    bits::complete::take as take_bits,
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::length_data,
    number::complete::{be_u16, be_u8},
    sequence::{pair, tuple},
    IResult,
};
use std::{env, fs, io::Read};

#[derive(Debug)]
struct MqttPacket {
    header: MqttHeader,
    payload: MqttPayload,
}

#[derive(Debug)]
struct MqttHeader {
    packet_type: u8,
    flags: u8,
    remaining_length: usize,
}

#[derive(Debug)]
enum MqttPayload {
    Connect(MqttConnect),
    ConnAck(MqttConnAck),
    Publish(MqttPublish),
    PubAck(MqttPubAck),
    PubRec(MqttPubRec),
    PubRel(MqttPubRel),
    PubComp(MqttPubComp),
    Subscribe(MqttSubscribe),
    SubAck(MqttSubAck),
    Unsubscribe(MqttUnsubscribe),
    UnsubAck(MqttUnsubAck),
    PingReq,
    PingResp,
    Disconnect,
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
struct MqttConnAck {
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
struct MqttPubAck {
    packet_id: u16,
}

#[derive(Debug)]
struct MqttPubRec {
    packet_id: u16,
}

#[derive(Debug)]
struct MqttPubRel {
    packet_id: u16,
}

#[derive(Debug)]
struct MqttPubComp {
    packet_id: u16,
}

#[derive(Debug)]
struct MqttSubscribe {
    packet_id: u16,
    topics: Vec<(String, u8)>,
}

#[derive(Debug)]
struct MqttSubAck {
    packet_id: u16,
    return_codes: Vec<u8>,
}

#[derive(Debug)]
struct MqttUnsubscribe {
    packet_id: u16,
    topics: Vec<String>,
}

#[derive(Debug)]
struct MqttUnsubAck {
    packet_id: u16,
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, header) = parse_mqtt_header(input)?;
    let (input, payload) = parse_mqtt_payload(&header, input)?;
    Ok((input, MqttPacket { header, payload }))
}

fn parse_mqtt_header(input: &[u8]) -> IResult<&[u8], MqttHeader> {
    let (input, (packet_type_flags, remaining_length)) = tuple((be_u8, parse_remaining_length))(input)?;
    let packet_type = packet_type_flags >> 4;
    let flags = packet_type_flags & 0x0F;
    Ok((input, MqttHeader { packet_type, flags, remaining_length }))
}

fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], usize> {
    let mut value = 0;
    let mut shift = 0;
    let mut input = input;
    loop {
        let (i, byte) = be_u8(input)?;
        input = i;
        value += ((byte & 0x7F) as usize) << shift;
        if byte & 0x80 == 0 {
            break;
        }
        shift += 7;
    }
    Ok((input, value))
}

fn parse_mqtt_payload(header: &MqttHeader, input: &[u8]) -> IResult<&[u8], MqttPayload> {
    match header.packet_type {
        1 => map(parse_connect, MqttPayload::Connect)(input),
        2 => map(parse_connack, MqttPayload::ConnAck)(input),
        3 => map(parse_publish(header.flags), MqttPayload::Publish)(input),
        4 => map(parse_puback, MqttPayload::PubAck)(input),
        5 => map(parse_pubrec, MqttPayload::PubRec)(input),
        6 => map(parse_pubrel, MqttPayload::PubRel)(input),
        7 => map(parse_pubcomp, MqttPayload::PubComp)(input),
        8 => map(parse_subscribe, MqttPayload::Subscribe)(input),
        9 => map(parse_suback, MqttPayload::SubAck)(input),
        10 => map(parse_unsubscribe, MqttPayload::Unsubscribe)(input),
        11 => map(parse_unsuback, MqttPayload::UnsubAck)(input),
        12 => Ok((input, MqttPayload::PingReq)),
        13 => Ok((input, MqttPayload::PingResp)),
        14 => Ok((input, MqttPayload::Disconnect)),
        _ => panic!("Unknown packet type"),
    }
}

fn parse_connect(input: &[u8]) -> IResult<&[u8], MqttConnect> {
    let (input, protocol_name) = parse_string(input)?;
    let (input, protocol_level) = be_u8(input)?;
    let (input, connect_flags) = be_u8(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, client_id) = parse_string(input)?;
    let (input, will_topic) = if connect_flags & 0x04 != 0 {
        let (input, topic) = parse_string(input)?;
        (input, Some(topic))
    } else {
        (input, None)
    };
    let (input, will_message) = if connect_flags & 0x04 != 0 {
        let (input, message) = parse_string(input)?;
        (input, Some(message))
    } else {
        (input, None)
    };
    let (input, username) = if connect_flags & 0x80 != 0 {
        let (input, username) = parse_string(input)?;
        (input, Some(username))
    } else {
        (input, None)
    };
    let (input, password) = if connect_flags & 0x40 != 0 {
        let (input, password) = parse_string(input)?;
        (input, Some(password))
    } else {
        (input, None)
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

fn parse_connack(input: &[u8]) -> IResult<&[u8], MqttConnAck> {
    let (input, (session_present, return_code)) = tuple((be_u8, be_u8))(input)?;
    Ok((input, MqttConnAck {
        session_present: session_present & 0x01 != 0,
        return_code,
    }))
}

fn parse_publish(flags: u8) -> impl Fn(&[u8]) -> IResult<&[u8], MqttPublish> {
    move |input| {
        let (input, topic_name) = parse_string(input)?;
        let (input, packet_id) = if flags & 0x06 != 0 {
            let (input, packet_id) = be_u16(input)?;
            (input, Some(packet_id))
        } else {
            (input, None)
        };
        let (input, payload) = take(input.len())(input)?;
        Ok((input, MqttPublish {
            topic_name,
            packet_id,
            payload: payload.to_vec(),
        }))
    }
}

fn parse_puback(input: &[u8]) -> IResult<&[u8], MqttPubAck> {
    let (input, packet_id) = be_u16(input)?;
    Ok((input, MqttPubAck { packet_id }))
}

fn parse_pubrec(input: &[u8]) -> IResult<&[u8], MqttPubRec> {
    let (input, packet_id) = be_u16(input)?;
    Ok((input, MqttPubRec { packet_id }))
}

fn parse_pubrel(input: &[u8]) -> IResult<&[u8], MqttPubRel> {
    let (input, packet_id) = be_u16(input)?;
    Ok((input, MqttPubRel { packet_id }))
}

fn parse_pubcomp(input: &[u8]) -> IResult<&[u8], MqttPubComp> {
    let (input, packet_id) = be_u16(input)?;
    Ok((input, MqttPubComp { packet_id }))
}

fn parse_subscribe(input: &[u8]) -> IResult<&[u8], MqttSubscribe> {
    let (input, packet_id) = be_u16(input)?;
    let (input, topics) = parse_topic_filters(input)?;
    Ok((input, MqttSubscribe { packet_id, topics }))
}

fn parse_suback(input: &[u8]) -> IResult<&[u8], MqttSubAck> {
    let (input, packet_id) = be_u16(input)?;
    let (input, return_codes) = parse_return_codes(input)?;
    Ok((input, MqttSubAck { packet_id, return_codes }))
}

fn parse_unsubscribe(input: &[u8]) -> IResult<&[u8], MqttUnsubscribe> {
    let (input, packet_id) = be_u16(input)?;
    let (input, topics) = parse_topic_filters(input)?;
    Ok((input, MqttUnsubscribe { packet_id, topics }))
}

fn parse_unsuback(input: &[u8]) -> IResult<&[u8], MqttUnsubAck> {
    let (input, packet_id) = be_u16(input)?;
    Ok((input, MqttUnsubAck { packet_id }))
}

fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, len) = be_u16(input)?;
    let (input, str_bytes) = take(len)(input)?;
    Ok((input, String::from_utf8_lossy(str_bytes).into_owned()))
}

fn parse_topic_filters(input: &[u8]) -> IResult<&[u8], Vec<(String, u8)>> {
    let mut input = input;
    let mut topics = Vec::new();
    while input.len() > 0 {
        let (i, topic) = parse_string(input)?;
        let (i, qos) = be_u8(i)?;
        input = i;
        topics.push((topic, qos));
    }
    Ok((input, topics))
}

fn parse_return_codes(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut input = input;
    let mut return_codes = Vec::new();
    while input.len() > 0 {
        let (i, code) = be_u8(input)?;
        input = i;
        return_codes.push(code);
    }
    Ok((input, return_codes))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
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