use nom::{
    bits::complete::{tag, take},
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::{digit1, hex_digit1},
    combinator::{map, map_res, opt},
    multi::{count, length_data, many_till},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::{
    collections::HashMap,
    fs::File,
    io::{Read, stdin},
    str,
};

#[derive(Debug, PartialEq)]
enum MqttPacketType {
    Connect = 1,
    ConnAck = 2,
    Publish = 3,
    PubAck = 4,
    PubRec = 5,
    PubRel = 6,
    PubComp = 7,
    Subscribe = 8,
    SubAck = 9,
    Unsubscribe = 10,
    UnsubAck = 11,
    PingReq = 12,
    PingResp = 13,
    Disconnect = 14,
}

#[derive(Debug, PartialEq)]
enum MqttQos {
    AtMostOnce = 0,
    AtLeastOnce = 1,
    ExactlyOnce = 2,
}

#[derive(Debug, PartialEq)]
struct MqttFixedHeader {
    packet_type: MqttPacketType,
    dup: bool,
    qos: MqttQos,
    retain: bool,
}

impl MqttFixedHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map(
            take(1u8),
            |header: &[u8]| MqttFixedHeader {
                packet_type: match header[0] >> 4 {
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
                    _ => panic!("Invalid packet type"),
                },
                dup: (header[0] & 0x08) != 0,
                qos: match (header[0] & 0x06) >> 1 {
                    0 => MqttQos::AtMostOnce,
                    1 => MqttQos::AtLeastOnce,
                    2 => MqttQos::ExactlyOnce,
                    _ => panic!("Invalid QoS"),
                },
                retain: (header[0] & 0x01) != 0,
            },
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct MqttVariableHeader {
    protocol_name: String,
    protocol_version: u8,
    connect_flags: u8,
    keep_alive: u16,
}

impl MqttVariableHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map(
            tuple((
                take(2u8),
                take(1u8),
                take(1u8),
                take(2u8),
            )),
            |(protocol_name_len, protocol_version, connect_flags, keep_alive): ([u8; 2], u8, u8, [u8; 2])| {
                MqttVariableHeader {
                    protocol_name: String::from_utf8_lossy(&input[2..(2 + protocol_name_len[0] as usize)]).into_owned(),
                    protocol_version,
                    connect_flags,
                    keep_alive: ((keep_alive[0] as u16) << 8) + keep_alive[1] as u16,
                }
            },
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct MqttConnectPacket {
    fixed_header: MqttFixedHeader,
    variable_header: MqttVariableHeader,
    payload: Vec<u8>,
}

impl MqttConnectPacket {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map(
            tuple((
                MqttFixedHeader::parse,
                MqttVariableHeader::parse,
                length_data(take(2u8)),
            )),
            |(fixed_header, variable_header, payload): (MqttFixedHeader, MqttVariableHeader, Vec<u8>)| {
                MqttConnectPacket {
                    fixed_header,
                    variable_header,
                    payload,
                }
            },
        )(input)
    }
}

fn main() {
    let mut file = match std::env::args().nth(1) {
        Some(filename) => File::open(filename).unwrap(),
        None => stdin(),
    };

    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).unwrap();

    let (_, packet) = MqttConnectPacket::parse(&buffer).unwrap();
    println!("{:?}", packet);
}