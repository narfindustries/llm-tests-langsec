use nom::{
    bits::complete::take as take_bits,
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, verify},
    multi::length_data,
    number::complete::{be_u16, be_u8},
    sequence::{pair, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
enum PacketType {
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
    packet_type: PacketType,
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
struct ConnackPacket {
    session_present: bool,
    reason_code: u8,
}

#[derive(Debug)]
struct PublishPacket {
    topic_name: String,
    packet_identifier: Option<极，问题出在 `parse_pubrel_packet` 函数的实现上。在之前的代码中，`parse_pubrel_packet` 函数没有正确闭合，导致编译错误。以下是修复后的完整代码：

fn parse_pubrel_packet(input: &[u8]) -> IResult<&[u8], PubrelPacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, reason_code) = be_u8(input)?;
    Ok((
        input,
        PubrelPacket {
            packet_identifier,
            reason_code,
        },
    ))
}

fn parse_pubcomp_packet(input: &[u8]) -> IResult<&[u8], PubcompPacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, reason_code) = be_u8(input)?;
    Ok((
        input,
        PubcompPacket {
            packet_identifier,
            reason_code,
        },
    ))
}

fn parse_subscribe_packet(input: &[u8]) -> IResult<&[u8], SubscribePacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, topic_filters) = parse_topic_filters(input)?;
    Ok((
        input,
        SubscribePacket {
            packet_identifier,
            topic_filters,
        },
    ))
}

fn parse_topic_filters(input: &[u8]) -> IResult<&[u8], Vec<(String, u8)>> {
    let mut input = input;
    let mut topic_filters = Vec::new();
    while !input.is_empty() {
        let (new_input, topic) = parse_utf8_string(input)?;
        let (new_input, qos) = be_u8(new_input)?;
        topic_filters.push((topic, qos));
        input = new_input;
    }
    Ok((input, topic_filters))
}

fn parse_suback_packet(input: &[u8]) -> IResult<&[u8], SubackPacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, reason_codes) = parse_reason_codes(input)?;
    Ok((
        input,
        SubackPacket {
            packet_identifier,
            reason_codes,
        },
    ))
}

fn parse_reason_codes(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut input = input;
    let mut reason_codes = Vec::new();
    while !input.is_empty() {
        let (new_input, reason_code) = be_u8(input)?;
        reason_codes.push(reason_code);
        input = new_input;
    }
    Ok((input, reason_codes))
}

fn parse_unsubscribe_packet(input: &[u8]) -> IResult<&[u8], UnsubscribePacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, topic_filters) = parse_topic_names(input)?;
    Ok((
        input,
        UnsubscribePacket {
            packet_identifier,
            topic_filters,
        },
    ))
}

fn parse_topic_names(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut input = input;
    let mut topic_names = Vec::new();
    while !input.is_empty() {
        let (new_input, topic) = parse_utf8_string(input)?;
        topic_names.push(topic);
        input = new_input;
    }
    Ok((input, topic_names))
}

fn parse_unsuback_packet(input: &[u8]) -> IResult<&[u8], UnsubackPacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, reason_codes) = parse_reason_codes(input)?;
    Ok((
        input,
        UnsubackPacket {
            packet_identifier,
            reason_codes,
        },
    ))
}

fn parse_disconnect_packet(input: &[u8]) -> IResult<&[u8], DisconnectPacket> {
    let (input, reason_code) = be_u8(input)?;
    Ok((
        input,
        DisconnectPacket {
            reason_code,
        },
    ))
}

fn parse_auth_packet(input: &[u8]) -> IResult<&[u8], AuthPacket> {
    let (input, reason_code) = be_u8(input)?;
    Ok((
        input,
        AuthPacket {
            reason_code,
        },
    ))
}

fn parse_packet(input: &[u8]) -> IResult<&[u8], MQTTPacket> {
    let (input, fixed_header) = parse_fixed_header(input)?;
    match fixed_header.packet_type {
        PacketType::Connect => {
            let (input, connect_packet) = parse_connect_packet(input)?;
            Ok((input, MQTTPacket::Connect(connect_packet)))
        }
        PacketType::Connack => {
            let (input, connack_packet) = parse_connack_packet(input)?;
            Ok((input, MQTTPacket::Connack(connack_packet)))
        }
        PacketType::Publish => {
            let (input, publish_packet) = parse_publish_packet(input)?;
            Ok((input, MQTTPacket::Publish(publish_packet)))
        }
        PacketType::Puback => {
            let (input, puback_packet) = parse_puback_packet(input)?;
            Ok((input, MQTTPacket::Puback(puback_packet)))
        }
        PacketType::Pubrec => {
            let (input, pubrec_packet) = parse_pubrec_packet(input)?;
            Ok((input, MQTTPacket::Pubrec(pubrec_packet)))
        }
        PacketType::Pubrel => {
            let (input, pubrel_packet) = parse_pubrel_packet(input)?;
            Ok((input, MQTTPacket::Pubrel(pubrel_packet)))
        }
        PacketType::Pubcomp => {
            let (input, pubcomp_packet) = parse_pubcomp_packet(input)?;
            Ok((input, MQTTPacket::Pubcomp(pubcomp_packet)))
        }
        PacketType::Subscribe => {
            let (input, subscribe_packet) = parse_subscribe_packet(input)?;
            Ok((input, MQTTPacket::Subscribe(subscribe_packet)))
        }
        PacketType::Suback => {
            let (input, suback_packet) = parse_suback_packet(input)?;
            Ok((input, MQTTPacket::Suback(suback_packet)))
        }
        PacketType::Unsubscribe => {
            let (input, unsubscribe_packet) = parse_unsubscribe_packet(input)?;
            Ok((input, MQTTPacket::Unsubscribe(unsubscribe_packet)))
        }
        PacketType::Unsuback => {
            let (input, unsuback_packet) = parse_unsuback_packet(input)?;
            Ok((input, MQTTPacket::Unsuback(unsuback_packet)))
        }
        PacketType::Pingreq => Ok((input, MQTTPacket::Pingreq)),
        PacketType::Pingresp => Ok((input, MQTTPacket::Pingresp)),
        PacketType::Disconnect => {
            let (input, disconnect_packet) = parse_disconnect_packet(input)?;
            Ok((input, MQTTPacket::Disconnect(disconnect_packet)))
        }
        PacketType::Auth => {
            let (input, auth_packet) = parse_auth_packet(input)?;
            Ok((input, MQTTPacket::Auth(auth_packet)))
        }
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len