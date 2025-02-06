use nom::{
    bytes::complete::{take, tag},
    combinator::{map, map_res},
    number::complete::{be_u16, be_u8, be_u32},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
enum MqttControlPacket {
    Connect(ConnectPacket),
    Connack(ConnackPacket),
    Publish(PublishPacket),
    Puback(PubackPacket),
    Pubrec(PubrecPacket),
    Pubrel(PubrelPacket),
    Pubcomp(PubcompPacket),
    Subscribe(SubscribePacket),
    Suback(SubackPacket),
    Unsubscribe(UnsubscribePacket),
    Unsuback(UnsubackPacket),
    Pingreq,
    Pingresp,
    Disconnect(DisconnectPacket),
    Auth(AuthPacket),
}

#[derive(Debug)]
struct ConnectPacket {
    protocol_name: String,
    protocol_level: u8,
    connect_flags: u8,
    keep_alive: u16,
    properties: Properties,
    client_identifier: String,
    will_properties: Option<Properties>,
    will_topic: Option<String>,
    will_payload: Option<Vec<u8>>,
    username: Option<String>,
    password: Option<String>,
}

#[derive(Debug)]
struct ConnackPacket {
    session_present_flag: u8,
    connect_reason_code: u8,
    properties: Properties,
}

#[derive(Debug)]
struct PublishPacket {
    topic_name: String,
    packet_identifier: Option<u16>,
    properties: Properties,
    payload: Vec<u8>,
}

#[derive(Debug)]
struct PubackPacket {
    packet_identifier: u16,
    reason_code: u8,
    properties: Properties,
}

#[derive(Debug)]
struct PubrecPacket {
    packet_identifier: u16,
    reason_code: u8,
    properties: Properties,
}

#[derive(Debug)]
struct PubrelPacket {
    packet_identifier: u16,
    reason_code: u8,
    properties: Properties,
}

#[derive(Debug)]
struct PubcompPacket {
    packet_identifier: u16,
    reason_code: u8,
    properties: Properties,
}

#[derive(Debug)]
struct SubscribePacket {
    packet_identifier: u16,
    properties: Properties,
    topic_filters: Vec<TopicFilter>,
}

#[derive(Debug)]
struct SubackPacket {
    packet_identifier: u16,
    properties: Properties,
    reason_code_list: Vec<u8>,
}

#[derive(Debug)]
struct UnsubscribePacket {
    packet_identifier: u16,
    properties: Properties,
    topic_filters: Vec<String>,
}

#[derive(Debug)]
struct UnsubackPacket {
    packet_identifier: u16,
    properties: Properties,
    reason_code_list: Vec<u8>,
}

#[derive(Debug)]
struct DisconnectPacket {
    reason_code: u8,
    properties: Properties,
}

#[derive(Debug)]
struct AuthPacket {
    reason_code: u8,
    properties: Properties,
}

#[derive(Debug)]
struct Properties {
    example_property: u32,
}

#[derive(Debug)]
struct TopicFilter {
    topic_filter: String,
    subscription_options: u8,
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No input file specified"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => println!("Failed to parse MQTT packet: {:?}", e),
    }

    Ok(())
}

fn parse_mqtt(input: &[u8]) -> IResult<&[u8], MqttControlPacket> {
    let (input, packet_type) = be_u8(input)?;
    match packet_type {
        0x10 => map(parse_connect_packet, MqttControlPacket::Connect)(input),
        0x20 => map(parse_connack_packet, MqttControlPacket::Connack)(input),
        0x30 => map(parse_publish_packet, MqttControlPacket::Publish)(input),
        0x40 => map(parse_puback_packet, MqttControlPacket::Puback)(input),
        0x50 => map(parse_pubrec_packet, MqttControlPacket::Pubrec)(input),
        0x60 => map(parse_pubrel_packet, MqttControlPacket::Pubrel)(input),
        0x70 => map(parse_pubcomp_packet, MqttControlPacket::Pubcomp)(input),
        0x80 => map(parse_subscribe_packet, MqttControlPacket::Subscribe)(input),
        0x90 => map(parse_suback_packet, MqttControlPacket::Suback)(input),
        0xA0 => map(parse_unsubscribe_packet, MqttControlPacket::Unsubscribe)(input),
        0xB0 => map(parse_unsuback_packet, MqttControlPacket::Unsuback)(input),
        0xC0 => map(|i| Ok((i, MqttControlPacket::Pingreq)), |_| {})(input),
        0xD0 => map(|i| Ok((i, MqttControlPacket::Pingresp)), |_| {})(input),
        0xE0 => map(parse_disconnect_packet, MqttControlPacket::Disconnect)(input),
        0xF0 => map(parse_auth_packet, MqttControlPacket::Auth)(input),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, (protocol_name, protocol_level, connect_flags, keep_alive)) = tuple((
        map_res(preceded(tag(b"\x00\x04MQTT"), take(4usize)), std::str::from_utf8),
        be_u8,
        be_u8,
        be_u16,
    ))(input)?;

    let (input, properties) = parse_properties(input)?;

    let (input, client_identifier) = parse_string(input)?;

    let (input, will_properties) = if connect_flags & 0x04 > 0 {
        map(parse_properties, Some)(input)?
    } else {
        (input, None)
    };

    let (input, will_topic) = if connect_flags & 0x04 > 0 {
        map(parse_string, Some)(input)?
    } else {
        (input, None)
    };

    let (input, will_payload) = if connect_flags & 0x04 > 0 {
        map(parse_bytes, Some)(input)?
    } else {
        (input, None)
    };

    let (input, username) = if connect_flags & 0x80 > 0 {
        map(parse_string, Some)(input)?
    } else {
        (input, None)
    };

    let (input, password) = if connect_flags & 0x40 > 0 {
        map(parse_string, Some)(input)?
    } else {
        (input, None)
    };

    Ok((
        input,
        ConnectPacket {
            protocol_name: protocol_name.to_string(),
            protocol_level,
            connect_flags,
            keep_alive,
            properties,
            client_identifier: client_identifier.to_string(),
            will_properties,
            will_topic: will_topic.map(|s| s.to_string()),
            will_payload,
            username: username.map(|s| s.to_string()),
            password: password.map(|s| s.to_string()),
        },
    ))
}

fn parse_connack_packet(input: &[u8]) -> IResult<&[u8], ConnackPacket> {
    let (input, (session_present_flag, connect_reason_code)) = tuple((be_u8, be_u8))(input)?;
    let (input, properties) = parse_properties(input)?;
    Ok((
        input,
        ConnackPacket {
            session_present_flag,
            connect_reason_code,
            properties,
        },
    ))
}

fn parse_publish_packet(input: &[u8]) -> IResult<&[u8], PublishPacket> {
    let (input, topic_name) = parse_string(input)?;
    let (input, packet_identifier) = if topic_name.len() > 0 { // Assuming QoS > 0 for simplicity
        map(be_u16, Some)(input)?
    } else {
        (input, None)
    };
    let (input, properties) = parse_properties(input)?;
    let (input, payload) = map_res(take(input.len()), |b: &[u8]| Ok::<Vec<u8>, nom::Err<(&[u8], nom::error::ErrorKind)>>(b.to_vec()))(input)?;
    Ok((
        input,
        PublishPacket {
            topic_name: topic_name.to_string(),
            packet_identifier,
            properties,
            payload,
        },
    ))
}

fn parse_puback_packet(input: &[u8]) -> IResult<&[u8], PubackPacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, reason_code) = be_u8(input)?;
    let (input, properties) = parse_properties(input)?;
    Ok((
        input,
        PubackPacket {
            packet_identifier,
            reason_code,
            properties,
        },
    ))
}

fn parse_pubrec_packet(input: &[u8]) -> IResult<&[u8], PubrecPacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, reason_code) = be_u8(input)?;
    let (input, properties) = parse_properties(input)?;
    Ok((
        input,
        PubrecPacket {
            packet_identifier,
            reason_code,
            properties,
        },
    ))
}

fn parse_pubrel_packet(input: &[u8]) -> IResult<&[u8], PubrelPacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, reason_code) = be_u8(input)?;
    let (input, properties) = parse_properties(input)?;
    Ok((
        input,
        PubrelPacket {
            packet_identifier,
            reason_code,
            properties,
        },
    ))
}

fn parse_pubcomp_packet(input: &[u8]) -> IResult<&[u8], PubcompPacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, reason_code) = be_u8(input)?;
    let (input, properties) = parse_properties(input)?;
    Ok((
        input,
        PubcompPacket {
            packet_identifier,
            reason_code,
            properties,
        },
    ))
}

fn parse_subscribe_packet(input: &[u8]) -> IResult<&[u8], SubscribePacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, properties) = parse_properties(input)?;
    let (input, topic_filters) = map_res(take(input.len()), |b: &[u8]| Ok::<Vec<u8>, nom::Err<(&[u8], nom::error::ErrorKind)>>(b.to_vec()))(input)?;
    Ok((
        input,
        SubscribePacket {
            packet_identifier,
            properties,
            topic_filters,
        },
    ))
}

fn parse_suback_packet(input: &[u8]) -> IResult<&[u8], SubackPacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, properties) = parse_properties(input)?;
    let (input, reason_code_list) = map_res(take(input.len()), |b: &[u8]| Ok::<Vec<u8>, nom::Err<(&[u8], nom::error::ErrorKind)>>(b.to_vec()))(input)?;
    Ok((
        input,
        SubackPacket {
            packet_identifier,
            properties,
            reason_code_list,
        },
    ))
}

fn parse_unsubscribe_packet(input: &[u8]) -> IResult<&[u8], UnsubscribePacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, properties) = parse_properties(input)?;
    let (input, topic_filters) = map_res(take(input.len()), |b: &[u8]| Ok::<Vec<u8>, nom::Err<(&[u8], nom::error::ErrorKind)>>(b.to_vec()))(input)?;
    Ok((
        input,
        UnsubscribePacket {
            packet_identifier,
            properties,
            topic_filters,
        },
    ))
}

fn parse_unsuback_packet(input: &[u8]) -> IResult<&[u8], UnsubackPacket> {
    let (input, packet_identifier) = be_u16(input)?;
    let (input, properties) = parse_properties(input)?;
    let (input, reason_code_list) = map_res(take(input.len()), |b: &[u8]| Ok::<Vec<u8>, nom::Err<(&[u8], nom::error::ErrorKind)>>(b.to_vec()))(input)?;
    Ok((
        input,
        UnsubackPacket {
            packet_identifier,
            properties,
            reason_code_list,
        },
    ))
}

fn parse_disconnect_packet(input: &[u8]) -> IResult<&[u8], DisconnectPacket> {
    let (input, reason_code) = be_u8(input)?;
    let (input, properties) = parse_properties(input)?;
    Ok((
        input,
        DisconnectPacket {
            reason_code,
            properties,
        },
    ))
}

fn parse_auth_packet(input: &[u8]) -> IResult<&[u8], AuthPacket> {
    let (input, reason_code) = be_u8(input)?;
    let (input, properties) = parse_properties(input)?;
    Ok((
        input,
        AuthPacket {
            reason_code,
            properties,
        },
    ))
}

fn parse_properties(input: &[u8]) -> IResult<&[u8], Properties> {
    let (input, example_property) = be_u32(input)?;
    Ok((input, Properties { example_property }))
}

fn parse_string(input: &[u8]) -> IResult<&[u8], &str> {
    let (input, len) = be_u16(input)?;
    map_res(take(len), std::str::from_utf8)(input)
}

fn parse_bytes(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, len) = be_u16(input)?;
    map(take(len), Vec::from)(input)
}