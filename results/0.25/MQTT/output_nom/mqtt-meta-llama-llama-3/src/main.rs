use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{length_data},
    number::complete::{be_u8, be_u16, be_u32},
    IResult,
};
use std::{env, fs};

#[derive(Debug)]
enum MqttControlPacketType {
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
}

#[derive(Debug)]
enum MqttQos {
    AtMostOnce,
    AtLeastOnce,
    ExactlyOnce,
}

#[derive(Debug)]
struct MqttFixedHeader {
    control_packet_type: MqttControlPacketType,
    flags: u8,
}

impl MqttFixedHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, first_byte) = be_u8(input)?;
        let control_packet_type = match first_byte >> 4 {
            1 => MqttControlPacketType::Connect,
            2 => MqttControlPacketType::Connack,
            3 => MqttControlPacketType::Publish,
            4 => MqttControlPacketType::Puback,
            5 => MqttControlPacketType::Pubrec,
            6 => MqttControlPacketType::Pubrel,
            7 => MqttControlPacketType::Pubcomp,
            8 => MqttControlPacketType::Subscribe,
            9 => MqttControlPacketType::Suback,
            10 => MqttControlPacketType::Unsubscribe,
            11 => MqttControlPacketType::Unsuback,
            12 => MqttControlPacketType::Pingreq,
            13 => MqttControlPacketType::Pingresp,
            14 => MqttControlPacketType::Disconnect,
            _ => panic!("Invalid control packet type"),
        };
        let flags = first_byte & 0x0f;
        Ok((input, MqttFixedHeader { control_packet_type, flags }))
    }
}

#[derive(Debug)]
struct MqttConnect {
    protocol_name: String,
    protocol_level: u8,
    connect_flags: u8,
    keep_alive: u16,
    properties: Option<MqttProperties>,
    client_id: String,
    will_topic: Option<String>,
    will_message: Option<String>,
    username: Option<String>,
    password: Option<String>,
}

impl MqttConnect {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, protocol_name) = map(take_while(|c| c != 0), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
        let (input, _) = be_u8(input)?; // protocol level
        let (input, connect_flags) = be_u8(input)?;
        let (input, keep_alive) = be_u16(input)?;
        let (input, properties) = opt(MqttProperties::parse)(input)?;
        let (input, client_id) = map(take_while(|c| c != 0), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
        let (input, will_topic) = opt(map(take_while(|c| c != 0), |v: &[u8]| String::from_utf8_lossy(v).into_owned()))(input)?;
        let (input, will_message) = opt(map(take_while(|c| c != 0), |v: &[u8]| String::from_utf8_lossy(v).into_owned()))(input)?;
        let (input, username) = opt(map(take_while(|c| c != 0), |v: &[u8]| String::from_utf8_lossy(v).into_owned()))(input)?;
        let (input, password) = opt(map(take_while(|c| c != 0), |v: &[u8]| String::from_utf8_lossy(v).into_owned()))(input)?;
        Ok((input, MqttConnect {
            protocol_name,
            protocol_level: 5,
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
}

#[derive(Debug)]
struct MqttProperties {
    session_expiry_interval: Option<u32>,
    receive_maximum: Option<u16>,
    maximum_packet_size: Option<u32>,
    topic_alias_maximum: Option<u16>,
    request_response_information: Option<u8>,
    request_problem_information: Option<u8>,
    user_properties: Vec<(String, String)>,
    authentication_method: Option<String>,
    authentication_data: Option<Vec<u8>>,
}

impl MqttProperties {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let mut session_expiry_interval = None;
        let mut receive_maximum = None;
        let mut maximum_packet_size = None;
        let mut topic_alias_maximum = None;
        let mut request_response_information = None;
        let mut request_problem_information = None;
        let mut user_properties = Vec::new();
        let mut authentication_method = None;
        let mut authentication_data = None;
        let (input, properties) = length_data(be_u16)(input)?;
        let mut properties = properties;
        while !properties.is_empty() {
            let (properties_remaining, property_id) = be_u8(properties)?;
            properties = properties_remaining;
            match property_id {
                0x11 => {
                    let (properties_remaining, value) = be_u32(properties)?;
                    session_expiry_interval = Some(value);
                    properties = properties_remaining;
                }
                0x12 => {
                    let (properties_remaining, value) = be_u16(properties)?;
                    receive_maximum = Some(value);
                    properties = properties_remaining;
                }
                0x13 => {
                    let (properties_remaining, value) = be_u32(properties)?;
                    maximum_packet_size = Some(value);
                    properties = properties_remaining;
                }
                0x14 => {
                    let (properties_remaining, value) = be_u16(properties)?;
                    topic_alias_maximum = Some(value);
                    properties = properties_remaining;
                }
                0x15 => {
                    let (properties_remaining, value) = be_u8(properties)?;
                    request_response_information = Some(value);
                    properties = properties_remaining;
                }
                0x16 => {
                    let (properties_remaining, value) = be_u8(properties)?;
                    request_problem_information = Some(value);
                    properties = properties_remaining;
                }
                0x17 => {
                    let (properties_remaining, key) = map(take_while(|c| c != 0), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(properties)?;
                    let (properties_remaining, value) = map(take_while(|c| c != 0), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(properties_remaining)?;
                    user_properties.push((key, value));
                    properties = properties_remaining;
                }
                0x18 => {
                    let (properties_remaining, value) = map(take_while(|c| c != 0), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(properties)?;
                    authentication_method = Some(value);
                    properties = properties_remaining;
                }
                0x19 => {
                    let (properties_remaining, value) = length_data(be_u16)(properties)?;
                    authentication_data = Some(value.to_vec());
                    properties = properties_remaining;
                }
                _ => panic!("Invalid property ID"),
            }
        }
        Ok((input, MqttProperties {
            session_expiry_interval,
            receive_maximum,
            maximum_packet_size,
            topic_alias_maximum,
            request_response_information,
            request_problem_information,
            user_properties,
            authentication_method,
            authentication_data,
        }))
    }
}

fn take_while<F>(f: F) -> impl Fn(&[u8]) -> IResult<&[u8], &[u8]>
where
    F: Fn(u8) -> bool,
{
    move |input: &[u8]| {
        let mut i = 0;
        while i < input.len() && f(input[i]) {
            i += 1;
        }
        Ok((&input[i..], &input[..i]))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let input_file = &args[1];
    let input = fs::read(input_file).expect("Failed to read input file");
    let (input, fixed_header) = MqttFixedHeader::parse(&input).expect("Failed to parse fixed header");
    match fixed_header.control_packet_type {
        MqttControlPacketType::Connect => {
            let (_input, connect) = MqttConnect::parse(input).expect("Failed to parse connect packet");
            println!("{:?}", connect);
        }
        _ => panic!("Only CONNECT packets are supported"),
    }
}