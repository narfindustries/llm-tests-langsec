use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::length_data,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    collections::HashMap,
    env,
    fs::File,
    io::Read,
};

#[derive(Debug)]
struct MqttPacket {
    fixed_header: FixedHeader,
    variable_header: VariableHeader,
    payload: Payload,
}

#[derive(Debug)]
struct FixedHeader {
    packet_type: u8,
    flags: u8,
    remaining_length: u32,
}

#[derive(Debug)]
enum VariableHeader {
    Connect {
        protocol_name: String,
        protocol_level: u8,
        connect_flags: u8,
        keep_alive: u16,
        properties: Properties,
    },
    Connack {
        session_present: u8,
        reason_code: u8,
        properties: Properties,
    },
    Publish {
        topic_name: String,
        packet_identifier: Option<u16>,
        properties: Properties,
    },
    Puback {
        packet_identifier: u16,
        reason_code: u8,
        properties: Properties,
    },
    Pubrec {
        packet_identifier: u16,
        reason_code: u8,
        properties: Properties,
    },
    Pubrel {
        packet_identifier: u16,
        reason_code: u8,
        properties: Properties,
    },
    Pubcomp {
        packet_identifier: u16,
        reason_code: u8,
        properties: Properties,
    },
    Subscribe {
        packet_identifier: u16,
        properties: Properties,
    },
    Suback {
        packet_identifier: u16,
        reason_codes: Vec<u8>,
        properties: Properties,
    },
    Unsubscribe {
        packet_identifier: u16,
        properties: Properties,
    },
    Unsuback {
        packet_identifier: u16,
        reason_codes: Vec<极8>,
        properties: Properties,
    },
    Pingreq,
    Pingresp,
    Disconnect {
        reason_code: u8,
        properties: Properties,
    },
    Auth {
        reason_code: u8,
        properties: Properties,
    },
}

#[derive(Debug)]
enum Payload {
    Connect {
        client_id: String,
        will_topic: Option<String>,
        will_message: Option<Vec<u8>>,
        username: Option<String>,
        password: Option<Vec<u8>>,
    },
    Publish {
        message: Vec<u8>,
    },
    Subscribe {
        topic_filters: Vec<(String, u8)>,
    },
    Unsubscribe {
        topic_filters: Vec<String>,
    },
    None,
}

#[derive(Debug)]
struct Properties {
    payload_format_indicator: Option<u8>,
    message_expiry_interval: Option<u32>,
    topic_alias: Option<u16>,
    response_topic: Option<String>,
    correlation_data: Option<Vec<u8>>,
    subscription_identifier: Option<u32>,
    content_type: Option<String>,
    session_expiry_interval: Option极u32>,
    receive_maximum: Option<u16>,
    maximum_packet_size: Option<u32>,
    topic_alias_maximum: Option<u16>,
    request_response_information: Option<u8>,
    request_problem_information: Option<u8>,
    user_properties: HashMap<String, String>,
    authentication_method: Option<String>,
    authentication_data: Option<Vec<u8>>,
    reason_string: Option<String>,
    server_reference: Option<String>,
}

fn parse_variable_byte_integer(input: &[u8]) -> IResult<&[u8], u32> {
    let mut value = 0u32;
    let mut shift = 0;
    let mut byte;
    let mut remaining = input;

    loop {
        (remaining, byte) = be_u8(remaining)?;
        value |= ((byte & 0x7F) as u32) << shift;
        if (byte & 0x80) == 0 {
            break;
        }
        shift += 7;
        if shift > 28 {
            return Err(nom::Err::Error((input, nom::error::ErrorKind::TooLarge)));
        }
    }

    Ok((remaining, value))
}

fn parse_utf8_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, len) = be_u16(input)?;
    let (input, data) = take(len)(input)?;
    let string = String::from_utf8_lossy(data).into_owned();
    Ok((input, string))
}

fn parse_properties(input: &[u8]) -> IResult<&[u8], Properties> {
    let mut properties = Properties {
        payload_format_indicator: None,
        message_expiry_interval: None,
        topic_alias: None,
        response_topic: None,
        correlation_data: None,
        subscription_identifier: None,
        content_type: None,
        session_expiry_interval: None,
        receive_maximum: None,
        maximum_packet_size: None,
        topic_alias_maximum: None,
        request_response_information: None,
        request_problem_information: None,
        user_properties: HashMap::new(),
        authentication_method: None,
        authentication_data: None,
        reason_string: None,
        server_reference: None,
    };

    let (mut input, properties_length) = parse_variable_byte_integer(input)?;
    let mut bytes_read = 0;

    while bytes_read < properties_length {
        let (remaining, identifier) = be_u8(input)?;
        input = remaining;
        bytes_read += 1;

        match identifier {
            0x01 => {
                let (remaining, value) = be_u8(input)?;
                properties.payload_format_indicator = Some(value);
                input = remaining;
                bytes_read += 1;
            }
            0x02 => {
                let (remaining, value) = be_u32(input)?;
                properties.message_expiry_interval = Some(value);
                input = remaining;
                bytes_read += 4;
            }
            0x03 => {
                let (remaining, value) = be_u16(input)?;
                properties.topic_alias = Some(value);
                input = remaining;
                bytes_read += 2;
            }
            0x08 => {
                let (remaining, value) = parse_utf8_string(input)?;
                properties.response_topic = Some(value);
                input = remaining;
                bytes_read += value.len() as u32 + 2;
            }
            0x09 => {
                let (remaining, value) = length_data(be_u16)(input)?;
                properties.correlation_data = Some(value.to_vec());
                input = remaining;
                bytes_read += value.len() as u32 + 2;
            }
            0x0B => {
                let (remaining, value) = parse_variable_byte_integer(input)?;
                properties.subscription_identifier = Some(value);
                input = remaining;
                bytes_read += (value.to_be_bytes().len() as u32) + 1;
            }
            0x03 => {
                let (remaining, value) = parse_utf8_string(input)?;
                properties.content_type = Some(value);
                input = remaining;
                bytes_read += value.len() as u32 + 2;
            }
            0x11 => {
                let (remaining, value) = be_u32(input)?;
                properties.session_expiry_interval = Some(value);
                input = remaining;
                bytes_read += 4;
            }
            0x21 => {
                let (remaining, value) = be_u16(input)?;
                properties.receive_maximum = Some(value);
                input = remaining;
                bytes_read += 2;
            }
            0x27 => {
                let (remaining, value) = be_u32(input)?;
                properties.maximum_packet_size = Some(value);
                input = remaining;
                bytes_read += 4;
            }
            0x22 => {
                let (remaining, value) = be_u16(input)?;
                properties.topic_alias_maximum = Some(value);
                input = remaining;
                bytes_read += 2;
            }
            0x19 => {
                let (remaining, value) = be_u8(input)?;
                properties.request_response_information = Some(value);
                input = remaining;
                bytes_read += 1;
            }
            0x17 => {
                let (remaining, value) = be_u8(input)?;
                properties.request_problem_information = Some(value);
                input = remaining;
                bytes_read += 1;
            }
            0x26 => {
                let (remaining, key) = parse_utf8_string(input)?;
                let (remaining, value) = parse_utf8_string(remaining)?;
                properties.user_properties.insert(key, value);
                input = remaining;
                bytes_read += key.len() as u32 + value.len() as u32 + 4;
            }
            0x15 => {
                let (remaining, value) = parse_utf8_string(input)?;
                properties.authentication_method = Some(value);
                input = remaining;
                bytes_read += value.len() as u32 + 2;
            }
            0x16 => {
                let (remaining, value) = length_data(be_u16)(input)?;
                properties.authentication_data = Some(value.to_vec());
                input = remaining