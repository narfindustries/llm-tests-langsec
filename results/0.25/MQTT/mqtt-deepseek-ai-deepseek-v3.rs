use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct MqttPacket {
    control_packet_type: u8,
    flags: u8,
    remaining_length: u32,
    payload: Vec<u8>,
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
struct ConnAckPacket {
    session_present: bool,
    connect_return_code: u8,
}

#[derive(Debug)]
struct PublishPacket {
    dup: bool,
    qos: u8,
    retain: bool,
    topic_name: String,
    packet_id: Option<u16>,
    payload: Vec<u8>,
}

#[derive(Debug)]
struct SubscribePacket {
    packet_id: u16,
    topic_filters: Vec<(String, u8)>,
}

#[derive(Debug)]
struct SubAckPacket {
    packet_id: u16,
    return_codes: Vec<u8>,
}

#[derive(Debug)]
struct UnsubscribePacket {
    packet_id: u16,
    topic_filters: Vec<String>,
}

#[derive(Debug)]
struct UnsubAckPacket {
    packet_id: u16,
}

#[derive(Debug)]
struct PingReqPacket {}

#[derive(Debug)]
struct PingRespPacket {}

#[derive(Debug)]
struct DisconnectPacket {
    reason_code: Option<u8>,
}

#[derive(Debug)]
struct AuthPacket {
    reason_code: Option<u8>,
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (control_packet_type, flags, remaining_length)) =
        tuple((be_u8, be_u8, parse_remaining_length))(input)?;
    let (input, payload) = take(remaining_length)(input)?;
    Ok((
        input,
        MqttPacket {
            control_packet_type,
            flags,
            remaining_length,
            payload: payload.to_vec(),
        },
    ))
}

fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], u32> {
    let mut value = 0;
    let mut shift = 0;
    let mut input = input;
    loop {
        let (new_input, byte) = be_u8(input)?;
        input = new_input;
        value |= ((byte & 0x7F) as u32) << shift;
        if (byte & 0x80) == 0 {
            break;
        }
        shift += 7;
    }
    Ok((input, value))
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, protocol_name) = parse_utf8_string(input)?;
    let (input, protocol_level) = be_u8(input)?;
    let (input, connect_flags) = be_u8(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, client_id) = parse_utf8_string(input)?;
    let will_topic = if (connect_flags & 0x04) != 0 {
        let (input, topic) = parse_utf8_string(input)?;
        Some(topic)
    } else {
        None
    };
    let will_message = if (connect_flags & 0x04) != 0 {
        let (input, message) = take(be_u16)(input)?;
        Some(message.to_vec())
    } else {
        None
    };
    let username = if (connect_flags & 0x80) != 0 {
        let (input, name) = parse_utf8_string(input)?;
        Some(name)
    } else {
        None
    };
    let password = if (connect_flags & 0x40) != 0 {
        let (input, pass) = take(be_u16)(input)?;
        Some(pass.to_vec())
    } else {
        None
    };
    Ok((
        input,
        ConnectPacket {
            protocol_name,
            protocol_level,
            connect_flags,
            keep_alive,
            client_id,
            will_topic,
            will_message,
            username,
            password,
        },
    ))
}

fn parse_conn_ack_packet(input: &[u8]) -> IResult<&[u8], ConnAckPacket> {
    let (input, session_present) = be_u8(input)?;
    let (input, connect_return_code) = be_u8(input)?;
    Ok((
        input,
        ConnAckPacket {
            session_present: session_present != 0,
            connect_return_code,
        },
    ))
}

fn parse_publish_packet(input: &[u8]) -> IResult<&[u8], PublishPacket> {
    let (input, topic_name) = parse_utf8_string(input)?;
    let (input, packet_id) = if (input[0] & 0x06) != 0 {
        let (input, id) = be_u16(input)?;
        Some(id)
    } else {
        None
    };
    let (input, payload) = take(input.len())(input)?;
    Ok((
        input,
        PublishPacket {
            dup: (input[0] & 0x08) != 0,
            qos: (input[0] & 0x06) >> 1,
            retain: (input[0] & 0x01) != 0,
            topic_name,
            packet_id,
            payload: payload.to_vec(),
        },
    ))
}

fn parse_subscribe_packet(input: &[u8]) -> IResult<&[u8], SubscribePacket> {
    let (input, packet_id) = be_u16(input)?;
    let mut topic_filters = Vec::new();
    let mut input = input;
    while input.len() > 0 {
        let (new_input, topic_filter) = parse_utf8_string(input)?;
        let (new_input, qos) = be_u8(new_input)?;
        topic_filters.push((topic_filter, qos));
        input = new_input;
    }
    Ok((
        input,
        SubscribePacket {
            packet_id,
            topic_filters,
        },
    ))
}

fn parse_sub_ack_packet(input: &[u8]) -> IResult<&[u8], SubAckPacket> {
    let (input, packet_id) = be_u16(input)?;
    let mut return_codes = Vec::new();
    let mut input = input;
    while input.len() > 0 {
        let (new_input, return_code) = be_u8(input)?;
        return_codes.push(return_code);
        input = new_input;
    }
    Ok((
        input,
        SubAckPacket {
            packet_id,
            return_codes,
        },
    ))
}

fn parse_unsubscribe_packet(input: &[u8]) -> IResult<&[u8], UnsubscribePacket> {
    let (input, packet_id) = be_u16(input)?;
    let mut topic_filters = Vec::new();
    let mut input = input;
    while input.len() > 0 {
        let (new_input, topic_filter) = parse_utf8_string(input)?;
        topic_filters.push(topic_filter);
        input = new_input;
    }
    Ok((
        input,
        UnsubscribePacket {
            packet_id,
            topic_filters,
        },
    ))
}

fn parse_unsub_ack_packet(input: &[u8]) -> IResult<&[u8], UnsubAckPacket> {
    let (input, packet_id) = be_u16(input)?;
    Ok((input, UnsubAckPacket { packet_id }))
}

fn parse_ping_req_packet(input: &[u8]) -> IResult<&[u8], PingReqPacket> {
    Ok((input, PingReqPacket {}))
}

fn parse_ping_resp_packet(input: &[u8]) -> IResult<&[u8], PingRespPacket> {
    Ok((input, PingRespPacket {}))
}

fn parse_disconnect_packet(input: &[u8]) -> IResult<&[u8], DisconnectPacket> {
    let (input, reason_code) = if input.len() > 0 {
        let (input, code) = be_u8(input)?;
        Some(code)
    } else {
        None
    };
    Ok((input, DisconnectPacket { reason_code }))
}

fn parse_auth_packet(input: &[u8]) -> IResult<&[u8], AuthPacket> {
    let (input, reason_code) = if input.len() > 0 {
        let (input, code) = be_u8(input)?;
        Some(code)
    } else {
        None
    };
    Ok((input, AuthPacket { reason_code }))
}

fn parse_utf8