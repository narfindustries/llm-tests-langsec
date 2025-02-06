use nom::{
    bytes::complete::take,
    combinator::map,
    multi::length_data,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{
    collections::HashMap,
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct MqttPacket {
    packet_type: u8,
    flags: u8,
    remaining_length: usize,
    variable_header: Vec<u8>,
    payload: Vec<u8>,
}

#[derive(Debug)]
struct ConnectPacket {
    protocol_name: String,
    protocol_level: u8,
    connect_flags: u8,
    keep_alive: u16,
    properties: ConnectProperties,
    client_id: String,
    will_topic: Option<String>,
    will_message: Option<Vec<u8>>,
    username: Option<String>,
    password: Option<Vec<u8>>,
}

#[derive(Debug)]
struct ConnectProperties {
    session_expiry_interval: Option<u32>,
    receive_maximum: Option<u16>,
    maximum_packet_size: Option<u32>,
    topic_alias_maximum: Option<u16>,
    request_response_information: Option<u8>,
    request_problem_information: Option<u8>,
    user_properties: HashMap<String, String>,
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (packet_type, flags)) = parse_fixed_header(input)?;
    let (input, remaining_length) = parse_remaining_length(input)?;
    let (input, variable_header) = take(remaining_length)(input)?;
    let (input, payload) = take(input.len())(input)?;

    Ok((
        input,
        MqttPacket {
            packet_type,
            flags,
            remaining_length,
            variable_header: variable_header.to_vec(),
            payload: payload.to_vec(),
        },
    ))
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], (u8, u8)> {
    let (input, byte) = be_u8(input)?;
    Ok((input, (byte >> 4, byte & 0x0F)))
}

fn parse_remaining_length(mut input: &[u8]) -> IResult<&[u8], usize> {
    let mut length = 0;
    let mut shift = 0;
    loop {
        let (input_temp, byte) = be_u8(input)?;
        input = input_temp;
        length |= ((byte & 0x7F) as usize) << shift;
        if byte & 0x80 == 0 {
            break;
        }
        shift += 7;
    }
    Ok((input, length))
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, protocol_name) = parse_utf8_string(input)?;
    let (input, protocol_level) = be_u8(input)?;
    let (input, connect_flags) = be_u8(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, properties) = parse_connect_properties(input)?;
    let (input, client_id) = parse_utf8_string(input)?;
    let (input, will_topic) = if connect_flags & 0x04 != 0 {
        map(parse_utf8_string, Some)(input)?
    } else {
        (input, None)
    };
    let (input, will_message) = if connect_flags & 0x04 != 0 {
        map(length_data(be_u16), |data: &[u8]| Some(data.to_vec()))(input)?
    } else {
        (input, None)
    };
    let (input, username) = if connect_flags & 0x80 != 0 {
        map(parse_utf8_string, Some)(input)?
    } else {
        (input, None)
    };
    let (input, password) = if connect_flags & 0x40 != 0 {
        map(length_data(be_u16), |data: &[u8]| Some(data.to_vec()))(input)?
    } else {
        (input, None)
    };

    Ok((
        input,
        ConnectPacket {
            protocol_name,
            protocol_level,
            connect_flags,
            keep_alive,
            properties,
            client_id,
            will_topic,
            will_message,
            username,
            password,
        },
    ))
}

fn parse_connect_properties(input: &[u8]) -> IResult<&[u8], ConnectProperties> {
    let (input, properties_length) = be_u16(input)?;
    let (input, properties_data) = take(properties_length)(input)?;
    let (input, session_expiry_interval) = parse_optional_property(properties_data, 0x11, be_u32)?;
    let (input, receive_maximum) = parse_optional_property(input, 0x21, be_u16)?;
    let (input, maximum_packet_size) = parse_optional_property(input, 0x27, be_u32)?;
    let (input, topic_alias_maximum) = parse_optional_property(input, 0x22, be_u16)?;
    let (input, request_response_information) = parse_optional_property(input, 0x19, be_u8)?;
    let (input, request_problem_information) = parse_optional_property(input, 0x17, be_u8)?;
    let (input, user_properties) = parse_user_properties(input)?;

    Ok((
        input,
        ConnectProperties {
            session_expiry_interval,
            receive_maximum,
            maximum_packet_size,
            topic_alias_maximum,
            request_response_information,
            request_problem_information,
            user_properties,
        },
    ))
}

fn parse_optional_property<'a, T>(
    input: &'a [u8],
    id: u8,
    parser: fn(&'a [u8]) -> IResult<&'a [u8], T>,
) -> IResult<&'a [u8], Option<T>> {
    if input.is_empty() || input[0] != id {
        return Ok((input, None));
    }
    let (input, _) = take(1usize)(input)?;
    map(parser, Some)(input)
}

fn parse_user_properties(input: &[u8]) -> IResult<&[u8], HashMap<String, String>> {
    let mut properties = HashMap::new();
    let mut input = input;
    while !input.is_empty() && input[0] == 0x26 {
        let (input_temp, _) = take(1usize)(input)?;
        let (input_temp, key) = parse_utf8_string(input_temp)?;
        let (input_temp, value) = parse_utf8_string(input_temp)?;
        properties.insert(key, value);
        input = input_temp;
    }
    Ok((input, properties))
}

fn parse_utf8_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, length) = be_u16(input)?;
    let (input, str_bytes) = take(length)(input)?;
    let s = String::from_utf8_lossy(str_bytes).to_string();
    Ok((input, s))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => eprintln!("Error parsing MQTT packet: {:?}", e),
    }

    Ok(())
}