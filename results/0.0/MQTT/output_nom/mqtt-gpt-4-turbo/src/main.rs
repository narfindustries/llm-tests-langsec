use nom::{
    bits::{bits, complete::take as take_bits},
    bytes::complete::{take, take_while},
    combinator::{map, map_res, opt},
    multi::count,
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
enum QoS {
    AtMostOnce,
    AtLeastOnce,
    ExactlyOnce,
}

#[derive(Debug)]
enum ControlPacketType {
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
    packet_type: ControlPacketType,
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
enum MQTTControlPacket {
    Connect(ConnectPacket),
    // Other packet types omitted for brevity
}

fn parse_qos(input: (u8, usize)) -> IResult<(u8, usize), QoS> {
    let (input, qos) = take_bits(2usize)(input)?;
    match qos {
        0 => Ok((input, QoS::AtMostOnce)),
        1 => Ok((input, QoS::AtLeastOnce)),
        2 => Ok((input, QoS::ExactlyOnce)),
        _ => unreachable!(),
    }
}

fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, length) = map_res(take(2usize), |s: &[u8]| {
        u16::from_be_bytes([s[0], s[1]]) as usize
    })(input)?;
    map_res(take(length), std::str::from_utf8)(input).map(|(next_input, result)| (next_input, result.to_string()))
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (input, (packet_type, flags, remaining_length)) = bits(tuple((
        take_bits(4usize),
        take_bits(4usize),
        parse_variable_byte_integer,
    )))(input)?;

    let packet_type = match packet_type {
        1 => ControlPacketType::Connect,
        2 => ControlPacketType::Connack,
        3 => ControlPacketType::Publish,
        4 => ControlPacketType::Puback,
        5 => ControlPacketType::Pubrec,
        6 => ControlPacketType::Pubrel,
        7 => ControlPacketType::Pubcomp,
        8 => ControlPacketType::Subscribe,
        9 => ControlPacketType::Suback,
        10 => ControlPacketType::Unsubscribe,
        11 => ControlPacketType::Unsuback,
        12 => ControlPacketType::Pingreq,
        13 => ControlPacketType::Pingresp,
        14 => ControlPacketType::Disconnect,
        15 => ControlPacketType::Auth,
        _ => unreachable!(),
    };

    Ok((input, FixedHeader {
        packet_type,
        flags,
        remaining_length,
    }))
}

fn parse_variable_byte_integer(input: (u8, usize)) -> IResult<(u8, usize), usize> {
    let mut value = 0;
    let mut multiplier = 1;
    let mut input = input;
    loop {
        let (next_input, byte) = take_bits(8usize)(input)?;
        input = next_input;
        value += (byte & 127) as usize * multiplier;
        if byte & 128 == 0 {
            break;
        }
        multiplier *= 128;
    }
    Ok((input, value))
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, protocol_name) = parse_string(input)?;
    let (input, protocol_level) = take(1usize)(input)?;
    let (input, connect_flags) = take(1usize)(input)?;
    let (input, keep_alive) = map(take(2usize), |s: &[u8]| u16::from_be_bytes([s[0], s[1]]))(input)?;
    let (input, client_id) = parse_string(input)?;

    let (input, will_topic) = opt(preceded(take(1usize), parse_string))(input)?;
    let (input, will_message) = opt(preceded(take(1usize), parse_string))(input)?;
    let (input, username) = opt(preceded(take(1usize), parse_string))(input)?;
    let (input, password) = opt(preceded(take(1usize), parse_string))(input)?;

    Ok((input, ConnectPacket {
        protocol_name,
        protocol_level: protocol_level[0],
        connect_flags: connect_flags[0],
        keep_alive,
        client_id,
        will_topic,
        will_message: will_message.map(|s| s.into_bytes()),
        username,
        password: password.map(|s| s.into_bytes()),
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No input file specified"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_fixed_header(&buffer) {
        Ok((input, header)) => {
            println!("Parsed Fixed Header: {:?}", header);
            match header.packet_type {
                ControlPacketType::Connect => {
                    if let Ok((_, connect_packet)) = parse_connect_packet(input) {
                        println!("Parsed Connect Packet: {:?}", connect_packet);
                    }
                },
                _ => println!("Other packet types not implemented"),
            }
        },
        Err(e) => println!("Error parsing MQTT packet: {:?}", e),
    }

    Ok(())
}