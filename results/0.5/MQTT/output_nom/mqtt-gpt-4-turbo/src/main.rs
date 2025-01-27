use nom::{
    bits::{bits, complete::take as take_bits},
    bytes::complete::{take, tag},
    combinator::{map_res, opt},
    multi::count,
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

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

fn parse_qos(input: (u8, usize)) -> IResult<(u8, usize), QoS> {
    match input {
        (i, 0) => Ok((i, QoS::AtMostOnce)),
        (i, 1) => Ok((i, QoS::AtLeastOnce)),
        (i, 2) => Ok((i, QoS::ExactlyOnce)),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, length) = map_res(take(2usize), |s: &[u8]| {
        u16::from_be_bytes([s[0], s[1]]) as usize
    })(input)?;
    map_res(take(length), std::str::from_utf8)(input)
        .map(|(next_input, result)| (next_input, result.to_string()))
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, protocol_name) = parse_string(input)?;
    let (input, protocol_level) = take(1usize)(input)?;
    let (input, connect_flags) = take(1usize)(input)?;
    let (input, keep_alive) = map_res(take(2usize), |s: &[u8]| {
        u16::from_be_bytes([s[0], s[1]])
    })(input)?;

    let (input, client_id) = parse_string(input)?;

    let (input, will_topic) = opt(preceded(tag([0b00000100]), parse_string))(input)?;
    let (input, will_message) = opt(preceded(tag([0b00000100]), take(2usize)))(input)?;

    let (input, username) = opt(preceded(tag([0b10000000]), parse_string))(input)?;
    let (input, password) = opt(preceded(tag([0b01000000]), take(2usize)))(input)?;

    Ok((
        input,
        ConnectPacket {
            protocol_name,
            protocol_level: protocol_level[0],
            connect_flags: connect_flags[0],
            keep_alive,
            client_id,
            will_topic,
            will_message: will_message.map(|wm| wm.to_vec()),
            username,
            password: password.map(|p| p.to_vec()),
        },
    ))
}

fn parse_mqtt(input: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (input, (packet_type, flags)) = bits::<_, _, nom::error::Error<_>, _, _>(tuple((
        take_bits(4usize),
        take_bits(4usize),
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
        _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    };

    let (input, remaining_length) = map_res(
        count(take_bits::<u8, _, _>(7usize), 4),
        |list: Vec<u8>| {
            let mut result = 0;
            let mut multiplier = 1;
            for byte in list.iter().rev() {
                result += (*byte as usize) * multiplier;
                multiplier *= 128;
            }
            Ok(result)
        },
    )(input)?;

    Ok((
        input,
        FixedHeader {
            packet_type,
            flags,
            remaining_length,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "No file path provided",
        ));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt(&buffer) {
        Ok((_, header)) => {
            println!("{:?}", header);
            if let ControlPacketType::Connect = header.packet_type {
                if let Ok((_, connect_packet)) = parse_connect_packet(&buffer[2..]) {
                    println!("{:?}", connect_packet);
                }
            }
        }
        Err(e) => println!("Failed to parse MQTT packet: {:?}", e),
    }

    Ok(())
}