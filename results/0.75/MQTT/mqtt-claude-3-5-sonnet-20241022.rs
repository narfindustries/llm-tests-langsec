use nom::{
    bits::{bits, complete::take},
    bytes::complete::{tag, take as take_bytes},
    combinator::{map, verify},
    multi::length_data,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs, io::Read};

#[derive(Debug)]
enum PacketType {
    CONNECT = 1,
    CONNACK = 2,
    PUBLISH = 3,
    PUBACK = 4,
    PUBREC = 5,
    PUBREL = 6,
    PUBCOMP = 7,
    SUBSCRIBE = 8,
    SUBACK = 9,
    UNSUBSCRIBE = 10,
    UNSUBACK = 11,
    PINGREQ = 12,
    PINGRESP = 13,
    DISCONNECT = 14,
    AUTH = 15,
}

#[derive(Debug)]
struct FixedHeader {
    packet_type: PacketType,
    dup_flag: bool,
    qos_level: u8,
    retain: bool,
    remaining_length: u32,
}

#[derive(Debug)]
struct ConnectFlags {
    username_flag: bool,
    password_flag: bool,
    will_retain: bool,
    will_qos: u8,
    will_flag: bool,
    clean_start: bool,
}

#[derive(Debug)]
struct Property {
    identifier: u8,
    value: Vec<u8>,
}

#[derive(Debug)]
struct ConnectPacket {
    fixed_header: FixedHeader,
    protocol_name: String,
    protocol_version: u8,
    connect_flags: ConnectFlags,
    keep_alive: u16,
    properties: Vec<Property>,
    client_id: String,
    will_properties: Option<Vec<Property>>,
    will_topic: Option<String>,
    will_payload: Option<Vec<u8>>,
    username: Option<String>,
    password: Option<Vec<u8>>,
}

fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], u32> {
    let mut value: u32 = 0;
    let mut multiplier: u32 = 1;
    let mut current_byte: u8;
    let mut bytes_read = 0;
    
    loop {
        let (rest, byte) = be_u8(input)?;
        current_byte = byte;
        value += ((current_byte & 0x7F) as u32) * multiplier;
        multiplier *= 128;
        bytes_read += 1;
        
        if current_byte & 0x80 == 0 || bytes_read >= 4 {
            break;
        }
    }
    
    Ok((&input[bytes_read..], value))
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (input, (packet_type_bits, flags)): (&[u8], (u8, u8)) = bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
        take(4usize),
        take(4usize),
    )))(input)?;
    
    let (input, remaining_length) = parse_remaining_length(input)?;
    
    let packet_type = match packet_type_bits {
        1 => PacketType::CONNECT,
        2 => PacketType::CONNACK,
        3 => PacketType::PUBLISH,
        4 => PacketType::PUBACK,
        5 => PacketType::PUBREC,
        6 => PacketType::PUBREL,
        7 => PacketType::PUBCOMP,
        8 => PacketType::SUBSCRIBE,
        9 => PacketType::SUBACK,
        10 => PacketType::UNSUBSCRIBE,
        11 => PacketType::UNSUBACK,
        12 => PacketType::PINGREQ,
        13 => PacketType::PINGRESP,
        14 => PacketType::DISCONNECT,
        15 => PacketType::AUTH,
        _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    };
    
    Ok((input, FixedHeader {
        packet_type,
        dup_flag: (flags & 0x08) != 0,
        qos_level: (flags & 0x06) >> 1,
        retain: (flags & 0x01) != 0,
        remaining_length,
    }))
}

fn parse_utf8_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, len) = be_u16(input)?;
    let (input, bytes) = take_bytes(len as usize)(input)?;
    Ok((input, String::from_utf8_lossy(bytes).into_owned()))
}

fn parse_connect_flags(input: &[u8]) -> IResult<&[u8], ConnectFlags> {
    let (input, flags) = be_u8(input)?;
    Ok((input, ConnectFlags {
        username_flag: (flags & 0x80) != 0,
        password_flag: (flags & 0x40) != 0,
        will_retain: (flags & 0x20) != 0,
        will_qos: (flags & 0x18) >> 3,
        will_flag: (flags & 0x04) != 0,
        clean_start: (flags & 0x02) != 0,
    }))
}

fn parse_property(input: &[u8]) -> IResult<&[u8], Property> {
    let (input, identifier) = be_u8(input)?;
    let (input, len) = be_u16(input)?;
    let (input, value) = take_bytes(len as usize)(input)?;
    Ok((input, Property {
        identifier,
        value: value.to_vec(),
    }))
}

fn parse_properties(input: &[u8]) -> IResult<&[u8], Vec<Property>> {
    let (input, property_length) = be_u8(input)?;
    let mut properties = Vec::new();
    let mut remaining = property_length as usize;
    let mut current_input = input;
    
    while remaining > 0 {
        let (rest, property) = parse_property(current_input)?;
        remaining -= property.value.len() + 3; // 3 for identifier and length
        properties.push(property);
        current_input = rest;
    }
    
    Ok((current_input, properties))
}

fn parse_connect(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, fixed_header) = parse_fixed_header(input)?;
    let (input, _) = tag(b"MQTT")(input)?;
    let (input, protocol_version) = verify(be_u8, |&x| x == 5)(input)?;
    let (input, connect_flags) = parse_connect_flags(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, properties) = parse_properties(input)?;
    let (input, client_id) = parse_utf8_string(input)?;
    
    let (input, will_properties) = if connect_flags.will_flag {
        let (input, props) = parse_properties(input)?;
        (input, Some(props))
    } else {
        (input, None)
    };
    
    let (input, will_topic) = if connect_flags.will_flag {
        let (input, topic) = parse_utf8_string(input)?;
        (input, Some(topic))
    } else {
        (input, None)
    };
    
    let (input, will_payload) = if connect_flags.will_flag {
        let (input, len) = be_u16(input)?;
        let (input, payload) = take_bytes(len as usize)(input)?;
        (input, Some(payload.to_vec()))
    } else {
        (input, None)
    };
    
    let (input, username) = if connect_flags.username_flag {
        let (input, name) = parse_utf8_string(input)?;
        (input, Some(name))
    } else {
        (input, None)
    };
    
    let (input, password) = if connect_flags.password_flag {
        let (input, len) = be_u16(input)?;
        let (input, pass) = take_bytes(len as usize)(input)?;
        (input, Some(pass.to_vec()))
    } else {
        (input, None)
    };
    
    Ok((input, ConnectPacket {
        fixed_header,
        protocol_name: String::from("MQTT"),
        protocol_version,
        connect_flags,
        keep_alive,
        properties,
        client_id,
        will_properties,
        will_topic,
        will_payload,
        username,
        password,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <mqtt_binary_file>", args[0]);
        std::process::exit(1);
    }
    
    let mut file = fs::File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    
    match parse_connect(&buffer) {
        Ok((remaining, packet)) => {
            println!("Successfully parsed MQTT packet:");
            println!("{:#?}", packet);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse MQTT packet: {:?}", e);
        }
    }
    
    Ok(())
}