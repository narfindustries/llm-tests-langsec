use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::take_while_m_n,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    fs::File,
    io::{Read, stdin},
    path::Path,
};

// Define MQTT packet types
#[derive(Debug, PartialEq)]
enum PacketType {
    Connect,
    ConnAck,
    Publish,
    PubAck,
    PubRec,
    PubRel,
    PubComp,
    Subscribe,
    SubAck,
    Unsubscribe,
    UnsubAck,
    PingReq,
    PingResp,
    Disconnect,
}

// Define MQTT QoS levels
#[derive(Debug, PartialEq)]
enum QoS {
    AtMostOnce,
    AtLeastOnce,
    ExactlyOnce,
}

// Define MQTT connect flags
#[derive(Debug, PartialEq)]
struct ConnectFlags {
    username: bool,
    password: bool,
    will_retain: bool,
    will_qos: QoS,
    will_flag: bool,
    clean_session: bool,
}

// Define MQTT connect packet
#[derive(Debug, PartialEq)]
struct ConnectPacket {
    protocol_name: String,
    protocol_version: u8,
    flags: ConnectFlags,
    keep_alive: u16,
    client_id: String,
    will_topic: Option<String>,
    will_message: Option<String>,
    username: Option<String>,
    password: Option<String>,
}

// Define MQTT connack packet
#[derive(Debug, PartialEq)]
struct ConnAckPacket {
    session_present: bool,
    return_code: u8,
}

// Define MQTT publish packet
#[derive(Debug, PartialEq)]
struct PublishPacket {
    topic: String,
    packet_id: Option<u16>,
    payload: Vec<u8>,
}

// Define MQTT publish acknowledgement packet
#[derive(Debug, PartialEq)]
struct PubAckPacket {
    packet_id: u16,
}

// Define MQTT packet
#[derive(Debug, PartialEq)]
enum Packet {
    Connect(ConnectPacket),
    ConnAck(ConnAckPacket),
    Publish(PublishPacket),
    PubAck(PubAckPacket),
}

// Implement parser for PacketType
fn packet_type(input: &[u8]) -> IResult<&[u8], PacketType> {
    map(be_u8, |x| match x >> 4 {
        1 => PacketType::Connect,
        2 => PacketType::ConnAck,
        3 => PacketType::Publish,
        4 => PacketType::PubAck,
        5 => PacketType::PubRec,
        6 => PacketType::PubRel,
        7 => PacketType::PubComp,
        8 => PacketType::Subscribe,
        9 => PacketType::SubAck,
        10 => PacketType::Unsubscribe,
        11 => PacketType::UnsubAck,
        12 => PacketType::PingReq,
        13 => PacketType::PingResp,
        14 => PacketType::Disconnect,
        _ => unreachable!(),
    })(input)
}

// Implement parser for QoS
fn qos(input: &[u8]) -> IResult<&[u8], QoS> {
    map(be_u8, |x| match x & 0b00000011 {
        0 => QoS::AtMostOnce,
        1 => QoS::AtLeastOnce,
        2 => QoS::ExactlyOnce,
        _ => unreachable!(),
    })(input)
}

// Implement parser for ConnectFlags
fn connect_flags(input: &[u8]) -> IResult<&[u8], ConnectFlags> {
    map(be_u8, |x| ConnectFlags {
        username: (x & 0b10000000) != 0,
        password: (x & 0b01000000) != 0,
        will_retain: (x & 0b00100000) != 0,
        will_qos: match (x & 0b00011000) >> 3 {
            0 => QoS::AtMostOnce,
            1 => QoS::AtLeastOnce,
            2 => QoS::ExactlyOnce,
            _ => unreachable!(),
        },
        will_flag: (x & 0b00000100) != 0,
        clean_session: (x & 0b00000010) != 0,
    })(input)
}

// Implement parser for ConnectPacket
fn connect_packet(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, _) = tag(b"MQTT\x04")(input)?;
    let (input, protocol_version) = be_u8(input)?;
    let (input, flags) = connect_flags(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, client_id_length) = be_u16(input)?;
    let (input, client_id) = take(client_id_length)(input)?;
    let client_id = String::from_utf8(client_id.to_vec()).unwrap();
    let (input, will_topic_length) = if flags.will_flag {
        be_u16(input)?
    } else {
        (input, 0)
    };
    let (input, will_topic) = if flags.will_flag {
        let (input, will_topic) = take(will_topic_length)(input)?;
        (input, Some(String::from_utf8(will_topic.to_vec()).unwrap()))
    } else {
        (input, None)
    };
    let (input, will_message_length) = if flags.will_flag {
        be_u16(input)?
    } else {
        (input, 0)
    };
    let (input, will_message) = if flags.will_flag {
        let (input, will_message) = take(will_message_length)(input)?;
        (input, Some(String::from_utf8(will_message.to_vec()).unwrap()))
    } else {
        (input, None)
    };
    let (input, username_length) = if flags.username {
        be_u16(input)?
    } else {
        (input, 0)
    };
    let (input, username) = if flags.username {
        let (input, username) = take(username_length)(input)?;
        (input, Some(String::from_utf8(username.to_vec()).unwrap()))
    } else {
        (input, None)
    };
    let (input, password_length) = if flags.password {
        be_u16(input)?
    } else {
        (input, 0)
    };
    let (input, password) = if flags.password {
        let (input, password) = take(password_length)(input)?;
        (input, Some(String::from_utf8(password.to_vec()).unwrap()))
    } else {
        (input, None)
    };
    Ok((
        input,
        ConnectPacket {
            protocol_name: "MQTT".to_string(),
            protocol_version,
            flags,
            keep_alive,
            client_id,
            will_topic,
            will_message,
            username,
            password,
        },
    ))
}

// Implement parser for ConnAckPacket
fn connack_packet(input: &[u8]) -> IResult<&[u8], ConnAckPacket> {
    let (input, _) = be_u8(input)?;
    let (input, session_present) = map(be_u8, |x| (x & 0b00000001) != 0)(input)?;
    let (input, return_code) = be_u8(input)?;
    Ok((
        input,
        ConnAckPacket {
            session_present,
            return_code,
        },
    ))
}

// Implement parser for PublishPacket
fn publish_packet(input: &[u8]) -> IResult<&[u8], PublishPacket> {
    let (input, topic_length) = be_u16(input)?;
    let (input, topic) = take(topic_length)(input)?;
    let topic = String::from_utf8(topic.to_vec()).unwrap();
    let (input, packet_id) = if (input[0] & 0b00000011) == 0b00000001 || (input[0] & 0b00000011) == 0b00000010 {
        be_u16(input)?
    } else {
        (input, None)
    };
    let (input, payload_length) = if (input[0] & 0b00000011) == 0b00000001 || (input[0] & 0b00000011) == 0b00000010 {
        let (input, remainder_length) = be_u16(input)?;
        (input, remainder_length - 2)
    } else {
        (input, 0)
    };
    let (input, payload) = take(payload_length)(input)?;
    Ok((
        input,
        PublishPacket {
            topic,
            packet_id,
            payload: payload.to_vec(),
        },
    ))
}

// Implement parser for PubAckPacket
fn puback_packet(input: &[u8]) -> IResult<&[u8], PubAckPacket> {
    let (input, _) = be_u8(input)?;
    let (input, packet_id) = be_u16(input)?;
    Ok((input, PubAckPacket { packet_id }))
}

// Implement parser for Packet
fn packet(input: &[u8]) -> IResult<&[u8], Packet> {
    let (input, packet_type) = packet_type(input)?;
    match packet_type {
        PacketType::Connect => {
            let (input, packet) = connect_packet(input)?;
            Ok((input, Packet::Connect(packet)))
        }
        PacketType::ConnAck => {
            let (input, packet) = connack_packet(input)?;
            Ok((input, Packet::ConnAck(packet)))
        }
        PacketType::Publish => {
            let (input, packet) = publish_packet(input)?;
            Ok((input, Packet::Publish(packet)))
        }
        PacketType::PubAck => {
            let (input, packet) = puback_packet(input)?;
            Ok((input, Packet::PubAck(packet)))
        }
        _ => unimplemented!(),
    }
}

// Main function to parse MQTT packet from file
fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }
    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", path.display(), why),
        Ok(file) => file,
    };
    let mut input = Vec::new();
    file.read_to_end(&mut input).unwrap();
    match packet(&input) {
        Ok((remaining, packet)) => println!("{:?}", packet),
        Err(err) => println!("Error: {:?}", err),
    }
}