use std::fs::File;
use std::io::{self, Read};
use std::env;
use nom::{
    IResult,
    bytes::streaming::{tag, take},
    number::streaming::{be_u8, be_u16},
    combinator::{map, map_res, opt},
    sequence::{preceded, tuple},
    multi::length_data,
};

#[derive(Debug)]
enum MQTTControlPacketType {
    CONNECT,
    CONNACK,
    PUBLISH,
    PUBACK,
    PUBREC,
    PUBREL,
    PUBCOMP,
    SUBSCRIBE,
    SUBACK,
    UNSUBSCRIBE,
    UNSUBACK,
    PINGREQ,
    PINGRESP,
    DISCONNECT,
    AUTH,
    Reserved(u8),
}

impl From<u8> for MQTTControlPacketType {
    fn from(byte: u8) -> Self {
        match byte {
            1 => MQTTControlPacketType::CONNECT,
            2 => MQTTControlPacketType::CONNACK,
            3 => MQTTControlPacketType::PUBLISH,
            4 => MQTTControlPacketType::PUBACK,
            5 => MQTTControlPacketType::PUBREC,
            6 => MQTTControlPacketType::PUBREL,
            7 => MQTTControlPacketType::PUBCOMP,
            8 => MQTTControlPacketType::SUBSCRIBE,
            9 => MQTTControlPacketType::SUBACK,
            10 => MQTTControlPacketType::UNSUBSCRIBE,
            11 => MQTTControlPacketType::UNSUBACK,
            12 => MQTTControlPacketType::PINGREQ,
            13 => MQTTControlPacketType::PINGRESP,
            14 => MQTTControlPacketType::DISCONNECT,
            15 => MQTTControlPacketType::AUTH,
            _ => MQTTControlPacketType::Reserved(byte),
        }
    }
}

#[derive(Debug)]
struct MQTTFixedHeader {
    packet_type: MQTTControlPacketType,
    flags: u8,
    remaining_length: u64,
}

fn variable_length_integer(input: &[u8]) -> IResult<&[u8], u64> {
    let mut multiplier: u64 = 1;
    let mut value: u64 = 0;
    let mut idx: usize = 0;

    for &encoded_byte in input.iter() {
        let byte_value = encoded_byte & 0x7F;
        value += byte_value as u64 * multiplier;
        multiplier *= 128;
        idx += 1;

        if idx > 4 {
            return Err(nom::Err::Failure((input, nom::error::ErrorKind::TooLarge)));
        }
        
        if encoded_byte & 0x80 == 0 {
            return Ok((&input[idx..], value));
        }
    }
    
    Err(nom::Err::Incomplete(nom::Needed::new(1)))
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], MQTTFixedHeader> {
    let (input, byte1) = be_u8(input)?;
    let packet_type = MQTTControlPacketType::from(byte1 >> 4);
    let flags = byte1 & 0x0F;
    let (input, remaining_length) = variable_length_integer(input)?;
    
    Ok((input, MQTTFixedHeader { packet_type, flags, remaining_length }))
}

// Placeholder for variable header and payload parsing
#[derive(Debug)]
enum MQTTVariableHeader {
    Default,
    Publish { topic_name: String, packet_id: Option<u16> },
}

#[derive(Debug)]
enum MQTTPayload {
    None,
    Bytes(Vec<u8>),
    String(String),
}

#[derive(Debug)]
struct MQTTPacket {
    fixed_header: MQTTFixedHeader,
    variable_header: MQTTVariableHeader,
    payload: MQTTPayload,
}

fn parse_variable_header_and_payload<'a>(
    input: &'a [u8],
    fixed_header: &MQTTFixedHeader,
) -> IResult<&'a [u8], (MQTTVariableHeader, MQTTPayload)> {
    match fixed_header.packet_type {
        MQTTControlPacketType::PUBLISH => {
            let (input, topic_length) = be_u16(input)?;
            let (input, topic_name) = map_res(take(topic_length), |bytes: &[u8]| {
                std::str::from_utf8(bytes).map(|s| s.to_string())
            })(input)?;
            
            let (input, packet_id) = opt(be_u16)(input)?;
            let variable_header = MQTTVariableHeader::Publish { topic_name, packet_id };
            let payload_length = fixed_header.remaining_length - (2 + topic_name.len() as u64 + if packet_id.is_some() { 2 } else { 0 });
            let (input, payload) = map(take(payload_length), |bytes: &[u8]| {
                MQTTPayload::Bytes(bytes.to_vec())
            })(input)?;

            Ok((input, (variable_header, payload)))
        }
        _ => Ok((input, (MQTTVariableHeader::Default, MQTTPayload::None))),
    }
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MQTTPacket> {
    let (input, fixed_header) = parse_fixed_header(input)?;
    let (input, (variable_header, payload)) = parse_variable_header_and_payload(input, &fixed_header)?;

    Ok((input, MQTTPacket { fixed_header, variable_header, payload }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    
    if args.len() != 2 {
        eprintln!("Usage: mqtt_parser <file>");
        return Ok(());
    }

    let data_filename = &args[1];
    let mut file = File::open(data_filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse MQTT packet: {:?}", e),
    }

    Ok(())
}