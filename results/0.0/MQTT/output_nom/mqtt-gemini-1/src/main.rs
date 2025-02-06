use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, rest},
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum MqttPacketType {
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

impl From<u8> for MqttPacketType {
    fn from(byte: u8) -> Self {
        match byte {
            1 => MqttPacketType::Connect,
            2 => MqttPacketType::Connack,
            3 => MqttPacketType::Publish,
            4 => MqttPacketType::Puback,
            5 => MqttPacketType::Pubrec,
            6 => MqttPacketType::Pubrel,
            7 => MqttPacketType::Pubcomp,
            8 => MqttPacketType::Subscribe,
            9 => MqttPacketType::Suback,
            10 => MqttPacketType::Unsubscribe,
            11 => MqttPacketType::Unsuback,
            12 => MqttPacketType::Pingreq,
            13 => MqttPacketType::Pingresp,
            14 => MqttPacketType::Disconnect,
            _ => panic!("Invalid MQTT packet type"),
        }
    }
}


fn mqtt_packet(input: &[u8]) -> IResult<&[u8], (MqttPacketType, Vec<u8>)> {
    let (input, packet_type) = map(take(1u8), |b: &[u8]| MqttPacketType::from(b[0]))(input)?;
    let (input, remaining_length) = be_u32(input)?;
    let (input, payload) = take(remaining_length as usize)(input)?;
    Ok((input, (packet_type, payload.to_vec())))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match mqtt_packet(&buffer) {
        Ok((_, (packet_type, payload))) => {
            println!("Packet Type: {:?}", packet_type);
            println!("Payload: {:?}", payload);
            //Further parsing of payload based on packet type would go here.  This is highly complex and omitted for brevity.
        }
        Err(e) => println!("Error parsing MQTT packet: {:?}", e),
    }
}

