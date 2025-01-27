use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{length_data, take_till},
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::{
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

#[derive(Debug)]
enum MqttMessageType {
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

#[derive(Debug)]
struct MqttMessage {
    message_type: MqttMessageType,
    dup: bool,
    qos: u8,
    retain: bool,
    remaining_length: u32,
    payload: Vec<u8>,
}

fn mqtt_message_type(input: &[u8]) -> IResult<&[u8], MqttMessageType> {
    map(
        be_u8,
        |value: u8| match value {
            1 => MqttMessageType::Connect,
            2 => MqttMessageType::ConnAck,
            3 => MqttMessageType::Publish,
            4 => MqttMessageType::PubAck,
            5 => MqttMessageType::PubRec,
            6 => MqttMessageType::PubRel,
            7 => MqttMessageType::PubComp,
            8 => MqttMessageType::Subscribe,
            9 => MqttMessageType::SubAck,
            10 => MqttMessageType::Unsubscribe,
            11 => MqttMessageType::UnsubAck,
            12 => MqttMessageType::PingReq,
            13 => MqttMessageType::PingResp,
            14 => MqttMessageType::Disconnect,
            _ => panic!("Invalid message type"),
        },
    )(input)
}

fn mqtt_connect(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8], u16, &[u8])> {
    tuple((
        tag(b"MQTT"),
        be_u8,
        be_u16,
        take_till(|c| c == 0),
    ))(input)
}

fn mqtt_publish(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8], &[u8])> {
    tuple((take_till(|c| c == 0), take_till(|c| c == 0), take(0)))(input)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let input_file = &args[1];
    let path = Path::new(input_file);
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).unwrap();
    let (input, _) = length_data(be_u32)(buffer.as_slice()).unwrap();
    let (input, message_type) = mqtt_message_type(input).unwrap();
    match message_type {
        MqttMessageType::Connect => {
            let (input, (magic_string, protocol_level, connect_flags, client_id)) =
                mqtt_connect(input).unwrap();
            println!(
                "Connect: magic_string={:?}, protocol_level={}, connect_flags={}, client_id={:?}",
                magic_string, protocol_level, connect_flags, client_id
            );
        }
        MqttMessageType::Publish => {
            let (input, (topic_name, packet_id, payload)) = mqtt_publish(input).unwrap();
            println!(
                "Publish: topic_name={:?}, packet_id={}, payload={:?}",
                topic_name, packet_id, payload
            );
        }
        _ => println!("Message type: {:?}", message_type),
    }
}