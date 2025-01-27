use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{length_data, take_while_m_n},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
struct MqttFixedHeader {
    message_type: MqttMessageType,
    dup: bool,
    qos: u8,
    retain: bool,
}

impl MqttFixedHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map(
            be_u8,
            |byte: u8| {
                let message_type = match byte >> 4 {
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
                };
                let dup = (byte & 0x08) != 0;
                let qos = (byte & 0x06) >> 1;
                let retain = (byte & 0x01) != 0;
                MqttFixedHeader {
                    message_type,
                    dup,
                    qos,
                    retain,
                }
            },
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct MqttConnectVariableHeader {
    protocol_name: String,
    protocol_level: u8,
    connect_flags: u8,
    keep_alive: u16,
}

impl MqttConnectVariableHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, _) = tag("MQTT")(input)?;
        let (input, protocol_level) = be_u8(input)?;
        let (input, connect_flags) = be_u8(input)?;
        let (input, keep_alive) = be_u16(input)?;
        let protocol_name = "MQTT".to_string();
        Ok((
            input,
            MqttConnectVariableHeader {
                protocol_name,
                protocol_level,
                connect_flags,
                keep_alive,
            },
        ))
    }
}

#[derive(Debug, PartialEq)]
struct MqttConnectPayload {
    client_id: String,
    will_topic: Option<String>,
    will_message: Option<String>,
    username: Option<String>,
    password: Option<String>,
}

impl MqttConnectPayload {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, client_id_length) = be_u16(input)?;
        let (input, client_id) = take(client_id_length)(input)?;
        let client_id = String::from_utf8_lossy(client_id).into_owned();
        let (input, will_topic) = opt(length_data(be_u16))(input)?;
        let will_topic = will_topic.map(|topic| String::from_utf8_lossy(topic).into_owned());
        let (input, will_message) = opt(length_data(be_u16))(input)?;
        let will_message = will_message.map(|message| String::from_utf8_lossy(message).into_owned());
        let (input, username) = opt(length_data(be_u16))(input)?;
        let username = username.map(|username| String::from_utf8_lossy(username).into_owned());
        let (input, password) = opt(length_data(be_u16))(input)?;
        let password = password.map(|password| String::from_utf8_lossy(password).into_owned());
        Ok((
            input,
            MqttConnectPayload {
                client_id,
                will_topic,
                will_message,
                username,
                password,
            },
        ))
    }
}

#[derive(Debug, PartialEq)]
struct MqttConnect {
    fixed_header: MqttFixedHeader,
    variable_header: MqttConnectVariableHeader,
    payload: MqttConnectPayload,
}

impl MqttConnect {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, fixed_header) = MqttFixedHeader::parse(input)?;
        let (input, variable_header) = MqttConnectVariableHeader::parse(input)?;
        let (input, payload) = MqttConnectPayload::parse(input)?;
        Ok((
            input,
            MqttConnect {
                fixed_header,
                variable_header,
                payload,
            },
        ))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (_input, connect) = MqttConnect::parse(&input).unwrap();
    println!("{:?}", connect);
}