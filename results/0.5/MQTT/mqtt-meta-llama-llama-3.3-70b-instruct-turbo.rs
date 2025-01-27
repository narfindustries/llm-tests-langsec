use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::take_while_m_n,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    env, fs,
    io::{self, Read},
    str,
};

#[derive(Debug, PartialEq)]
enum MqttMessageType {
    Connect = 1,
    Connack = 2,
    Publish = 3,
    Puback = 4,
    Pubrec = 5,
    Pubrel = 6,
    Pubcomp = 7,
    Subscribe = 8,
    Suback = 9,
    Unsubscribe = 10,
    Unsuback = 11,
    Pingreq = 12,
    Pingresp = 13,
    Disconnect = 14,
}

#[derive(Debug, PartialEq)]
struct MqttFixedHeader {
    msg_type: MqttMessageType,
    dup: bool,
    qos: u8,
    retain: bool,
}

impl MqttFixedHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map(
            verify(
                be_u8,
                |value| value >= 0x10 && value <= 0x0F || value == 0x0D || value == 0x0E,
            ),
            |value| {
                let msg_type = match value & 0xF0 {
                    0x10 => MqttMessageType::Connect,
                    0x20 => MqttMessageType::Connack,
                    0x30 => MqttMessageType::Publish,
                    0x40 => MqttMessageType::Puback,
                    0x50 => MqttMessageType::Pubrec,
                    0x60 => MqttMessageType::Pubrel,
                    0x70 => MqttMessageType::Pubcomp,
                    0x80 => MqttMessageType::Subscribe,
                    0x90 => MqttMessageType::Suback,
                    0xA0 => MqttMessageType::Unsubscribe,
                    0xB0 => MqttMessageType::Unsuback,
                    0xC0 => MqttMessageType::Pingreq,
                    0xD0 => MqttMessageType::Pingresp,
                    0xE0 => MqttMessageType::Disconnect,
                    _ => unreachable!(),
                };
                let dup = (value & 0x08) != 0;
                let qos = (value & 0x06) >> 1;
                let retain = (value & 0x01) != 0;
                MqttFixedHeader {
                    msg_type,
                    dup,
                    qos,
                    retain,
                }
            },
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct MqttConnackFlags {
    session_present: bool,
}

impl MqttConnackFlags {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map(be_u8, |value| MqttConnackFlags {
            session_present: (value & 0x01) != 0,
        })(input)
    }
}

#[derive(Debug, PartialEq)]
struct MqttConnack {
    sp: bool,
    rc: u8,
}

impl MqttConnack {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, _) = tag(&[0x02])(input)?;
        let (input, sp) = map(be_u8, |value| (value & 0x01) != 0)(input);
        let (input, rc) = be_u8(input);
        Ok((input, MqttConnack { sp, rc }))
    }
}

#[derive(Debug, PartialEq)]
struct MqttPublish {
    topic_name: String,
    packet_id: Option<u16>,
}

impl MqttPublish {
    fn parse(input: &[u8], qos: u8) -> IResult<&[u8], Self> {
        let (input, topic_name_length) = be_u16(input);
        let (input, topic_name) = take(topic_name_length)(input);
        let topic_name = str::from_utf8(topic_name)
            .unwrap()
            .to_string();
        let packet_id = if qos > 0 {
            let (input, packet_id) = be_u16(input);
            Some(packet_id)
        } else {
            (input, None)
        };
        Ok((input, MqttPublish {
            topic_name,
            packet_id,
        }))
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Usage: mqtt_parser <input_file>",
        ));
    }
    let mut file = fs::File::open(&args[1])?;
    let mut input = Vec::new();
    file.read_to_end(&mut input)?;
    let (input, fixed_header) = MqttFixedHeader::parse(&input).unwrap();
    match fixed_header.msg_type {
        MqttMessageType::Connack => {
            let (input, connack) = MqttConnack::parse(input).unwrap();
            println!("{:?}", connack);
        }
        MqttMessageType::Publish => {
            let publish = MqttPublish::parse(input, fixed_header.qos).unwrap();
            println!("{:?}", publish);
        }
        _ => unreachable!(),
    }
    Ok(())
}