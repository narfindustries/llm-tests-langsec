use nom::{
    bytes::complete::{take},
    combinator::{opt},
    multi::length_data,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{env, fs};

#[derive(Debug)]
enum MessageType {
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

impl MessageType {
    fn from_u8(n: u8) -> Option<Self> {
        match n {
            1 => Some(Self::Connect),
            2 => Some(Self::Connack),
            3 => Some(Self::Publish),
            4 => Some(Self::Puback),
            5 => Some(Self::Pubrec),
            6 => Some(Self::Pubrel),
            7 => Some(Self::Pubcomp),
            8 => Some(Self::Subscribe),
            9 => Some(Self::Suback),
            10 => Some(Self::Unsubscribe),
            11 => Some(Self::Unsuback),
            12 => Some(Self::Pingreq),
            13 => Some(Self::Pingresp),
            14 => Some(Self::Disconnect),
            15 => Some(Self::Auth),
            _ => None,
        }
    }
}

#[derive(Debug)]
enum PropertyIdentifier {
    PayloadFormatIndicator,
    MessageExpiryInterval,
    ContentType,
    ResponseTopic,
    CorrelationData,
    SubscriptionIdentifier,
    SessionExpiryInterval,
    AssignedClientIdentifier,
    ServerKeepAlive,
    AuthenticationMethod,
    AuthenticationData,
    RequestProblemInformation,
    WillDelayInterval,
    RequestResponseInformation,
    ResponseInformation,
    ServerReference,
    ReasonString,
    ReceiveMaximum,
    TopicAliasMaximum,
    TopicloAlias,
    MaximumQoS,
    RetainAvailable,
    UserProperty,
    MaximumPacketSize,
    WildcardSubscriptionAvailable,
    SubscriptionIdentifierAvailable,
    SharedSubscriptionAvailable,
}

impl PropertyIdentifier {
    fn from_u8(n: u8) -> Option<Self> {
        match n {
            0x01 => Some(Self::PayloadFormatIndicator),
            0x02 => Some(Self::MessageExpiryInterval),
            0x03 => Some(Self::ContentType),
            0x08 => Some(Self::ResponseTopic),
            0x09 => Some(Self::CorrelationData),
            0x0b => Some(Self::SubscriptionIdentifier),
            0x11 => Some(Self::SessionExpiryInterval),
            0x12 => Some(Self::AssignedClientIdentifier),
            0x13 => Some(Self::ServerKeepAlive),
            0x15 => Some(Self::AuthenticationMethod),
            0x16 => Some(Self::AuthenticationData),
            0x17 => Some(Self::RequestProblemInformation),
            0x18 => Some(Self::WillDelayInterval),
            0x19 => Some(Self::RequestResponseInformation),
            0x1a => Some(Self::ResponseInformation),
            0x1c => Some(Self::ServerReference),
            0x1f => Some(Self::ReasonString),
            0x21 => Some(Self::ReceiveMaximum),
            0x22 => Some(Self::TopicAliasMaximum),
            0x23 => Some(Self::TopicloAlias),
            0x24 => Some(Self::MaximumQoS),
            0x25 => Some(Self::RetainAvailable),
            0x26 => Some(Self::UserProperty),
            0x27 => Some(Self::MaximumPacketSize),
            0x28 => Some(Self::WildcardSubscriptionAvailable),
            0x29 => Some(Self::SubscriptionIdentifierAvailable),
            0x2a => Some(Self::SharedSubscriptionAvailable),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct MqttFixedHeader {
    message_type: MessageType,
    dup: bool,
    qos: u8,
    retain: bool,
}

impl MqttFixedHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, first_byte) = take(1u8)(input)?;
        let message_type = MessageType::from_u8((first_byte[0] & 0xf0) >> 4).unwrap();
        let dup = (first_byte[0] & 0x08) != 0;
        let qos = (first_byte[0] & 0x06) >> 1;
        let retain = (first_byte[0] & 0x01) != 0;
        Ok((input, MqttFixedHeader { message_type, dup, qos, retain }))
    }
}

#[derive(Debug)]
struct MqttVariableHeader {
    packet_identifier: Option<u16>,
    properties: Option<Vec<u8>>,
}

impl MqttVariableHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, packet_identifier) = opt(be_u16)(input)?;
        let (input, properties) = opt(length_data(be_u16).map(|x| x.to_vec()))(input)?;
        Ok((input, MqttVariableHeader { packet_identifier, properties }))
    }
}

#[derive(Debug)]
struct MqttConnect {
    protocol_name: String,
    protocol_version: u8,
    connect_flags: u8,
    keep_alive: u16,
}

impl MqttConnect {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, protocol_name_length) = be_u16(input)?;
        let (input, protocol_name) = take(protocol_name_length)(input)?;
        let (input, protocol_version) = be_u8(input)?;
        let (input, connect_flags) = be_u8(input)?;
        let (input, keep_alive) = be_u16(input)?;
        let protocol_name = String::from_utf8_lossy(protocol_name).into_owned();
        Ok((input, MqttConnect { protocol_name, protocol_version, connect_flags, keep_alive }))
    }
}

#[derive(Debug)]
struct MqttPacket {
    fixed_header: MqttFixedHeader,
    variable_header: MqttVariableHeader,
    payload: Vec<u8>,
}

impl MqttPacket {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, fixed_header) = MqttFixedHeader::parse(input)?;
        let (input, variable_header) = MqttVariableHeader::parse(input)?;
        let (input, payload_length) = be_u16(input)?;
        let (input, payload) = take(payload_length)(input)?;
        let payload = payload.to_vec();
        Ok((input, MqttPacket { fixed_header, variable_header, payload }))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let input = fs::read(args[1].clone()).expect("Failed to read file");
    let result = MqttPacket::parse(&input);
    match result {
        Ok((_remaining, packet)) => {
            println!("{:?}", packet);
        }
        Err(err) => {
            eprintln!("Error parsing packet: {:?}", err);
        }
    }
}