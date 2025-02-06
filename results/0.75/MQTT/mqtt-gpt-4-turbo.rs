use nom::{
    IResult, bytes::complete::{take}, multi::many0, number::complete::{be_u8, be_u16, be_u32},
    error::ParseError,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug, PartialEq)]
enum QoS {
    AtMostOnce,
    AtLeastOnce,
    ExactlyOnce,
}

#[derive(Debug, PartialEq)]
enum ControlPacket {
    Connect(ConnectPacket),
    Connack,
    Publish(PublishPacket),
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

#[derive(Debug, PartialEq)]
struct ConnectPacket {
    protocol_name: String,
    protocol_level: u8,
    connect_flags: u8,
    keep_alive: u16,
    properties: Vec<Property>,
    client_id: String,
    will_properties: Option<Vec<Property>>,
    will_topic: Option<String>,
    will_payload: Option<Vec<u8>>,
    username: Option<String>,
    password: Option<String>,
}

#[derive(Debug, PartialEq)]
struct PublishPacket {
    dup_flag: bool,
    qos_level: QoS,
    retain: bool,
    topic_name: String,
    packet_identifier: Option<u16>,
    properties: Vec<Property>,
    payload: Vec<u8>,
}

#[derive(Debug, PartialEq)]
enum Property {
    PayloadFormatIndicator(u8),
    MessageExpiryInterval(u32),
    ContentType(String),
    ResponseTopic(String),
    CorrelationData(Vec<u8>),
    SubscriptionIdentifier(u32),
    SessionExpiryInterval(u32),
    AssignedClientIdentifier(String),
    ServerKeepAlive(u16),
    AuthenticationMethod(String),
    AuthenticationData(Vec<u8>),
    RequestProblemInformation(u8),
    WillDelayInterval(u32),
    RequestResponseInformation(u8),
    ResponseInformation(String),
    ServerReference(String),
    ReasonString(String),
    ReceiveMaximum(u16),
    TopicAliasMaximum(u16),
    TopicAlias(u16),
    MaximumQoS(u8),
    RetainAvailable(u8),
    UserProperty(String, String),
    MaximumPacketSize(u32),
    WildcardSubscriptionAvailable(u8),
    SubscriptionIdentifierAvailable(u8),
    SharedSubscriptionAvailable(u8),
}

fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, length) = be_u16(input)?;
    let (input, raw_string) = take(length)(input)?;
    match std::str::from_utf8(raw_string) {
        Ok(s) => Ok((input, s.to_string())),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Fail))),
    }
}

fn parse_property(input: &[u8]) -> IResult<&[u8], Property> {
    let (input, identifier) = be_u8(input)?;
    match identifier {
        0x01 => {
            let (input, value) = be_u8(input)?;
            Ok((input, Property::PayloadFormatIndicator(value)))
        },
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Switch))),
    }
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, protocol_name) = parse_string(input)?;
    let (input, protocol_level) = be_u8(input)?;
    let (input, connect_flags) = be_u8(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, properties) = many0(parse_property)(input)?;
    let (input, client_id) = parse_string(input)?;
    Ok((input, ConnectPacket {
        protocol_name,
        protocol_level,
        connect_flags,
        keep_alive,
        properties,
        client_id,
        will_properties: None,
        will_topic: None,
        will_payload: None,
        username: None,
        password: None,
    }))
}

fn parse_mqtt(input: &[u8]) -> IResult<&[u8], ControlPacket> {
    let (input, packet_type) = be_u8(input)?;
    match packet_type {
        0x10 => {
            let (input, packet) = parse_connect_packet(input)?;
            Ok((input, ControlPacket::Connect(packet)))
        },
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Switch))),
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "Please provide exactly one argument"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => println!("Failed to parse MQTT data: {:?}", e),
    }

    Ok(())
}