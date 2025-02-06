use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{length_data},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, Result},
};

#[derive(Debug, PartialEq)]
enum QoS {
    AtMostOnce,
    AtLeastOnce,
    ExactlyOnce,
}

#[derive(Debug, PartialEq)]
enum Dup {
    NotDuplicate,
    Duplicate,
}

#[derive(Debug, PartialEq)]
enum ConnectFlags {
    CleanStart(bool),
    WillFlag(bool),
    WillQoS(QoS),
    WillRetain(bool),
    PasswordFlag(bool),
    UsernameFlag(bool),
}

#[derive(Debug, PartialEq)]
enum ReasonCode {
    Success,
    NormalDisconnection,
    GoingToSleep,
    AwakenByOtherEvent,
    DisconnectedByServer,
    BadConnection,
    ProtocolError,
    ImplementationSpecificError,
    UnspecifiedError,
    MalformedPacket,
    ProtocolVersionUnsupported,
    InvalidClientIdentifier,
    BadUsernameOrPassword,
    NotAuthorized,
    ServerUnavailable,
    ServerBusy,
    Banned,
    ServerShuttingDown,
    BadAuthenticationMethod,
    KeepAliveTimeout,
    SessionTakenOver,
    TopicFilterInvalid,
    TopicNameInvalid,
    PacketIdentifierInUse,
    PacketTooLarge,
    MessageRateTooHigh,
    QuotaExceeded,
    AdministrativeAction,
    PayloadFormatInvalid,
    RetainNotSupported,
    QoSNotSupported,
    UseAnotherServer,
    ServerMoved,
    SharedSubscriptionNotSupported,
    ConnectionRateExceeded,
    MaximumConnectTime,
    SubscriptionIdentifiersNotSupported,
    WildcardSubscriptionsNotSupported,
}

#[derive(Debug, PartialEq)]
enum Property {
    PayloadFormatIndicator(bool),
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
    RequestProblemInformation(bool),
    WillDelayInterval(u32),
    RequestResponseInformation(bool),
    ResponseInformation(String),
    ServerReference(String),
    ReasonString(String),
    ReceiveMaximum(u16),
    TopicAliasMaximum(u16),
    TopicAlias(u16),
    MaximumQoS(QoS),
    RetainAvailable(bool),
    UserProperty(String, String),
    MaximumPacketSize(u32),
    WildcardSubscriptionAvailable(bool),
    SubscriptionIdentifierAvailable(bool),
    SharedSubscriptionAvailable(bool),
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], (QoS, Dup)> {
    let (input, byte) = take(1u8)(input)?;
    let qos = match byte[0] & 0b0000_1100 {
        0b0000_0000 => QoS::AtMostOnce,
        0b0000_0100 => QoS::AtLeastOnce,
        0b0000_1000 => QoS::ExactlyOnce,
        _ => unreachable!(),
    };
    let dup = match byte[0] & 0b0000_0010 {
        0b0000_0000 => Dup::NotDuplicate,
        0b0000_0010 => Dup::Duplicate,
        _ => unreachable!(),
    };
    Ok((input, (qos, dup)))
}

fn parse_connect_flags(input: &[u8]) -> IResult<&[u8], ConnectFlags> {
    let (input, byte) = take(1u8)(input)?;
    let clean_start = (byte[0] & 0b0000_0001) != 0;
    let will_flag = (byte[0] & 0b0000_0010) != 0;
    let will_qos = match byte[0] & 0b0000_1100 {
        0b0000_0000 => QoS::AtMostOnce,
        0b0000_0100 => QoS::AtLeastOnce,
        0b0000_1000 => QoS::ExactlyOnce,
        _ => unreachable!(),
    };
    let will_retain = (byte[0] & 0b0001_0000) != 0;
    let password_flag = (byte[0] & 0b0010_0000) != 0;
    let username_flag = (byte[0] & 0b0100_0000) != 0;
    Ok((input, ConnectFlags::CleanStart(clean_start)))
}

fn parse_property(input: &[u8]) -> IResult<&[u8], Property> {
    let (input, property_id) = be_u8(input)?;
    let (input, property_value) = match property_id {
        0x01 => {
            let (input, value) = be_u8(input)?;
            (input, Property::PayloadFormatIndicator(value != 0))
        }
        0x02 => {
            let (input, value) = be_u32(input)?;
            (input, Property::MessageExpiryInterval(value))
        }
        0x03 => {
            let (input, value) = take_while_m_n(1, 65535, |c| c != 0)(input)?;
            (input, Property::ContentType(String::from_utf8_lossy(value).into_owned()))
        }
        0x08 => {
            let (input, value) = take_while_m_n(1, 65535, |c| c != 0)(input)?;
            (input, Property::ResponseTopic(String::from_utf8_lossy(value).into_owned()))
        }
        0x09 => {
            let (input, value) = length_data(be_u16)(input)?;
            (input, Property::CorrelationData(value.to_vec()))
        }
        0x0b => {
            let (input, value) = be_u32(input)?;
            (input, Property::SubscriptionIdentifier(value))
        }
        0x11 => {
            let (input, value) = be_u32(input)?;
            (input, Property::SessionExpiryInterval(value))
        }
        0x12 => {
            let (input, value) = take_while_m_n(1, 65535, |c| c != 0)(input)?;
            (input, Property::AssignedClientIdentifier(String::from_utf8_lossy(value).into_owned()))
        }
        0x13 => {
            let (input, value) = be_u16(input)?;
            (input, Property::ServerKeepAlive(value))
        }
        0x15 => {
            let (input, value) = take_while_m_n(1, 65535, |c| c != 0)(input)?;
            (input, Property::AuthenticationMethod(String::from_utf8_lossy(value).into_owned()))
        }
        0x16 => {
            let (input, value) = length_data(be_u16)(input)?;
            (input, Property::AuthenticationData(value.to_vec()))
        }
        0x17 => {
            let (input, value) = be_u8(input)?;
            (input, Property::RequestProblemInformation(value != 0))
        }
        0x18 => {
            let (input, value) = be_u32(input)?;
            (input, Property::WillDelayInterval(value))
        }
        0x19 => {
            let (input, value) = be_u8(input)?;
            (input, Property::RequestResponseInformation(value != 0))
        }
        0x1a => {
            let (input, value) = take_while_m_n(1, 65535, |c| c != 0)(input)?;
            (input, Property::ResponseInformation(String::from_utf8_lossy(value).into_owned()))
        }
        0x1c => {
            let (input, value) = take_while_m_n(1, 65535, |c| c != 0)(input)?;
            (input, Property::ServerReference(String::from_utf8_lossy(value).into_owned()))
        }
        0x1f => {
            let (input, value) = take_while_m_n(1, 65535, |c| c != 0)(input)?;
            (input, Property::ReasonString(String::from_utf8_lossy(value).into_owned()))
        }
        0x21 => {
            let (input, value) = be_u16(input)?;
            (input, Property::ReceiveMaximum(value))
        }
        0x22 => {
            let (input, value) = be_u16(input)?;
            (input, Property::TopicAliasMaximum(value))
        }
        0x23 => {
            let (input, value) = be_u16(input)?;
            (input, Property::TopicAlias(value))
        }
        0x24 => {
            let (input, value) = be_u8(input)?;
            (input, Property::MaximumQoS(match value {
                0 => QoS::AtMostOnce,
                1 => QoS::AtLeastOnce,
                2 => QoS::ExactlyOnce,
                _ => unreachable!(),
            }))
        }
        0x25 => {
            let (input, value) = be_u8(input)?;
            (input, Property::RetainAvailable(value != 0))
        }
        0x26 => {
            let (input, key) = take_while_m_n(1, 65535, |c| c != 0)(input)?;
            let (input, value) = take_while_m_n(1, 65535, |c| c != 0)(input)?;
            (
                input,
                Property::UserProperty(
                    String::from_utf8_lossy(key).into_owned(),
                    String::from_utf8_lossy(value).into_owned(),
                ),
            )
        }
        0x27 => {
            let (input, value) = be_u32(input)?;
            (input, Property::MaximumPacketSize(value))
        }
        0x28 => {
            let (input, value) = be_u8(input)?;
            (input, Property::WildcardSubscriptionAvailable(value != 0))
        }
        0x29 => {
            let (input, value) = be_u8(input)?;
            (input, Property::SubscriptionIdentifierAvailable(value != 0))
        }
        0x2a => {
            let (input, value) = be_u8(input)?;
            (input, Property::SharedSubscriptionAvailable(value != 0))
        }
        _ => unreachable!(),
    };
    Ok((input, property_value))
}

fn parse_connect(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0x10])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    let (input, _) = take(4u8)(input)?; // protocol name
    let (input, _) = be_u8(input)?; // protocol version
    let (input, _) = parse_connect_flags(input)?;
    let (input, _) = be_u16(input)?; // keep alive
    let (input, _) = opt(length_data(be_u16))(input)?;
    let (input, _) = take_while_m_n(1, 65535, |c| c != 0)(input)?; // client identifier
    Ok((input, ()))
}

fn parse_connack(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0x20])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    let (input, _) = be_u8(input)?; // session present
    let (input, _) = be_u8(input)?; // return code
    let (input, _) = opt(length_data(be_u16))(input)?;
    Ok((input, ()))
}

fn parse_publish(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0x30])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    let (input, _) = take_while_m_n(1, 65535, |c| c != 0)(input)?; // topic name
    let (input, _) = opt(be_u16)(input)?; // packet identifier
    let (input, _) = opt(length_data(be_u16))(input)?;
    let (input, _) = length_data(be_u16)(input)?; // payload
    Ok((input, ()))
}

fn parse_puback(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0x40])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    let (input, _) = be_u16(input)?; // packet identifier
    let (input, _) = be_u8(input)?; // reason code
    let (input, _) = opt(length_data(be_u16))(input)?;
    Ok((input, ()))
}

fn parse_pubrec(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0x50])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    let (input, _) = be_u16(input)?; // packet identifier
    let (input, _) = be_u8(input)?; // reason code
    let (input, _) = opt(length_data(be_u16))(input)?;
    Ok((input, ()))
}

fn parse_pubrel(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0x60])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    let (input, _) = be_u16(input)?; // packet identifier
    let (input, _) = be_u8(input)?; // reason code
    let (input, _) = opt(length_data(be_u16))(input)?;
    Ok((input, ()))
}

fn parse_pubcomp(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0x70])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    let (input, _) = be_u16(input)?; // packet identifier
    let (input, _) = be_u8(input)?; // reason code
    let (input, _) = opt(length_data(be_u16))(input)?;
    Ok((input, ()))
}

fn parse_subscribe(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0x80])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    let (input, _) = be_u16(input)?; // packet identifier
    let (input, _) = opt(length_data(be_u16))(input)?;
    let (input, _) = take_while_m_n(1, 65535, |c| c != 0)(input)?; // topic filter
    Ok((input, ()))
}

fn parse_suback(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0x90])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    let (input, _) = be_u16(input)?; // packet identifier
    let (input, _) = opt(length_data(be_u16))(input)?;
    let (input, _) = be_u8(input)?; // return code
    Ok((input, ()))
}

fn parse_unsubscribe(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0xa0])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    let (input, _) = be_u16(input)?; // packet identifier
    let (input, _) = opt(length_data(be_u16))(input)?;
    let (input, _) = take_while_m_n(1, 65535, |c| c != 0)(input)?; // topic filter
    Ok((input, ()))
}

fn parse_unsuback(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0xb0])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    let (input, _) = be_u16(input)?; // packet identifier
    let (input, _) = opt(length_data(be_u16))(input)?;
    let (input, _) = be_u8(input)?; // return code
    Ok((input, ()))
}

fn parse_pingreq(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0xc0])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    Ok((input, ()))
}

fn parse_pingresp(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0xd0])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    Ok((input, ()))
}

fn parse_disconnect(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0xe0])(input)?;
    let (input, _) = parse_fixed_header(input)?;
    let (input, _) = be_u8(input)?; // reason code
    let (input, _) = opt(length_data(be_u16))(input)?;
    Ok((input, ()))
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }
    let mut file = File::open(&args[1])?;
    let mut input = Vec::new();
    file.read_to_end(&mut input)?;
    let Ok((input, _)) = parse_connect(input.as_slice()) else { 
        return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Failed to parse connect packet")); 
    };
    let Ok((input, _)) = parse_connack(input) else { 
        return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Failed to parse connack packet")); 
    };
    let Ok((input, _)) = parse_publish(input) else { 
        return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Failed to parse publish packet")); 
    };
    let Ok((input, _)) = parse_puback(input) else { 
        return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Failed to parse puback packet")); 
    };
    let Ok((input, _)) = parse_pubrec(input) else { 
        return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Failed to parse pubrec packet")); 
    };
   