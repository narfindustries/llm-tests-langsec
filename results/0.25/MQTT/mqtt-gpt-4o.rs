use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    multi::length_data,
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
enum MqttPacket {
    Connect {
        protocol_name: String,
        protocol_level: u8,
        connect_flags: u8,
        keep_alive: u16,
        client_id: String,
        will_topic: Option<String>,
        will_message: Option<String>,
        username: Option<String>,
        password: Option<String>,
    },
    // Other packet types can be added here
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, packet_type) = be_u8(input)?;
    match packet_type >> 4 {
        1 => parse_connect_packet(input),
        _ => unimplemented!(), // Handle other packet types
    }
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, _) = be_u8(input)?; // Remaining length, not used here
    let (input, protocol_name) = length_data(be_u16)(input)?;
    let (input, protocol_level) = be_u8(input)?;
    let (input, connect_flags) = be_u8(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, client_id) = length_data(be_u16)(input)?;

    let will_flag = connect_flags & 0x04 != 0;
    let username_flag = connect_flags & 0x80 != 0;
    let password_flag = connect_flags & 0x40 != 0;

    let (input, will_topic) = if will_flag {
        map(opt(length_data(be_u16)), |opt| opt.map(|s| String::from_utf8_lossy(s).into_owned()))(input)?
    } else {
        (input, None)
    };

    let (input, will_message) = if will_flag {
        map(opt(length_data(be_u16)), |opt| opt.map(|s| String::from_utf8_lossy(s).into_owned()))(input)?
    } else {
        (input, None)
    };

    let (input, username) = if username_flag {
        map(opt(length_data(be_u16)), |opt| opt.map(|s| String::from_utf8_lossy(s).into_owned()))(input)?
    } else {
        (input, None)
    };

    let (input, password) = if password_flag {
        map(opt(length_data(be_u16)), |opt| opt.map(|s| String::from_utf8_lossy(s).into_owned()))(input)?
    } else {
        (input, None)
    };

    Ok((
        input,
        MqttPacket::Connect {
            protocol_name: String::from_utf8_lossy(protocol_name).into_owned(),
            protocol_level,
            connect_flags,
            keep_alive,
            client_id: String::from_utf8_lossy(client_id).into_owned(),
            will_topic,
            will_message,
            username,
            password,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse MQTT packet: {:?}", e),
    }

    Ok(())
}