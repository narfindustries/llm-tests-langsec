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
use std::io::Read;
use std::path::Path;

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
    let (input, (_, protocol_name, protocol_level, connect_flags, keep_alive, client_id)) =
        tuple((
            tag(&[0x00, 0x04]), // Length of "MQTT"
            map_res(length_data(be_u16), |s: &[u8]| {
                std::str::from_utf8(s).map(String::from)
            }),
            be_u8,
            be_u8,
            be_u16,
            map_res(length_data(be_u16), |s: &[u8]| {
                std::str::from_utf8(s).map(String::from)
            }),
        ))(input)?;

    let will_flag = connect_flags & 0x04 != 0;
    let username_flag = connect_flags & 0x80 != 0;
    let password_flag = connect_flags & 0x40 != 0;

    let (input, will_topic) = if will_flag {
        map_res(length_data(be_u16), |s: &[u8]| {
            std::str::from_utf8(s).map(String::from)
        })(input)?
    } else {
        (input, None)
    };

    let (input, will_message) = if will_flag {
        map_res(length_data(be_u16), |s: &[u8]| {
            std::str::from_utf8(s).map(String::from)
        })(input)?
    } else {
        (input, None)
    };

    let (input, username) = if username_flag {
        map_res(length_data(be_u16), |s: &[u8]| {
            std::str::from_utf8(s).map(String::from)
        })(input)?
    } else {
        (input, None)
    };

    let (input, password) = if password_flag {
        map_res(length_data(be_u16), |s: &[u8]| {
            std::str::from_utf8(s).map(String::from)
        })(input)?
    } else {
        (input, None)
    };

    Ok((
        input,
        MqttPacket::Connect {
            protocol_name,
            protocol_level,
            connect_flags,
            keep_alive,
            client_id,
            will_topic,
            will_message,
            username,
            password,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    if let Err(err) = file.read_to_end(&mut buffer) {
        eprintln!("Error reading file: {}", err);
        return;
    }

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(err) => eprintln!("Error parsing MQTT packet: {:?}", err),
    }
}