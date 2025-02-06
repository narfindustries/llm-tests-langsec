use nom::{
    bytes::complete::{take, take_until},
    combinator::{map, map_res, opt},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ModbusFrame {
    transaction_identifier: u16,
    protocol_identifier: u16,
    length: u16,
    unit_identifier: u8,
    function_code: u8,
    data: Vec<u8>,
}

fn parse_modbus_frame(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    let (input, (transaction_identifier, protocol_identifier, length, unit_identifier, function_code)) =
        tuple((be_u16, be_u16, be_u16, be_u8, be_u8))(input)?;

    let data_len = length as usize;
    let (input, data) = take(data_len)(input)?;

    Ok((
        input,
        ModbusFrame {
            transaction_identifier,
            protocol_identifier,
            length,
            unit_identifier,
            function_code,
            data: data.to_vec(),
        },
    ))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file {}: {}", filename, err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    };

    match parse_modbus_frame(&buffer) {
        Ok((remaining, frame)) => {
            println!("Parsed Modbus Frame:\n{:?}\nRemaining bytes: {:?}", frame, remaining);
        }
        Err(err) => {
            eprintln!("Error parsing Modbus frame: {:?}", err);
        }
    }
}

