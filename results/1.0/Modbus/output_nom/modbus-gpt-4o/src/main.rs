// Ensure to add dependencies in your Cargo.toml
// nom = "6.1"
// nom-derive = "3.1.0"

use nom::{
    IResult,
    bytes::complete::{take, take_while},
    number::complete::be_u16,
    combinator::map_res,
    sequence::tuple,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug, PartialEq)]
pub struct ModbusFrame {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: u8,
    data: Vec<u8>,
}

fn parse_modbus_frame(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    let (input, (transaction_id, protocol_id, length, unit_id, function_code))
        = tuple((be_u16, be_u16, be_u16, take(1u8), take(1u8)))(input)?;

    // length is from unit_id onwards
    // include unit_id(1 byte) + function_code(1 byte)
    let data_length = length - 2;

    let (input, data) = take(data_length)(input)?;

    let frame = ModbusFrame {
        transaction_id,
        protocol_id,
        length,
        unit_id: unit_id[0],
        function_code: function_code[0],
        data: data.to_vec(),
    };

    Ok((input, frame))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open the file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read the file");

    match parse_modbus_frame(&buffer) {
        Ok((_, frame)) => {
            println!("Parsed Modbus Frame: {:#?}", frame);
        }
        Err(e) => {
            eprintln!("Failed to parse Modbus frame: {:?}", e);
        }
    }
}