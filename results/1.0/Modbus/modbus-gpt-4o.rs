use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::{be_u16, le_u16, u8},
    sequence::tuple,
    IResult,
};

#[derive(Debug)]
pub struct ModbusTcpHeader {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
}

#[derive(Debug)]
pub struct ModbusFrame {
    header: ModbusTcpHeader,
    function_code: u8,
    data: Vec<u8>,
}

fn parse_modbus_tcp_header(input: &[u8]) -> IResult<&[u8], ModbusTcpHeader> {
    let (input, (transaction_id, protocol_id, length, unit_id)) =
        tuple((be_u16, be_u16, be_u16, u8))(input)?;

    Ok((
        input,
        ModbusTcpHeader {
            transaction_id,
            protocol_id,
            length,
            unit_id,
        },
    ))
}

fn parse_modbus_frame(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    let (input, header) = parse_modbus_tcp_header(input)?;
    let remaining_length = header.length - 2; // subtract size of unit_id and function_code
    let (input, (function_code, data)) = tuple((u8, take(remaining_length)))(input)?;

    Ok((
        input,
        ModbusFrame {
            header,
            function_code,
            data: data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    match parse_modbus_frame(&buffer) {
        Ok((_, frame)) => println!("{:?}", frame),
        Err(e) => eprintln!("Error parsing TCP frame: {:?}", e),
    }
}