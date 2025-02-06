use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct ModbusPdu {
    function_code: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
struct ModbusRtu {
    slave_address: u8,
    pdu: ModbusPdu,
    crc: u16,
}

#[derive(Debug)]
struct ModbusTcp {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    pdu: ModbusPdu,
}

#[derive(Debug)]
enum ModbusFrame {
    Rtu(ModbusRtu),
    Tcp(ModbusTcp),
}

fn parse_modbus_pdu(input: &[u8]) -> IResult<&[u8], ModbusPdu> {
    let (input, function_code) = be_u8(input)?;
    let (input, data) = take(input.len())(input)?;
    Ok((
        input,
        ModbusPdu {
            function_code,
            data: data.to_vec(),
        },
    ))
}

fn parse_modbus_rtu(input: &[u8]) -> IResult<&[u8], ModbusRtu> {
    let (input, slave_address) = be_u8(input)?;
    let (input, pdu) = parse_modbus_pdu(input)?;
    let (input, crc) = be_u16(input)?;
    Ok((
        input,
        ModbusRtu {
            slave_address,
            pdu,
            crc,
        },
    ))
}

fn parse_modbus_tcp(input: &[u8]) -> IResult<&[u8], ModbusTcp> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, pdu) = parse_modbus_pdu(input)?;
    Ok((
        input,
        ModbusTcp {
            transaction_id,
            protocol_id,
            length,
            unit_id,
            pdu,
        },
    ))
}

fn parse_modbus_frame(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    if input.len() >= 7 {
        let (input, rtu_frame) = parse_modbus_rtu(input)?;
        Ok((input, ModbusFrame::Rtu(rtu_frame)))
    } else {
        let (input, tcp_frame) = parse_modbus_tcp(input)?;
        Ok((input, ModbusFrame::Tcp(tcp_frame)))
    }
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

    match parse_modbus_frame(&buffer) {
        Ok((_, frame)) => println!("{:?}", frame),
        Err(e) => eprintln!("Failed to parse Modbus frame: {:?}", e),
    }

    Ok(())
}