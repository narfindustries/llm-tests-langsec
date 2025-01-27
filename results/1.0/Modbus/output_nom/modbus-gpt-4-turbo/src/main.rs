use nom::{
    IResult, 
    bytes::complete::{take, tag}, 
    number::complete::{be_u8, be_u16}
};
use std::{env, fs::File, io::{self, Read}};

#[derive(Debug)]
struct ModbusFrame {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    pdu: ProtocolDataUnit,
}

#[derive(Debug)]
enum ProtocolDataUnit {
    ReadCoils {
        function_code: u8,
        starting_address: u16,
        quantity_of_coils: u16,
    },
    ReadDiscreteInputs {
        function_code: u8,
        starting_address: u16,
        quantity_of_inputs: u16,
    },
    ReadHoldingRegisters {
        function_code: u8,
        starting_address: u16,
        quantity_of_registers: u16,
    },
    ReadInputRegisters {
        function_code: u8,
        starting_address: u16,
        quantity_of_registers: u16,
    },
    WriteSingleCoil {
        function_code: u8,
        output_address: u16,
        output_value: u16,
    },
    WriteSingleRegister {
        function_code: u8,
        register_address: u16,
        register_value: u16,
    },
    WriteMultipleCoils {
        function_code: u8,
        starting_address: u16,
        quantity_of_outputs: u16,
        byte_count: u8,
        output_values: Vec<u8>,
    },
    WriteMultipleRegisters {
        function_code: u8,
        starting_address: u16,
        quantity_of_registers: u16,
        byte_count: u8,
        register_values: Vec<u8>,
    },
    // Other function codes can be implemented in similar pattern
}

fn parse_modbus_frame(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, pdu) = parse_pdu(input)?;

    Ok((input, ModbusFrame {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        pdu
    }))
}

fn parse_pdu(input: &[u8]) -> IResult<&[u8], ProtocolDataUnit> {
    let (input, function_code) = be_u8(input)?;

    match function_code {
        0x01 => {
            let (input, starting_address) = be_u16(input)?;
            let (input, quantity_of_coils) = be_u16(input)?;
            Ok((input, ProtocolDataUnit::ReadCoils {
                function_code,
                starting_address,
                quantity_of_coils
            }))
        },
        // Additional parsing branches for each function code
        _ => panic!("Unimplemented function code"),
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("Usage: {} <modbus_data_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;

    match parse_modbus_frame(&contents) {
        Ok((_rest, frame)) => println!("{:?}", frame),
        Err(e) => println!("Failed to parse Modbus frame: {:?}", e)
    }

    Ok(())
}