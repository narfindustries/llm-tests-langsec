// Import necessary crates
use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

// Define a data structure for the Modbus ADU
#[derive(Debug)]
struct ModbusAdu {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: u8,
    data: Vec<u8>,
}

// Function to parse Modbus ADU
fn parse_modbus_adu(input: &[u8]) -> IResult<&[u8], ModbusAdu> {
    let (input, (transaction_id, protocol_id, length, unit_id, function_code)) =
        tuple((be_u16, be_u16, be_u16, be_u8, be_u8))(input)?;
    let (input, data) = take(length - 2u16)(input)?; // length includes unit_id and function_code
    Ok((
        input,
        ModbusAdu {
            transaction_id,
            protocol_id,
            length,
            unit_id,
            function_code,
            data: data.to_vec(),
        },
    ))
}

fn main() {
    // Collect command line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    // Open and read the binary file
    let input_path = Path::new(&args[1]);
    let mut file = File::open(&input_path).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    // Parse the Modbus ADU
    match parse_modbus_adu(&buffer) {
        Ok((_, adu)) => println!("{:?}", adu),
        Err(err) => eprintln!("Failed to parse Modbus ADU: {:?}", err),
    }
}