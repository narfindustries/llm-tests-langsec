use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::tuple;
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

// Modbus function codes
const READ_COILS: u8 = 0x01;
const READ_DISCRETE_INPUTS: u8 = 0x02;
const READ_HOLDING_REGISTERS: u8 = 0x03;
const READ_INPUT_REGISTERS: u8 = 0x04;
const WRITE_SINGLE_COIL: u8 = 0x05;
const WRITE_SINGLE_REGISTER: u8 = 0x06;
const READ_EXCEPTION_STATUS: u8 = 0x07;
const DIAGNOSTICS: u8 = 0x08;
const GET_COMM_EVENT_COUNTER: u8 = 0x0B;
const GET_COMM_EVENT_LOG: u8 = 0x0C;
const WRITE_MULTIPLE_COILS: u8 = 0x0F;
const WRITE_MULTIPLE_REGISTERS: u8 = 0x10;
const REPORT_SLAVE_ID: u8 = 0x11;
const READ_FILE_RECORD: u8 = 0x14;
const WRITE_FILE_RECORD: u8 = 0x15;
const MASK_WRITE_REGISTER: u8 = 0x16;
const READ_FIFO_QUEUE: u8 = 0x18;
const ENC_INITIALIZED_WRITE: u8 = 0x2B;

// Modbus PDU types
enum Pdu {
    ReadCoils(ReadCoilsPdu),
    ReadDiscreteInputs(ReadDiscreteInputsPdu),
    ReadHoldingRegisters(ReadHoldingRegistersPdu),
    ReadInputRegisters(ReadInputRegistersPdu),
    WriteSingleCoil(WriteSingleCoilPdu),
    WriteSingleRegister(WriteSingleRegisterPdu),
    ReadExceptionStatus(ReadExceptionStatusPdu),
    Diagnostics(DiagnosticsPdu),
    GetCommEventCounter(GetCommEventCounterPdu),
    GetCommEventLog(GetCommEventLogPdu),
    WriteMultipleCoils(WriteMultipleCoilsPdu),
    WriteMultipleRegisters(WriteMultipleRegistersPdu),
    ReportSlaveId(ReportSlaveIdPdu),
    ReadFileRecord(ReadFileRecordPdu),
    WriteFileRecord(WriteFileRecordPdu),
    MaskWriteRegister(MaskWriteRegisterPdu),
    ReadFifoQueue(ReadFifoQueuePdu),
    EncInitializedWrite(EncInitializedWritePdu),
}

// Modbus PDU structures
#[derive(Debug)]
struct ReadCoilsPdu {
    starting_address: u16,
    quantity: u16,
}

#[derive(Debug)]
struct ReadDiscreteInputsPdu {
    starting_address: u16,
    quantity: u16,
}

#[derive(Debug)]
struct ReadHoldingRegistersPdu {
    starting_address: u16,
    quantity: u16,
}

#[derive(Debug)]
struct ReadInputRegistersPdu {
    starting_address: u16,
    quantity: u16,
}

#[derive(Debug)]
struct WriteSingleCoilPdu {
    output_address: u16,
    output_value: bool,
}

#[derive(Debug)]
struct WriteSingleRegisterPdu {
    register_address: u16,
    register_value: u16,
}

#[derive(Debug)]
struct ReadExceptionStatusPdu {
}

#[derive(Debug)]
struct DiagnosticsPdu {
    sub_function: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
struct GetCommEventCounterPdu {
}

#[derive(Debug)]
struct GetCommEventLogPdu {
}

#[derive(Debug)]
struct WriteMultipleCoilsPdu {
    starting_address: u16,
    quantity: u16,
    coil_data: Vec<bool>,
}

#[derive(Debug)]
struct WriteMultipleRegistersPdu {
    starting_address: u16,
    quantity: u16,
    register_data: Vec<u16>,
}

#[derive(Debug)]
struct ReportSlaveIdPdu {
    slave_id: u8,
    run_indicator_status: bool,
}

#[derive(Debug)]
struct ReadFileRecordPdu {
    file_number: u16,
    record_number: u16,
    record_length: u16,
}

#[derive(Debug)]
struct WriteFileRecordPdu {
    file_number: u16,
    record_number: u16,
    record_data: Vec<u8>,
}

#[derive(Debug)]
struct MaskWriteRegisterPdu {
    register_address: u16,
    and_mask: u16,
    or_mask: u16,
}

#[derive(Debug)]
struct ReadFifoQueuePdu {
    fifo_pointer_address: u16,
}

#[derive(Debug)]
struct EncInitializedWritePdu {
    sub_function: u8,
    data: Vec<u8>,
}

// Modbus ADU parser
fn adu_parser(input: &[u8]) -> IResult<&[u8], Pdu> {
    let (input, transaction_id) = take(2u8)(input)?;
    let (input, protocol_id) = take(2u8)(input)?;
    let (input, length) = take(2u8)(input)?;
    let (input, unit_id) = take(1u8)(input)?;
    let (input, function_code) = take(1u8)(input)?;

    match function_code[0] {
        READ_COILS => {
            let (input, pdu) = read_coils_pdu_parser(input)?;
            Ok((input, Pdu::ReadCoils(pdu)))
        }
        READ_DISCRETE_INPUTS => {
            let (input, pdu) = read_discrete_inputs_pdu_parser(input)?;
            Ok((input, Pdu::ReadDiscreteInputs(pdu)))
        }
        READ_HOLDING_REGISTERS => {
            let (input, pdu) = read_holding_registers_pdu_parser(input)?;
            Ok((input, Pdu::ReadHoldingRegisters(pdu)))
        }
        READ_INPUT_REGISTERS => {
            let (input, pdu) = read_input_registers_pdu_parser(input)?;
            Ok((input, Pdu::ReadInputRegisters(pdu)))
        }
        WRITE_SINGLE_COIL => {
            let (input, pdu) = write_single_coil_pdu_parser(input)?;
            Ok((input, Pdu::WriteSingleCoil(pdu)))
        }
        WRITE_SINGLE_REGISTER => {
            let (input, pdu) = write_single_register_pdu_parser(input)?;
            Ok((input, Pdu::WriteSingleRegister(pdu)))
        }
        READ_EXCEPTION_STATUS => {
            let (input, pdu) = read_exception_status_pdu_parser(input)?;
            Ok((input, Pdu::ReadExceptionStatus(pdu)))
        }
        DIAGNOSTICS => {
            let (input, pdu) = diagnostics_pdu_parser(input)?;
            Ok((input, Pdu::Diagnostics(pdu)))
        }
        GET_COMM_EVENT_COUNTER => {
            let (input, pdu) = get_comm_event_counter_pdu_parser(input)?;
            Ok((input, Pdu::GetCommEventCounter(pdu)))
        }
        GET_COMM_EVENT_LOG => {
            let (input, pdu) = get_comm_event_log_pdu_parser(input)?;
            Ok((input, Pdu::GetCommEventLog(pdu)))
        }
        WRITE_MULTIPLE_COILS => {
            let (input, pdu) = write_multiple_coils_pdu_parser(input)?;
            Ok((input, Pdu::WriteMultipleCoils(pdu)))
        }
        WRITE_MULTIPLE_REGISTERS => {
            let (input, pdu) = write_multiple_registers_pdu_parser(input)?;
            Ok((input, Pdu::WriteMultipleRegisters(pdu)))
        }
        REPORT_SLAVE_ID => {
            let (input, pdu) = report_slave_id_pdu_parser(input)?;
            Ok((input, Pdu::ReportSlaveId(pdu)))
        }
        READ_FILE_RECORD => {
            let (input, pdu) = read_file_record_pdu_parser(input)?;
            Ok((input, Pdu::ReadFileRecord(pdu)))
        }
        WRITE_FILE_RECORD => {
            let (input, pdu) = write_file_record_pdu_parser(input)?;
            Ok((input, Pdu::WriteFileRecord(pdu)))
        }
        MASK_WRITE_REGISTER => {
            let (input, pdu) = mask_write_register_pdu_parser(input)?;
            Ok((input, Pdu::MaskWriteRegister(pdu)))
        }
        READ_FIFO_QUEUE => {
            let (input, pdu) = read_fifo_queue_pdu_parser(input)?;
            Ok((input, Pdu::ReadFifoQueue(pdu)))
        }
        ENC_INITIALIZED_WRITE => {
            let (input, pdu) = enc_initialized_write_pdu_parser(input)?;
            Ok((input, Pdu::EncInitializedWrite(pdu)))
        }
        _ => Err(nom::Err::Error(nom::error::ErrorKind::NonDigit)),
    }
}

// Modbus PDU parsers
fn read_coils_pdu_parser(input: &[u8]) -> IResult<&[u8], ReadCoilsPdu> {
    let (input, starting_address) = take(2u8)(input)?;
    let (input, quantity) = take(2u8)(input)?;
    Ok((input, ReadCoilsPdu {
        starting_address: u16::from_be_bytes([starting_address[0], starting_address[1]]),
        quantity: u16::from_be_bytes([quantity[0], quantity[1]]),
    }))
}

fn read_discrete_inputs_pdu_parser(input: &[u8]) -> IResult<&[u8], ReadDiscreteInputsPdu> {
    let (input, starting_address) = take(2u8)(input)?;
    let (input, quantity) = take(2u8)(input)?;
    Ok((input, ReadDiscreteInputsPdu {
        starting_address: u16::from_be_bytes([starting_address[0], starting_address[1]]),
        quantity: u16::from_be_bytes([quantity[0], quantity[1]]),
    }))
}

fn read_holding_registers_pdu_parser(input: &[u8]) -> IResult<&[u8], ReadHoldingRegistersPdu> {
    let (input, starting_address) = take(2u8)(input)?;
    let (input, quantity) = take(2u8)(input)?;
    Ok((input, ReadHoldingRegistersPdu {
        starting_address: u16::from_be_bytes([starting_address[0], starting_address[1]]),
        quantity: u16::from_be_bytes([quantity[0], quantity[1]]),
    }))
}

fn read_input_registers_pdu_parser(input: &[u8]) -> IResult<&[u8], ReadInputRegistersPdu> {
    let (input, starting_address) = take(2u8)(input)?;
    let (input, quantity) = take(2u8)(input)?;
    Ok((input, ReadInputRegistersPdu {
        starting_address: u16::from_be_bytes([starting_address[0], starting_address[1]]),
        quantity: u16::from_be_bytes([quantity[0], quantity[1]]),
    }))
}

fn write_single_coil_pdu_parser(input: &[u8]) -> IResult<&[u8], WriteSingleCoilPdu> {
    let (input, output_address) = take(2u8)(input)?;
    let (input, output_value) = take(2u8)(input)?;
    Ok((input, WriteSingleCoilPdu {
        output_address: u16::from_be_bytes([output_address[0], output_address[1]]),
        output_value: (output_value[0] & 0xFF) != 0,
    }))
}

fn write_single_register_pdu_parser(input: &[u8]) -> IResult<&[u8], WriteSingleRegisterPdu> {
    let (input, register_address) = take(2u8)(input)?;
    let (input, register_value) = take(2u8)(input)?;
    Ok((input, WriteSingleRegisterPdu {
        register_address: u16::from_be_bytes([register_address[0], register_address[1]]),
        register_value: u16::from_be_bytes([register_value[0], register_value[1]]),
    }))
}

fn read_exception_status_pdu_parser(input: &[u8]) -> IResult<&[u8], ReadExceptionStatusPdu> {
    Ok((input, ReadExceptionStatusPdu {}))
}

fn diagnostics_pdu_parser(input: &[u8]) -> IResult<&[u8], DiagnosticsPdu> {
    let (input, sub_function) = take(1u8)(input)?;
    let (input, data) = many0(take(1u8))(input)?;
    Ok((input, DiagnosticsPdu {
        sub_function: sub_function[0],
        data: data.into_iter().flatten().cloned().collect(),
    }))
}

fn get_comm_event_counter_pdu_parser(input: &[u8]) -> IResult<&[u8], GetCommEventCounterPdu> {
    Ok((input, GetCommEventCounterPdu {}))
}

fn get_comm_event_log_pdu_parser(input: &[u8]) -> IResult<&[u8], GetCommEventLogPdu> {
    Ok((input, GetCommEventLogPdu {}))
}

fn write_multiple_coils_pdu_parser(input: &[u8]) -> IResult<&[u8], WriteMultipleCoilsPdu> {
    let (input, starting_address) = take(2u8)(input)?;
    let (input, quantity) = take(2u8)(input)?;
    let (input, coil_data) = many0(take(1u8))(input)?;
    Ok((input, WriteMultipleCoilsPdu {
        starting_address: u16::from_be_bytes([starting_address[0], starting_address[1]]),
        quantity: u16::from_be_bytes([quantity[0], quantity[1]]),
        coil_data: coil_data.into_iter().flatten().map(|x| (x[0] & 0xFF) != 0).collect(),
    }))
}

fn write_multiple_registers_pdu_parser(input: &[u8]) -> IResult<&[u8], WriteMultipleRegistersPdu> {
    let (input, starting_address) = take(2u8)(input)?;
    let (input, quantity) = take(2u8)(input)?;
    let (input, register_data) = many0(take(2u8))(input)?;
    Ok((input, WriteMultipleRegistersPdu {
        starting_address: u16::from_be_bytes([starting_address[0], starting_address[1]]),
        quantity: u16::from_be_bytes([quantity[0], quantity[1]]),
        register_data: register_data.into_iter().flatten().map(|x| u16::from_be_bytes([x[0], x[1]])).collect(),
    }))
}

fn report_slave_id_pdu_parser(input: &[u8]) -> IResult<&[u8], ReportSlaveIdPdu> {
    let (input, slave_id) = take(1u8)(input)?;
    let (input, run_indicator_status) = take(1u8)(input)?;
    Ok((input, ReportSlaveIdPdu {
        slave_id: slave_id[0],
        run_indicator_status: (run_indicator_status[0] & 0xFF) != 0,
    }))
}

fn read_file_record_pdu_parser(input: &[u8]) -> IResult<&[u8], ReadFileRecordPdu> {
    let (input, file_number) = take(2u8)(input)?;
    let (input, record_number) = take(2u8)(input)?;
    let (input, record_length) = take(2u8)(input)?;
    Ok((input, ReadFileRecordPdu {
        file_number: u16::from_be_bytes([file_number[0], file_number[1]]),
        record_number: u16::from_be_bytes([record_number[0], record_number[1]]),
        record_length: u16::from_be_bytes([record_length[0], record_length[1]]),
    }))
}

fn write_file_record_pdu_parser(input: &[u8]) -> IResult<&[u8], WriteFileRecordPdu> {
    let (input, file_number) = take(2u8)(input)?;
    let (input, record_number) = take(2u8)(input)?;
    let (input, record_data) = many0(take(1u8))(input)?;
    Ok((input, WriteFileRecordPdu {
        file_number: u16::from_be_bytes([file_number[0], file_number[1]]),
        record_number: u16::from_be_bytes([record_number[0], record_number[1]]),
        record_data: record_data.into_iter().flatten().cloned().collect(),
    }))
}

fn mask_write_register_pdu_parser(input: &[u8]) -> IResult<&[u8], MaskWriteRegisterPdu> {
    let (input, register_address) = take(2u8)(input)?;
    let (input, and_mask) = take(2u8)(input)?;
    let (input, or_mask) = take(2u8)(input)?;
    Ok((input, MaskWriteRegisterPdu {
        register_address: u16::from_be_bytes([register_address[0], register_address[1]]),
        and_mask: u16::from_be_bytes([and_mask[0], and_mask[1]]),
        or_mask: u16::from_be_bytes([or_mask[0], or_mask[1]]),
    }))
}

fn read_fifo_queue_pdu_parser(input: &[u8]) -> IResult<&[u8], ReadFifoQueuePdu> {
    let (input, fifo_pointer_address) = take(2u8)(input)?;
    Ok((input, ReadFifoQueuePdu {
        fifo_pointer_address: u16::from_be_bytes([fifo_pointer_address[0], fifo_pointer_address[1]]),
    }))
}

fn enc_initialized_write_pdu_parser(input: &[u8]) -> IResult<&[u8], EncInitializedWritePdu> {
    let (input, sub_function) = take(1u8)(input)?;
    let (input, data) = many0(take(1u8))(input)?;
    Ok((input, EncInitializedWritePdu {
        sub_function: sub_function[0],
        data: data.into_iter().flatten().cloned().collect(),
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Failed to read file");

    let (_remaining, pdu) = adu_parser(&data).expect("Failed to parse ADU");

    println!("{:?}", pdu);
}