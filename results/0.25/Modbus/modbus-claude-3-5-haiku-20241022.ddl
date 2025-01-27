type modbus_function_code = 
  | ReadCoils                  -- 0x01
  | ReadDiscreteInputs         -- 0x02 
  | ReadHoldingRegisters       -- 0x03
  | ReadInputRegisters         -- 0x04
  | WriteSingleCoil            -- 0x05
  | WriteSingleRegister        -- 0x06
  | WriteMultipleCoils         -- 0x0F
  | WriteMultipleRegisters     -- 0x10

type modbus_exception_code =
  | IllegalFunction            -- 0x01
  | IllegalDataAddress         -- 0x02
  | IllegalDataValue           -- 0x03
  | SlaveDeviceFailure         -- 0x04
  | Acknowledge                -- 0x05
  | SlaveDeviceBusy            -- 0x06
  | MemoryParityError          -- 0x08
  | GatewayPathUnavailable     -- 0x0A
  | GatewayTargetDeviceFailed  -- 0x0B

type modbus_frame = {
  slave_address: u8,
  function_code: modbus_function_code,
  data: list u8,
  crc: u16
}

type modbus_exception_response = {
  slave_address: u8,
  exception_function_code: u8,
  exception_code: modbus_exception_code
}

let is_valid_function_code (fc: u8) : bool =
  match fc with
  | 0x01 -> true
  | 0x02 -> true
  | 0x03 -> true
  | 0x04 -> true
  | 0x05 -> true
  | 0x06 -> true
  | 0x0F -> true
  | 0x10 -> true
  | _ -> false

let calculate_crc (frame: list u8) : u16 =
  -- CRC-16 (MODBUS) calculation implementation
  0xFFFF  -- Placeholder return

let validate_frame (frame: modbus_frame) : bool =
  let calculated_crc = calculate_crc 
    (frame.slave_address :: 
     (u8_of_function_code frame.function_code) :: 
     frame.data) in
  calculated_crc = frame.crc

let u8_of_function_code (fc: modbus_function_code) : u8 =
  match fc with
  | ReadCoils -> 0x01
  | ReadDiscreteInputs -> 0x02
  | ReadHoldingRegisters -> 0x03
  | ReadInputRegisters -> 0x04
  | WriteSingleCoil -> 0x05
  | WriteSingleRegister -> 0x06
  | WriteMultipleCoils -> 0x0F
  | WriteMultipleRegisters -> 0x10

let parse_modbus_frame (raw_data: list u8) : 
  result modbus_frame modbus_exception_response =
  match raw_data with
  | slave_addr :: func_code :: rest ->
    if not (is_valid_function_code func_code) then
      Error {
        slave_address = slave_addr;
        exception_function_code = func_code lor 0x80;
        exception_code = IllegalFunction
      }
    else 
      -- Additional frame parsing logic would go here
      Error {
        slave_address = slave_addr;
        exception_function_code = func_code lor 0x80;
        exception_code = IllegalDataAddress
      }
  | _ -> 
    Error {
      slave_address = 0x00;
      exception_function_code = 0x00;
      exception_code = IllegalFunction
    }