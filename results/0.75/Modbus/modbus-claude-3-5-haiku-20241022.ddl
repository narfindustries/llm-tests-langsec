type modbus_function_code = 
  | Read_Coils                  -- 0x01
  | Read_Discrete_Inputs        -- 0x02
  | Read_Holding_Registers      -- 0x03
  | Read_Input_Registers        -- 0x04
  | Write_Single_Coil           -- 0x05
  | Write_Single_Register       -- 0x06
  | Read_Exception_Status       -- 0x07
  | Diagnostics                 -- 0x08
  | Write_Multiple_Coils        -- 0x0F
  | Write_Multiple_Registers    -- 0x10
  | Report_Slave_ID             -- 0x11

type modbus_exception_code = 
  | Illegal_Function
  | Illegal_Data_Address
  | Illegal_Data_Value
  | Slave_Device_Failure
  | Acknowledge
  | Slave_Device_Busy
  | Negative_Acknowledge
  | Memory_Parity_Error

type modbus_frame = {
  slave_address: u8,
  function_code: modbus_function_code,
  data: list<u8>,
  crc: u16
}

let calculate_crc16(frame_data: list<u8>) -> u16 = 
  let initial_value = 0xFFFF in
  let polynomial = 0xA001 in
  let crc = frame_data.fold(initial_value, fun(current_crc, byte) -> 
    let xored_crc = current_crc ^ byte in
    let result = [0..7].fold(xored_crc, fun(temp_crc, _) ->
      if temp_crc & 0x0001 == 1 
      then (temp_crc >> 1) ^ polynomial 
      else temp_crc >> 1
    ) in
    result
  ) in
  crc

let validate_modbus_frame(frame: modbus_frame) -> bool = 
  let frame_data = [
    frame.slave_address, 
    frame.function_code as u8, 
  ] in
  let calculated_crc = calculate_crc16(frame_data) in
  calculated_crc == frame.crc

parser modbus_protocol = {
  slave_address: u8,
  function_code: modbus_function_code,
  data_length: u8,
  data: list<u8> where data.length == data_length,
  crc: u16
} when validate_modbus_frame({
  slave_address: slave_address,
  function_code: function_code,
  data: data,
  crc: crc
})