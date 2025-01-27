type modbus_function_code = 
  | Read_Coils                 of uint(8)
  | Read_Discrete_Inputs       of uint(8)
  | Read_Holding_Registers     of uint(8)
  | Read_Input_Registers       of uint(8)
  | Write_Single_Coil          of uint(8)
  | Write_Single_Register      of uint(8)
  | Write_Multiple_Coils       of uint(8)
  | Write_Multiple_Registers   of uint(8)

type modbus_request = {
  slave_address : uint(8),
  function_code : modbus_function_code,
  data : list(uint(8))
}

type modbus_response = {
  slave_address : uint(8),
  function_code : modbus_function_code,
  data : list(uint(8))
}

let parse_modbus_request = do {
  slave_addr <- take_uint8;
  func_code <- take_uint8;
  request_data <- parse_request_data func_code;
  return {
    slave_address = slave_addr;
    function_code = func_code;
    data = request_data
  }
}

let parse_request_data (func_code : modbus_function_code) = do {
  match func_code with 
  | Read_Coils _ -> do {
      start_addr <- take_uint16_be;
      num_coils <- take_uint16_be;
      return [uint8_of_uint16 (start_addr >> 8), uint8_of_uint16 start_addr, 
              uint8_of_uint16 (num_coils >> 8), uint8_of_uint16 num_coils]
    }
  | Read_Discrete_Inputs _ -> do {
      start_addr <- take_uint16_be;
      num_inputs <- take_uint16_be;
      return [uint8_of_uint16 (start_addr >> 8), uint8_of_uint16 start_addr, 
              uint8_of_uint16 (num_inputs >> 8), uint8_of_uint16 num_inputs]
    }
  | Read_Holding_Registers _ -> do {
      start_addr <- take_uint16_be;
      num_registers <- take_uint16_be;
      return [uint8_of_uint16 (start_addr >> 8), uint8_of_uint16 start_addr, 
              uint8_of_uint16 (num_registers >> 8), uint8_of_uint16 num_registers]
    }
  | Read_Input_Registers _ -> do {
      start_addr <- take_uint16_be;
      num_registers <- take_uint16_be;
      return [uint8_of_uint16 (start_addr >> 8), uint8_of_uint16 start_addr, 
              uint8_of_uint16 (num_registers >> 8), uint8_of_uint16 num_registers]
    }
  | Write_Single_Coil _ -> do {
      coil_addr <- take_uint16_be;
      coil_value <- take_uint16_be;
      return [uint8_of_uint16 (coil_addr >> 8), uint8_of_uint16 coil_addr, 
              uint8_of_uint16 (coil_value >> 8), uint8_of_uint16 coil_value]
    }
  | Write_Single_Register _ -> do {
      register_addr <- take_uint16_be;
      register_value <- take_uint16_be;
      return [uint8_of_uint16 (register_addr >> 8), uint8_of_uint16 register_addr, 
              uint8_of_uint16 (register_value >> 8), uint8_of_uint16 register_value]
    }
  | Write_Multiple_Coils _ -> do {
      start_addr <- take_uint16_be;
      num_coils <- take_uint16_be;
      byte_count <- take_uint8;
      coil_data <- take_list byte_count;
      return ([uint8_of_uint16 (start_addr >> 8), uint8_of_uint16 start_addr, 
               uint8_of_uint16 (num_coils >> 8), uint8_of_uint16 num_coils, 
               byte_count] ++ coil_data)
    }
  | Write_Multiple_Registers _ -> do {
      start_addr <- take_uint16_be;
      num_registers <- take_uint16_be;
      byte_count <- take_uint8;
      register_data <- take_list byte_count;
      return ([uint8_of_uint16 (start_addr >> 8), uint8_of_uint16 start_addr, 
               uint8_of_uint16 (num_registers >> 8), uint8_of_uint16 num_registers, 
               byte_count] ++ register_data)
    }
}

let generate_modbus_request (req : modbus_request) = do {
  put_uint8 req.slave_address;
  put_uint8 (match req.function_code with
    | Read_Coils _ -> 1
    | Read_Discrete_Inputs _ -> 2
    | Read_Holding_Registers _ -> 3
    | Read_Input_Registers _ -> 4
    | Write_Single_Coil _ -> 5
    | Write_Single_Register _ -> 6
    | Write_Multiple_Coils _ -> 15
    | Write_Multiple_Registers _ -> 16);
  put_list req.data
}