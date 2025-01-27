ุทร define modbus_meta_llama : byte[1024] = {
  // Transaction ID
  transaction_id: uint16 @ 0:16,
  
  // Protocol ID
  protocol_id: uint16 @ 1:16,
  
  // Length
  length: uint16 @ 2:16,
  
  // Unit ID
  unit_id: uint8 @ 3:8,
  
  // Function Code
  function_code: uint8 @ 4:8,
  
  // Starting Address
  starting_address: uint16 @ 5:16,
  
  // Quantity
  quantity: uint16 @ 6:16,
  
  // Data
  data: byte[length - 5] @ 7:8,
}

define modbus_response_llama : byte[1024] = {
  // Transaction ID
  transaction_id: uint16 @ 0:16,
  
  // Protocol ID
  protocol_id: uint16 @ 1:16,
  
  // Length
  length: uint16 @ 2:16,
  
  // Unit ID
  unit_id: uint8 @ 3:8,
  
  // Function Code
  function_code: uint8 @ 4:8,
  
  // Byte Count
  byte_count: uint8 @ 5:8,
  
  // Register Values
  register_values: byte[byte_count] @ 6:8,
}

define modbus_request_llama : byte[1024] = {
  // Transaction ID
  transaction_id: uint16 @ 0:16,
  
  // Protocol ID
  protocol_id: uint16 @ 1:16,
  
  // Length
  length: uint16 @ 2:16,
  
  // Unit ID
  unit_id: uint8 @ 3:8,
  
  // Function Code
  function_code: uint8 @ 4:8,
  
  // Starting Address
  starting_address: uint16 @ 5:16,
  
  // Quantity
  quantity: uint16 @ 6:16,
}

define modbus : byte[1024] = {
  if @0:8 == 0x01 {
    modbus_request_llama
  } else if @0:8 == 0x02 {
    modbus_response_llama
  } else {
    modbus_meta_llama
  }
}