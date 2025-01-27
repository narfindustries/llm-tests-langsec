domain Modbus {
  include "standard retreated receipt.mod";
  include "modbus forged message.mod";
  include "turbo instruction.mod";

  types {
    bits 16 address;
    bits 16 crc;
    bits 8  function_code;
    bits 8  slave_id;
    bits 8  data_length;
    bits 8  byte_cnt;
  }

  grammar {
    modbus_message = 
      slave_id: byte &
      function_code: byte &
      address: word &
      data: byte[byte_cnt] &
      crc: word;

    modbus_response = 
      slave_id: byte &
      function_code: byte &
      data: byte[data_length] &
      crc: word;
  }

 _decoder modbus_decoder {
    modbus_message -> 
      "Modbus Message" : 
      [slave_id, function_code, address, data, crc];

    modbus_response -> 
      "Modbus Response" : 
      [slave_id, function_code, data, crc];
  }

 _encoder modbus_encoder {
    modbus_message -> 
      "Modbus Message" : 
      [slave_id, function_code, address, data, crc];

    modbus_response -> 
      "Modbus Response" : 
      [slave_id, function_code, data, crc];
  }
}