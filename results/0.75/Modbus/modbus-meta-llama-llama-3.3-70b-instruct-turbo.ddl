grammar Modbus {
  rule frame = 
    tran_id: uint16 
    proto_id: uint16 
    len: uint16 
    unit_id: uint8 
    func_code: uint8 
    data: data_element*

  rule data_element = 
    addr: uint16 
    num_regs: uint16 
    val: uint16
}

grammar data_element {
  rule data = 
    byte_count: uint8 
    byte_values: uint8*
}

grammar function_code_1 {
  rule frame = 
    func_code: uint8 (eq(0x01)) 
    data: data_element*
}

grammar function_code_2 {
  rule frame = 
    func_code: uint8 (eq(0x02)) 
    data: data_element*
}

grammar function_code_3 {
  rule frame = 
    func_code: uint8 (eq(0x03)) 
    data: data_element*
}

grammar function_code_4 {
  rule frame = 
    func_code: uint8 (eq(0x04)) 
    data: data_element*
}

grammar function_code_5 {
  rule frame = 
    func_code: uint8 (eq(0x05)) 
    data: data_element*
}

grammar function_code_6 {
  rule frame = 
    func_code: uint8 (eq(0x06)) 
    data: data_element*
}

grammar function_code_15 {
  rule frame = 
    func_code: uint8 (eq(0x0F)) 
    data: data_element*
}

grammar function_code_16 {
  rule frame = 
    func_code: uint8 (eq(0x10)) 
    data: data_element*
}