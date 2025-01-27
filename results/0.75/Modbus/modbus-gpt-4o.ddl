module Modbus

-- Define the Modbus Protocol Data Unit
PDU = structure {
    function_code: uint8,                -- Function code
    data: switch (self.function_code) {  -- Data based on function code
        0x01 -> coil_status,
        0x02 -> input_status,
        0x03 -> holding_registers,
        0x04 -> input_registers,
        0x05 -> write_single_coil,
        0x06 -> write_single_register,
        0x0F -> write_multiple_coils,
        0x10 -> write_multiple_registers
    }
}

-- Define specific structures for different function codes

coil_status = structure {
    byte_count: uint8,
    coil_status: array(uint8, self.byte_count)
}

input_status = structure {
    byte_count: uint8,
    input_status: array(uint8, self.byte_count)
}

holding_registers = structure {
    byte_count: uint8,
    registers: array(uint16, self.byte_count / 2)
}

input_registers = structure {
    byte_count: uint8,
    registers: array(uint16, self.byte_count / 2)
}

write_single_coil = structure {
    output_address: uint16,
    output_value: uint16
}

write_single_register = structure {
    register_address: uint16,
    register_value: uint16
}

write_multiple_coils = structure {
    starting_address: uint16,
    quantity_of_outputs: uint16,
    byte_count: uint8,
    outputs_value: array(uint8, self.byte_count)
}

write_multiple_registers = structure {
    starting_address: uint16,
    quantity_of_registers: uint16,
    byte_count: uint8,
    registers_value: array(uint16, self.byte_count / 2)
}

-- Combine the Modbus Application Data Unit (ADU)
ADU = structure {
    address: uint8,  -- Address of the slave
    pdu: PDU,        -- Protocol Data Unit
    crc: uint16      -- Error checking
}

-- Overall Modbus Frame
ModbusFrame = structure {
    mbap_header: structure {
        transaction_id: uint16,
        protocol_id: uint16,
        length: uint16,
        unit_id: uint8
    },
    adu: ADU
}