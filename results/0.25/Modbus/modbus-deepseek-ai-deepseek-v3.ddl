Modbus {
    transaction_identifier: uint16;
    protocol_identifier: uint16;
    length: uint16;
    unit_identifier: uint8;
    function_code: uint8;
    data: switch (function_code) {
        0x01: ReadCoils {
            starting_address: uint16;
            quantity_of_coils: uint16;
        };
        0x02: ReadDiscreteInputs {
            starting_address: uint16;
            quantity_of_inputs: uint16;
        };
        0x03: ReadHoldingRegisters {
            starting_address: uint16;
            quantity_of_registers: uint16;
        };
        0x04: ReadInputRegisters {
            starting_address: uint16;
            quantity_of_registers: uint16;
        };
        0x05: WriteSingleCoil {
            output_address: uint16;
            output_value: uint16;
        };
        0x06: WriteSingleRegister {
            register_address: uint16;
            register_value: uint16;
        };
        0x0F: WriteMultipleCoils {
            starting_address: uint16;
            quantity_of_outputs: uint16;
            byte_count: uint8;
            outputs_value: bytes[byte_count];
        };
        0x10: WriteMultipleRegisters {
            starting_address: uint16;
            quantity_of_registers: uint16;
            byte_count: uint8;
            registers_value: bytes[byte_count];
        };
        0x16: MaskWriteRegister {
            reference_address: uint16;
            and_mask: uint16;
            or_mask: uint16;
        };
        0x17: ReadWriteMultipleRegisters {
            read_starting_address: uint16;
            quantity_to_read: uint16;
            write_starting_address: uint16;
            quantity_to_write: uint16;
            write_byte_count: uint8;
            write_registers_value: bytes[write_byte_count];
        };
        0x18: ReadFIFOQueue {
            fifo_pointer_address: uint16;
        };
        0x2B: EncapsulatedInterfaceTransport {
            mei_type: uint8;
            mei_data: switch (mei_type) {
                0x0D: CANopenGeneralReference {
                    reference_type: uint8;
                    reference_address: uint16;
                    reference_data: bytes[length - 6];
                };
                0x0E: ReadDeviceIdentification {
                    device_identification_code: uint8;
                    object_id: uint8;
                    conformity_level: uint8;
                    more_follows: uint8;
                    next_object_id: uint8;
                    object_value: bytes[length - 6];
                };
                default: bytes[length - 2];
            };
        };
        default: bytes[length - 2];
    };
    exception_code: optional uint8;
    checksum: optional uint16;
    error_code: optional uint8;
    mei_error_code: optional uint8;
    mei_error_data: optional bytes[length - 2];
    mei_sub_function_code: optional uint8;
    mei_sub_function_data: optional bytes[length - 2];
}