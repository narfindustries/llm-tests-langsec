protocol Modbus {
    type Frame = {
        slave_address: u8,
        function_code: FunctionCode,
        payload: Payload,
        crc: u16
    }

    type FunctionCode = enum(u8) {
        ReadCoils = 0x01,
        ReadDiscreteInputs = 0x02,
        ReadHoldingRegisters = 0x03,
        ReadInputRegisters = 0x04,
        WriteSingleCoil = 0x05,
        WriteSingleRegister = 0x06,
        WriteMultipleCoils = 0x0F,
        WriteMultipleRegisters = 0x10,
        FileRecordRead = 0x14,
        FileRecordWrite = 0x15,
        EncapsulatedInterface = 0x2B
    }

    type Payload = variant {
        ReadCoils: {
            start_address: u16,
            quantity: u16
        },
        ReadDiscreteInputs: {
            start_address: u16,
            quantity: u16
        },
        ReadHoldingRegisters: {
            start_address: u16,
            quantity: u16
        },
        ReadInputRegisters: {
            start_address: u16,
            quantity: u16
        },
        WriteSingleCoil: {
            coil_address: u16,
            coil_value: u16
        },
        WriteSingleRegister: {
            register_address: u16,
            register_value: u16
        },
        WriteMultipleCoils: {
            start_address: u16,
            quantity: u16,
            byte_count: u8,
            coil_values: [u8]
        },
        WriteMultipleRegisters: {
            start_address: u16,
            quantity: u16,
            byte_count: u8,
            register_values: [u16]
        },
        FileRecordRead: {
            reference_type: u8,
            file_number: u16,
            record_number: u16,
            record_length: u16
        },
        FileRecordWrite: {
            reference_type: u8,
            file_number: u16,
            record_number: u16,
            record_length: u16,
            record_data: [u8]
        },
        EncapsulatedInterface: {
            mei_type: u8,
            device_id_code: u8,
            object_id: u8,
            object_value: [u8]
        }
    }
}