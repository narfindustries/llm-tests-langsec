namespace Modbus

// Modbus TCP/IP ADU
struct ModbusTCP
    uint16 transaction_id
    uint16 protocol_id
    uint16 length
    uint8 unit_id
    ModbusPDU pdu
end struct

// Modbus RTU Frame
struct ModbusRTU
    uint8 address
    ModbusPDU pdu
    uint16 crc
end struct

// Modbus ASCII Frame
struct ModbusASCII
    char start = ':'
    uint8 address
    ModbusPDU pdu
    uint8 lrc
    char[2] end = "\r\n"
end struct

// Modbus PDU
struct ModbusPDU
    uint8 function_code
    Data data
end struct

// Data field varies based on function code
union Data
    // Read Coils, Read Discrete Inputs, Read Holding Registers, Read Input Registers
    struct ReadRequest
        uint16 start_address
        uint16 quantity
    end struct if (pdu.function_code in [0x01, 0x02, 0x03, 0x04])

    // Write Single Coil, Write Single Register
    struct WriteSingleRequest
        uint16 address
        uint16 value
    end struct if (pdu.function_code in [0x05, 0x06])

    // Write Multiple Coils, Write Multiple Registers
    struct WriteMultipleRequest
        uint16 start_address
        uint16 quantity
        uint8 byte_count
        uint8[] values
    end struct if (pdu.function_code in [0x0F, 0x10])

    // Report Slave ID
    struct ReportSlaveID
        uint8 byte_count
        uint8[] data
    end struct if (pdu.function_code == 0x11)

    // Default case for other function codes
    struct DefaultData
        uint8[] raw_data
    end struct if (pdu.function_code not in [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x0F, 0x10, 0x11])
end union

end namespace