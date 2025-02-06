struct ModbusProtocol {

    // Modbus Application Data Unit (ADU) for Modbus RTU
    struct ModbusRTUFrame {
        uint8 address; // Slave address (1-247), 0 for broadcast
        ModbusPDU pdu; // Protocol Data Unit
        uint16 crc; // CRC for error-checking
    }

    // Modbus Application Data Unit (ADU) for Modbus TCP
    struct ModbusTCPFrame {
        uint16 transactionIdentifier; // Transaction ID (0-65535)
        uint16 protocolIdentifier; // Protocol ID, always 0 for Modbus
        uint16 length; // Remaining length of the frame (6-260)
        uint8 unitIdentifier; // Unit ID (0-255)
        ModbusPDU pdu; // Protocol Data Unit
    }

    // Modbus Protocol Data Unit (PDU)
    struct ModbusPDU {
        uint8 functionCode; // Function code (1-255)
        DataField data; // Data specific to the function
    }

    // Data Field based on Function Code
    union DataField {
        ReadRequestData readRequest if (parent.functionCode in [1, 2, 3, 4]);
        WriteSingleRequestData writeSingleRequest if (parent.functionCode in [5, 6]);
        WriteMultipleRequestData writeMultipleRequest if (parent.functionCode in [15, 16]);
        RawData rawData;
    }

    // Read Request Data Structure
    struct ReadRequestData {
        uint16 startAddress; // Starting address
        uint16 quantity; // Quantity of items to read
    }

    // Write Single Request Data Structure
    struct WriteSingleRequestData {
        uint16 address; // Address to write
        uint16 value; // Value to write
    }

    // Write Multiple Request Data Structure
    struct WriteMultipleRequestData {
        uint16 startAddress; // Starting address
        uint16 quantity; // Quantity of items to write
        uint8 byteCount; // Number of data bytes
        bytes data[byteCount]; // Data to write
    }

    // Raw Data Structure for unsupported function codes
    struct RawData {
        bytes rawData[]; // Raw data
    }
}