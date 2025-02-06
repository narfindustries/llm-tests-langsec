ModbusRTU = struct {
    Address: uint8; // Slave address (1-247, 0 for broadcast)
    FunctionCode: uint8; // Function code (e.g., 01 for Read Coils)
    Data: bytes; // Data field, length varies by function
    CRC: uint16; // Error check (Cyclic Redundancy Check)

    let DataLength = calculateDataLength(FunctionCode);

    calculateDataLength = function(funcCode: uint8): uint16 {
        switch (funcCode) {
            case 0x01, 0x02, 0x03, 0x04:
                return 4; // Starting address and quantity
            case 0x05, 0x06:
                return 4; // Address and value
            case 0x0F, 0x10:
                return 5 + (Data[4] as uint8); // Address, quantity, byte count, and values
            default:
                return Data.length; // Manufacturer-specific or other
        }
    }
}

ModbusTCP = struct {
    TransactionID: uint16; // Transaction Identifier
    ProtocolID: uint16; // Protocol Identifier (always 0)
    Length: uint16; // Length of remaining bytes
    UnitID: uint8; // Unit Identifier (1-247, 0 for broadcast)
    FunctionCode: uint8; // Function code (e.g., 01 for Read Coils)
    Data: bytes; // Data field, length varies by function

    let DataLength = Length - 2; // Subtract UnitID and FunctionCode

    calculateDataLength = function(funcCode: uint8): uint16 {
        switch (funcCode) {
            case 0x01, 0x02, 0x03, 0x04:
                return 4; // Starting address and quantity
            case 0x05, 0x06:
                return 4; // Address and value
            case 0x0F, 0x10:
                return 5 + (Data[4] as uint8); // Address, quantity, byte count, and values
            default:
                return Data.length; // Manufacturer-specific or other
        }
    }
}

ModbusExceptionResponse = struct {
    Address: uint8; // Slave address
    FunctionCode: uint8; // Function code with highest bit set
    ExceptionCode: uint8; // Exception code (e.g., 01 for Illegal Function)
    CRC: uint16; // Error check (Cyclic Redundancy Check)
}