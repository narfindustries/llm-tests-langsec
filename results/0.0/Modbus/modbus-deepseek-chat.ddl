// Modbus Protocol Specification in Daedalus
// This specification defines the structure of Modbus TCP frames.

import "network.ddl"

struct ModbusTCP {
    transaction_id: uint16;
    protocol_id: uint16;
    length: uint16;
    unit_id: uint8;
    function_code: uint8;
    data: bytes[length - 2]; // Subtract 2 for unit_id and function_code
}

struct ModbusTCPRequest : ModbusTCP {
    // Additional fields for Modbus TCP requests can be added here
}

struct ModbusTCPResponse : ModbusTCP {
    // Additional fields for Modbus TCP responses can be added here
}

// Example usage
// modbus_request: ModbusTCPRequest;
// modbus_response: ModbusTCPResponse;