Protocol Modbus {
    // RTU Frame
    Structure RTUFrame {
        u8 address : 1..247; // Address Field: 1-247
        u8 function_code : 1..127; // Function code: 1-127
        byte[] data; // Data field, size varies based on the function
        u16 crc; // CRC field: error-checking code
    }

    // TCP Frame
    Structure TCPFrame {
        u16 transaction_id; // Transaction Identifier
        u16 protocol_id = 0; // Protocol Identifier, always 0 for Modbus TCP
        u16 length; // Length of the following bytes
        u8 unit_id : 1..247; // Unit Identifier, similar to address in RTU
        u8 function_code : 1..127; // Function code: 1-127
        byte[] data; // Data field, size varies based on the function
    }

    Variant Message {
        0: RTUFrame rtu_frame;
        1: TCPFrame tcp_frame;
    }

    Message message;
}