type NTP_Packet = struct {
    li_vn_mode : bitfield {
        li   : 2;  // Leap Indicator
        vn   : 3;  // Version Number
        mode : 3;  // Mode
    };
    stratum      : uint8;   // Stratum level of the local clock
    poll         : int8;    // Poll interval
    precision    : int8;    // Precision of the local clock

    root_delay       : uint32;  // Total round-trip delay to the primary reference source
    root_dispersion  : uint32;  // Total dispersion to the primary reference source
    reference_id     : uint32;  // Reference identifier

    reference_timestamp : uint64;  // Last time the local clock was updated
    origin_timestamp    : uint64;  // Transmit time of the request by the client
    receive_timestamp   : uint64;  // Receive time of the request by the server
    transmit_timestamp  : uint64;  // Transmit time of the response by the server
};

type NTP_Control_Message = struct {
    flags : bitfield {
        r : 1;  // Response bit
        more : 1;  // More packets bit
        error : 1;  // Error bit
        opcode : 5;  // Opcode
    };
    sequence_number : uint16;  // Sequence number of the control message
    status : uint16;  // Status field for the control message
    association_id : uint16;  // Association identifier
    offset : uint16;  // Offset
    count : uint16;  // Count of data in the payload
    data : bytes;  // Optional data based on the opcode
};

type NTP_Private = struct {
    request_code : uint8;  // Request code
    error_code : uint8;  // Error code if any
    sequence_number : uint16;  // Sequence number
    status : uint16;  // Status
    association_id : uint16;  // Association identifier
    data : bytes;  // Data specific to the request
};

type NTP_Message = union {
    standard : NTP_Packet;
    control : NTP_Control_Message;
    private : NTP_Private;
} switch (NTP_Packet.li_vn_mode.mode) {
    case 6: control;
    case 7: private;
    default: standard;
};