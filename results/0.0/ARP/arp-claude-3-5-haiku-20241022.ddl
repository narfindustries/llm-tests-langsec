type ARP = {
    hardware_type: uint16,
    protocol_type: uint16,
    hardware_addr_len: uint8,
    protocol_addr_len: uint8,
    operation: enum {
        Request = 1,
        Reply = 2
    },
    sender_hardware_addr: [uint8],
    sender_protocol_addr: [uint8],
    target_hardware_addr: [uint8],
    target_protocol_addr: [uint8]
} deriving (Read, Show)