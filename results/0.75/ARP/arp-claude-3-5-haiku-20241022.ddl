type ARP = struct {
    hardware_type: uint16,
    protocol_type: uint16,
    hardware_addr_len: uint8,
    protocol_addr_len: uint8,
    operation: uint16,
    sender_hardware_addr: array<uint8, 6>,
    sender_protocol_addr: array<uint8, 4>,
    target_hardware_addr: array<uint8, 6>,
    target_protocol_addr: array<uint8, 4>
}

predicate valid_arp(arp: ARP) {
    arp.hardware_addr_len == 6 and
    arp.protocol_addr_len == 4 and
    (arp.operation == 1 or arp.operation == 2)
}

type ARPPacket = {
    arp: ARP
}

let valid_hardware_type = [1]
let valid_protocol_type = [0x0800]
let valid_operation = [1, 2]

let is_valid_arp_packet = fn(arp: ARP) -> bool {
    arp.hardware_type in valid_hardware_type and
    arp.protocol_type in valid_protocol_type and
    arp.operation in valid_operation
}