type icmp_header = {
    type: uint8,
    code: uint8,
    checksum: uint16,
    identifier: uint16,
    sequence_number: uint16,
    data: bytes
}
