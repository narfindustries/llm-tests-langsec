arp_packet {
  hardware_type: uint16,
  protocol_type: uint16,
  hardware_address_length: uint8,
  protocol_address_length: uint8,
  operation: uint16,
  sender_hardware_address: bytes => hardware_address_length,
  sender_protocol_address: bytes => protocol_address_length,
  target_hardware_address: bytes => hardware_address_length,
  target_protocol_address: bytes => protocol_address_length
}

arp_request {
  extends: arp_packet,
  operation: 1
}

arp_reply {
  extends: arp_packet,
  operation: 2
}

rarp_request {
  extends: arp_packet,
  operation: 3
}

rarp_reply {
  extends: arp_packet,
  operation: 4
}

drarp_request {
  extends: arp_packet,
  operation: 5
}

drarp_reply {
  extends: arp_packet,
  operation: 6
}

drarp_error {
  extends: arp_packet,
  operation: 7
}

inarp_request {
  extends: arp_packet,
  operation: 8
}

inarp_reply {
  extends: arp_packet,
  operation: 9
}