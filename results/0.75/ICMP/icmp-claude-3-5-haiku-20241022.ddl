type ICMPPacket = format {
  type_: u8;
  code: u8;
  checksum: u16;
  body: variant {
    EchoReply: {
      identifier: u16;
      sequence_number: u16;
      payload: [u8]
    },
    DestinationUnreachable: {
      unused: u16;
      next_hop_mtu: u16;
      original_datagram: [u8]
    },
    SourceQuench: {
      unused: u32;
      original_datagram: [u8]
    },
    Redirect: {
      gateway_address: u32;
      original_datagram: [u8]
    },
    EchoRequest: {
      identifier: u16;
      sequence_number: u16;
      payload: [u8]
    },
    RouterAdvertisement: {
      number_addresses: u8;
      address_entry_size: u8;
      lifetime: u16;
      router_addresses: [{
        address: u32;
        preference_level: u32
      }]
    },
    RouterSolicitation: {},
    TimeExceeded: {
      unused: u16;
      original_datagram: [u8]
    },
    ParameterProblem: {
      pointer: u8;
      unused: u24;
      original_datagram: [u8]
    },
    Timestamp: {
      identifier: u16;
      sequence_number: u16;
      originate_timestamp: u32;
      receive_timestamp: u32;
      transmit_timestamp: u32
    },
    TimestampReply: {
      identifier: u16;
      sequence_number: u16;
      originate_timestamp: u32;
      receive_timestamp: u32;
      transmit_timestamp: u32
    },
    InformationRequest: {
      identifier: u16;
      sequence_number: u16
    },
    InformationReply: {
      identifier: u16;
      sequence_number: u16
    }
  }
}