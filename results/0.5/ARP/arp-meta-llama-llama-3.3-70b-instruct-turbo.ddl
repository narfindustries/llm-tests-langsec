domain ARP {
  type ARPMessage {
    htype: uint16,
    ptype: uint16,
    hlen: uint8,
    plen: uint8,
    oper: uint16,
    sha: array uint8 of size hlen,
    spa: array uint8 of size plen,
    tha: array uint8 of size hlen,
    tpa: array uint8 of size plen
  }

  type ARPRequest extends ARPMessage {
    oper = 1,
    tha = array uint8 of size hlen with value [0]
  }

  type ARPReply extends ARPMessage {
    oper = 2
  }

  type RARPRequest extends ARPMessage {
    oper = 3
  }

  type RARPReply extends ARPMessage {
    oper = 4
  }

  type EthernetARP extends ARPMessage {
    htype = 1,
    ptype = 0x0800,
    hlen = 6,
    plen = 4
  }

  type IPv6ARP extends ARPMessage {
    htype = 1,
    ptype = 0x86DD,
    hlen = 6,
    plen = 16
  }
}