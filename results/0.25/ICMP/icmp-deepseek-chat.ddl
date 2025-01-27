import Daedalus

def ICMPHeader =
  block
    uint8 type
    uint8 code
    uint16 checksum
    uint16 identifier
    uint16 sequenceNumber

def ICMPEchoRequest =
  block
    ICMPHeader header
    uint8[..] data

def ICMPEchoReply =
  block
    ICMPHeader header
    uint8[..] data

def ICMPPacket =
  block
    ICMPHeader header
    case header.type of
      0 -> ICMPEchoReply echoReply
      8 -> ICMPEchoRequest echoRequest
      _ -> uint8[..] payload

def Main =
  block
    ICMPPacket packet