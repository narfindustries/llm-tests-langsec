module ICMP {
  import Network.IPv4;

  type ICMPMessage = struct {
    type       : UInt8;
    code       : UInt8;
    checksum   : UInt16;
    restOfHeader : match type {
      0 => EchoReply;
      8 => EchoRequest;
      _ => UnknownMsg;
    }
  }

  type EchoReply = struct {
    identifier : UInt16;
    sequenceNum: UInt16;
    data       : Bytes;
  }

  type EchoRequest = struct {
    identifier : UInt16;
    sequenceNum: UInt16;
    data       : Bytes;
  }

  type UnknownMsg = struct {
    data : Bytes;
  }

  type IPv4Packet = struct {
    header : IPv4.Header;
    data   : match header.protocol {
      1 => ICMPMessage;
      _ => Bytes;
    }
  }
}