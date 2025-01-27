module ICMP {
  type ICMPMessage = struct {
    type       : UInt8;
    code       : UInt8;
    checksum   : UInt16;
    restOfHeader : UInt32;
    data       : Bytes &size=(this.len - 8);
  } &size=this.len;

  type EchoReply = ICMPMessage &when(this.type == 0);
  type DestinationUnreachable = ICMPMessage &when(this.type == 3);
  type SourceQuench = ICMPMessage &when(this.type == 4);
  type Redirect = ICMPMessage &when(this.type == 5);
  type Echo = ICMPMessage &when(this.type == 8);
  type TimeExceeded = ICMPMessage &when(this.type == 11);
  type ParameterProblem = ICMPMessage &when(this.type == 12);
  type Timestamp = ICMPMessage &when(this.type == 13);
  type TimestampReply = ICMPMessage &when(this.type == 14);
  type InformationRequest = ICMPMessage &when(this.type == 15);
  type InformationReply = ICMPMessage &when(this.type == 16);
  type AddressMaskRequest = ICMPMessage &when(this.type == 17);
  type AddressMaskReply = ICMPMessage &when(this.type == 18);

  type ICMPDatagram = union {
    echoReply : EchoReply;
    destinationUnreachable : DestinationUnreachable;
    sourceQuench : SourceQuench;
    redirect : Redirect;
    echo : Echo;
    timeExceeded : TimeExceeded;
    parameterProblem : ParameterProblem;
    timestamp : Timestamp;
    timestampReply : TimestampReply;
    informationRequest : InformationRequest;
    informationReply : InformationReply;
    addressMaskRequest : AddressMaskRequest;
    addressMaskReply : AddressMaskReply;
  };
}