module MQTT {
  type Byte = U8;

  type FixedHeader = struct {
    packetTypeFlags : U8;
    remainingLength : RemainingLength;
  }

  type RemainingLength = struct {
    bytes : [U8];
    value : U32 = remainingLengthValue(bytes);
  }

  type ConnectFlags = struct {
    reserved : U1 = 0;
    cleanSession : U1;
    willFlag : U1;
    willQoS : U2;
    willRetain : U1;
    passwordFlag : U1;
    usernameFlag : U1;
  }

  type ConnectPayload = struct {
    clientId : UTF8String;
    willTopic : UTF8String if this.parent.connectFlags.willFlag == 1;
    willMessage : UTF8String if this.parent.connectFlags.willFlag == 1;
    username : UTF8String if this.parent.connectFlags.usernameFlag == 1;
    password : UTF8String if this.parent.connectFlags.passwordFlag == 1;
  }

  type ConnectVariableHeader = struct {
    protocolName : UTF8String;
    protocolLevel : U8;
    connectFlags : ConnectFlags;
    keepAlive : U16;
  }

  type ConnectPacket = struct {
    fixedHeader : FixedHeader;
    variableHeader : ConnectVariableHeader;
    payload : ConnectPayload;
  }

  type PublishPacket = struct {
    fixedHeader : FixedHeader;
    topicName : UTF8String;
    packetIdentifier : U16 if this.fixedHeader.packetTypeFlags & 0x06 != 0;
    payload : [Byte];
  }

  type SubscribePayload = struct {
    topicFilter : UTF8String;
    qos : U8;
  }

  type SubscribePacket = struct {
    fixedHeader : FixedHeader;
    packetIdentifier : U16;
    payload : [SubscribePayload];
  }

  type UTF8String = struct {
    length : U16;
    string : [Byte] : length;
  }

  function remainingLengthValue(bytes : [U8]) : U32 {
    var multiplier : U32 = 1;
    var value : U32 = 0;
    var index : U32 = 0;
    while index < bytes.length {
      value += U32(bytes[index] & 127) * multiplier;
      if (bytes[index] & 128 == 0) {
        break;
      }
      multiplier *= 128;
      index += 1;
    }
    return value;
  }
}