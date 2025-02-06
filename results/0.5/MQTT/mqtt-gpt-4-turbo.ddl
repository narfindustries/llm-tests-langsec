type Byte = UInt8;
type TwoByteInteger = UInt16;
type FourByteInteger = UInt32;

type VariableByteInteger = {
  value : UInt32;
} using {
  local multiplier = 1;
  local value = 0;
  local encodedByte = 0;
  do {
    encodedByte = UInt8;
    value += (encodedByte & 127) * multiplier;
    multiplier *= 128;
  } while (encodedByte & 128 != 0);
  return value;
};

type UTF8EncodedString = {
  length : TwoByteInteger;
  value : String;
} using {
  length = length(value);
};

type BinaryData = {
  length : TwoByteInteger;
  value : [Byte];
} using {
  length = length(value);
};

type MQTTControlPacket = {
  packetType : Byte;
  flags : Byte;
  remainingLength : VariableByteInteger;
  payload : MQTTControlPayload;
} using {
  packetType = packetType(payload);
  flags = flags(payload);
};

type MQTTControlPayload = CONNECT | CONNACK | PUBLISH | PUBACK | PUBREC | PUBREL | PUBCOMP | SUBSCRIBE | SUBACK | UNSUBSCRIBE | UNSUBACK | PINGREQ | PINGRESP | DISCONNECT | AUTH;

type CONNECT = {
  protocolName : UTF8EncodedString;
  protocolLevel : Byte;
  connectFlags : Byte;
  keepAlive : TwoByteInteger;
  properties : CONNECTProperties;
  clientIdentifier : UTF8EncodedString;
  willProperties : WillProperties;
  willTopic : UTF8EncodedString;
  willPayload : BinaryData;
  username : UTF8EncodedString;
  password : BinaryData;
};

type CONNACK = {
  acknowledgeFlags : Byte;
  reasonCode : Byte;
  properties : CONNACKProperties;
};

type PUBLISH = {
  topicName : UTF8EncodedString;
  packetIdentifier : TwoByteInteger;
  properties : PUBLISHProperties;
  payload : BinaryData;
};

type PUBACK = {
  packetIdentifier : TwoByteInteger;
  reasonCode : Byte;
  properties : PUBACKProperties;
};

type PUBREC = {
  packetIdentifier : TwoByteInteger;
  reasonCode : Byte;
  properties : PUBRECProperties;
};

type PUBREL = {
  packetIdentifier : TwoByteInteger;
  reasonCode : Byte;
  properties : PUBRELProperties;
};

type PUBCOMP = {
  packetIdentifier : TwoByteInteger;
  reasonCode : Byte;
  properties : PUBCOMPProperties;
};

type SUBSCRIBE = {
  packetIdentifier : TwoByteInteger;
  properties : SUBSCRIBEProperties;
  subscriptions : [Subscription];
};

type SUBACK = {
  packetIdentifier : TwoByteInteger;
  properties : SUBACKProperties;
  returnCodes : [Byte];
};

type UNSUBSCRIBE = {
  packetIdentifier : TwoByteInteger;
  properties : UNSUBSCRIBEProperties;
  topicFilters : [UTF8EncodedString];
};

type UNSUBACK = {
  packetIdentifier : TwoByteInteger;
  properties : UNSUBACKProperties;
  reasonCodes : [Byte];
};

type PINGREQ = {};
type PINGRESP = {};

type DISCONNECT = {
  reasonCode : Byte;
  properties : DISCONNECTProperties;
};

type AUTH = {
  reasonCode : Byte;
  properties : AUTHProperties;
};

type CONNECTProperties = {
  sessionExpiryInterval : FourByteInteger;
  receiveMaximum : TwoByteInteger;
  maximumPacketSize : FourByteInteger;
  topicAliasMaximum : TwoByteInteger;
  requestResponseInformation : Byte;
  requestProblemInformation : Byte;
  userProperties : [UserProperty];
  authenticationMethod : UTF8EncodedString;
  authenticationData : BinaryData;
};

type WillProperties = {
  willDelayInterval : FourByteInteger;
  payloadFormatIndicator : Byte;
  messageExpiryInterval : FourByteInteger;
  contentType : UTF8EncodedString;
  responseTopic : UTF8EncodedString;
  correlationData : BinaryData;
  userProperties : [UserProperty];
};

type UserProperty = {
  name : UTF8EncodedString;
  value : UTF8EncodedString;
};

type Subscription = {
  topicFilter : UTF8EncodedString;
  options : Byte;
};