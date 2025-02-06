module MQTT;

import util::Bits;
import util::Array;
import util::String;
import util::VInt;

type ConnectFlags = struct {
  reserved : Bits(1) = 0;
  cleanStart : Bits(1);
  willFlag : Bits(1);
  willQoS : Bits(2);
  willRetain : Bits(1);
  passwordFlag : Bits(1);
  usernameFlag : Bits(1);
};

type Property = union {
  case 0x01 : PayloadFormatIndicator : Bits(8);
  case 0x02 : MessageExpiryInterval : Bits(32);
  case 0x03 : ContentType : String;
  case 0x08 : ResponseTopic : String;
  case 0x09 : CorrelationData : Array<Bits(8)>;
  case 0x0B : SubscriptionIdentifier : VInt;
  case 0x11 : SessionExpiryInterval : Bits(32);
  case 0x12 : AssignedClientIdentifier : String;
  case 0x13 : ServerKeepAlive : Bits(16);
  case 0x15 : AuthenticationMethod : String;
  case 0x16 : AuthenticationData : Array<Bits(8)>;
  case 0x17 : RequestProblemInformation : Bits(8);
  case 0x18 : WillDelayInterval : Bits(32);
  case 0x19 : RequestResponseInformation : Bits(8);
  case 0x21 : ReceiveMaximum : Bits(16);
  case 0x22 : TopicAliasMaximum : Bits(16);
  case 0x23 : TopicAlias : Bits(16);
  case 0x24 : MaximumQoS : Bits(8);
  case 0x25 : RetainAvailable : Bits(8);
  case 0x26 : UserProperty : String;
  case 0x27 : MaximumPacketSize : Bits(32);
  case 0x28 : WildcardSubscriptionAvailable : Bits(8);
  case 0x29 : SubscriptionIdentifiersAvailable : Bits(8);
  case 0x2A : SharedSubscriptionAvailable : Bits(8);
  default : UnknownProperty : Array<Bits(8)>;
};

type Properties = struct {
  length : VInt;
  properties : Array<Property> (length);
};

type ConnectPayload = struct {
  clientIdentifier : String;
  willProperties : Properties if ^ConnectFlags.willFlag;
  willTopic : String if ^ConnectFlags.willFlag;
  willPayload : Array<Bits(8)> if ^ConnectFlags.willFlag;
  username : String if ^ConnectFlags.usernameFlag;
  password : Array<Bits(8)> if ^ConnectFlags.passwordFlag;
};

type Connect = struct {
  protocolName : String;
  protocolLevel : Bits(8);
  connectFlags : ConnectFlags;
  keepAlive : Bits(16);
  properties : Properties;
  payload : ConnectPayload;
};

type ConnAckFlags = struct {
  sessionPresent : Bits(1);
  reserved : Bits(7) = 0;
};

type ConnAck = struct {
  ackFlags : ConnAckFlags;
  returnCode : Bits(8);
  properties : Properties;
};

type PublishFlags = struct {
  dup : Bits(1);
  qos : Bits(2);
  retain : Bits(1);
};

type Publish = struct {
  flags : PublishFlags;
  topicName : String;
  packetIdentifier : Bits(16) if ^PublishFlags.qos > 0;
  properties : Properties;
  payload : Array<Bits(8)>;
};

type Ack = struct {
  packetIdentifier : Bits(16);
  reasonCode : Bits(8);
  properties : Properties;
};

type SubscribePayload = struct {
  topicFilter : String;
  options : Bits(8);
};

type Subscribe = struct {
  packetIdentifier : Bits(16);
  properties : Properties;
  payload : Array<SubscribePayload>;
};

type Unsubscribe = struct {
  packetIdentifier : Bits(16);
  properties : Properties;
  topicFilters : Array<String>;
};

type PingReq = struct {
};

type PingResp = struct {
};

type Disconnect = struct {
  reasonCode : Bits(8);
  properties : Properties;
};

type Auth = struct {
  reasonCode : Bits(8);
  properties : Properties;
};

type MQTT = union {
  case 1 : Connect;
  case 2 : ConnAck;
  case 3 : Publish;
  case 4 : Ack; // PUBACK
  case 5 : Ack; // PUBREC
  case 6 : Ack; // PUBREL
  case 7 : Ack; // PUBCOMP
  case 8 : Subscribe;
  case 9 : Ack; // SUBACK
  case 10 : Unsubscribe;
  case 11 : Ack; // UNSUBACK
  case 12 : PingReq;
  case 13 : PingResp;
  case 14 : Disconnect;
  case 15 : Auth;
};