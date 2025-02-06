format binary

type FixedHeader = struct {
  MessageType: uint4,
  Dup: bool,
  QoS: uint2,
  RETAIN: bool,
}

type ConnectFlags = struct {
  Username: bool,
  Password: bool,
  WillRetain: bool,
  WillQoS: uint2,
  WillFlag: bool,
  CleanStart: bool,
}

type Connect = struct {
  ProtocolName: string,
  ProtocolVersion: uint8,
  ConnectFlags: ConnectFlags,
  KeepAlive: uint16,
  Properties: optional Properties,
  Username: optional string,
  Password: optional binary,
  WillTopic: optional string,
  WillMessage: optional binary,
}

type ConnAckProperties = struct {
  SessionExpiryInterval: optional uint32,
  ReceiveMaximum: optional uint16,
  MaximumQoS: optional uint2,
  RetainAvailable: optional bool,
  MaximumPacketSize: optional uint32,
  TopicAliasMaximum: optional uint16,
  ReasonString: optional string,
}

type ConnAck = struct {
  SessionPresent: bool,
  ConnectReturnCode: uint8,
  Properties: optional ConnAckProperties,
}

type PublishProperties = struct {
  PayloadFormatIndicator: optional bool,
  MessageExpiryInterval: optional uint32,
  TopicAlias: optional uint16,
  ResponseTopic: optional string,
  CorrelationData: optional binary,
  UserProperty: optional array UserProperty,
  SubscriptionIdentifier: optional array uint32,
  ContentType: optional string,
}

type Publish = struct {
  TopicName: string,
  PacketIdentifier: optional uint16,
  Properties: optional PublishProperties,
  Payload: binary,
}

type PubAckProperties = struct {
  ReasonString: optional string,
  UserProperty: optional array UserProperty,
}

type PubAck = struct {
  PacketIdentifier: uint16,
  ReasonCode: uint8,
  Properties: optional PubAckProperties,
}

type PubRecProperties = struct {
  ReasonString: optional string,
  UserProperty: optional array UserProperty,
}

type PubRec = struct {
  PacketIdentifier: uint16,
  ReasonCode: uint8,
  Properties: optional PubRecProperties,
}

type PubRelProperties = struct {
  ReasonString: optional string,
  UserProperty: optional array UserProperty,
}

type PubRel = struct {
  PacketIdentifier: uint16,
  ReasonCode: uint8,
  Properties: optional PubRelProperties,
}

type PubCompProperties = struct {
  ReasonString: optional string,
  UserProperty: optional array UserProperty,
}

type PubComp = struct {
  PacketIdentifier: uint16,
  ReasonCode: uint8,
  Properties: optional PubCompProperties,
}

type SubscribeProperties = struct {
  SubscriptionIdentifier: optional array uint32,
  UserProperty: optional array UserProperty,
}

type Subscription = struct {
  TopicFilter: string,
  RequestedQoS: uint8,
  NoLocal: optional bool,
  RetainAsPublished: optional bool,
  RetainHandling: optional uint8,
}

type Subscribe = struct {
  PacketIdentifier: uint16,
  Properties: optional SubscribeProperties,
  Subscriptions: array Subscription,
}

type SubAckProperties = struct {
  ReasonString: optional string,
  UserProperty: optional array UserProperty,
}

type SubAck = struct {
  PacketIdentifier: uint16,
  Properties: optional SubAckProperties,
  ReturnCodes: array uint8,
}

type UnsubscribeProperties = struct {
  UserProperty: optional array UserProperty,
}

type Unsubscribe = struct {
  PacketIdentifier: uint16,
  Properties: optional UnsubscribeProperties,
  TopicFilters: array string,
}

type UnsubAckProperties = struct {
  ReasonString: optional string,
  UserProperty: optional array UserProperty,
}

type UnsubAck = struct {
  PacketIdentifier: uint16,
  Properties: optional UnsubAckProperties,
  ReturnCodes: array uint8,
}

type DisconnectProperties = struct {
  SessionExpiryInterval: optional uint32,
  ReasonString: optional string,
  UserProperty: optional array UserProperty,
  ServerReference: optional string,
}

type Disconnect = struct {
  ReasonCode: uint8,
  Properties: optional DisconnectProperties,
}

type PingReq = struct {
}

type PingResp = struct {
}

type UserProperty = struct {
  Name: string,
  Value: string,
}

type Properties = struct {
  PayloadFormatIndicator: optional bool,
  MessageExpiryInterval: optional uint32,
  TopicAlias: optional uint16,
  ResponseTopic: optional string,
  CorrelationData: optional binary,
  UserProperty: optional array UserProperty,
  SubscriptionIdentifier: optional array uint32,
  ContentType: optional string,
  SessionExpiryInterval: optional uint32,
  ReceiveMaximum: optional uint16,
  MaximumQoS: optional uint2,
  RetainAvailable: optional bool,
  MaximumPacketSize: optional uint32,
  TopicAliasMaximum: optional uint16,
  ReasonString: optional string,
  ServerReference: optional string,
}

type MqttMessage = choice {
  FixedHeader: FixedHeader,
  Connect: Connect,
  ConnAck: ConnAck,
  Publish: Publish,
  PubAck: PubAck,
  PubRec: PubRec,
  PubRel: PubRel,
  PubComp: PubComp,
  Subscribe: Subscribe,
  SubAck: SubAck,
  Unsubscribe: Unsubscribe,
  UnsubAck: UnsubAck,
  PingReq: PingReq,
  PingResp: PingResp,
  Disconnect: Disconnect,
}