packet = {
  fixedHeader: FixedHeader,
  variableHeader: VariableHeader,
  payload: Payload
}

FixedHeader = {
  packetType: PacketType,
  flags: Flags,
  remainingLength: RemainingLength
}

PacketType : uint 4 = {
  1: "CONNECT",
  2: "CONNACK",
  3: "PUBLISH",
  4: "PUBACK",
  5: "PUBREC",
  6: "PUBREL",
  7: "PUBCOMP",
  8: "SUBSCRIBE",
  9: "SUBACK",
  10: "UNSUBSCRIBE",
  11: "UNSUBACK",
  12: "PINGREQ",
  13: "PINGRESP",
  14: "DISCONNECT",
  15: "AUTH"
}

Flags : uint 4 = {
  bit 3: DUP : bool,
  bit 2-1: QoS : uint 2,
  bit 0: RETAIN : bool
}

RemainingLength : varint 1-4

VariableHeader = {
  case packetType of {
    "CONNECT": ConnectVariableHeader,
    "CONNACK": ConnackVariableHeader,
    "PUBLISH": PublishVariableHeader,
    "PUBACK": PubackVariableHeader,
    "PUBREC": PubrecVariableHeader,
    "PUBREL": PubrelVariableHeader,
    "PUBCOMP": PubcompVariableHeader,
    "SUBSCRIBE": SubscribeVariableHeader,
    "SUBACK": SubackVariableHeader,
    "UNSUBSCRIBE": UnsubscribeVariableHeader,
    "UNSUBACK": UnsubackVariableHeader,
    "PINGREQ": {},
    "PINGRESP": {},
    "DISCONNECT": DisconnectVariableHeader,
    "AUTH": AuthVariableHeader
  }
}

ConnectVariableHeader = {
  protocolName: UTF8String,
  protocolLevel: uint 8,
  connectFlags: ConnectFlags,
  keepAlive: uint 16
}

ConnectFlags = {
  bit 7: userNameFlag : bool,
  bit 6: passwordFlag : bool,
  bit 5: willRetain : bool,
  bit 4-3: willQoS : uint 2,
  bit 2: willFlag : bool,
  bit 1: cleanStart : bool,
  bit 0: reserved : uint 1
}

ConnackVariableHeader = {
  sessionPresent: bool,
  reasonCode: ReasonCode
}

ReasonCode : uint 8 = {
  0x00: "Success",
  0x01: "UnspecifiedError",
  0x02: "MalformedPacket",
  0x03: "ProtocolError",
  0x04: "ImplementationSpecificError",
  0x05: "UnsupportedProtocolVersion",
  0x06: "ClientIdentifierNotValid",
  0x07: "BadUserNameOrPassword",
  0x08: "NotAuthorized",
  0x09: "ServerUnavailable",
  0x0A: "ServerBusy",
  0x0B: "Banned",
  0x0C: "BadAuthenticationMethod",
  0x0D: "TopicNameInvalid",
  0x0E: "PacketTooLarge",
  0x0F: "QuotaExceeded",
  0x10: "PayloadFormatInvalid",
  0x11: "RetainNotSupported",
  0x12: "QoSNotSupported",
  0x13: "UseAnotherServer",
  0x14: "ServerMoved",
  0x15: "ConnectionRateExceeded",
  0x16: "MaximumConnectTime",
  0x17: "SubscriptionIdentifiersNotSupported",
  0x18: "WildcardSubscriptionsNotSupported"
}

PublishVariableHeader = {
  topicName: UTF8String,
  packetIdentifier: uint 16?
}

PubackVariableHeader = {
  packetIdentifier: uint 16,
  reasonCode: ReasonCode
}

PubrecVariableHeader = {
  packetIdentifier: uint 16,
  reasonCode: ReasonCode
}

PubrelVariableHeader = {
  packetIdentifier: uint 16,
  reasonCode: ReasonCode
}

PubcompVariableHeader = {
  packetIdentifier: uint 16,
  reasonCode: ReasonCode
}

SubscribeVariableHeader = {
  packetIdentifier: uint 16,
  properties: Properties
}

SubackVariableHeader = {
  packetIdentifier: uint 16,
  reasonCodes: ReasonCode[]
}

UnsubscribeVariableHeader = {
  packetIdentifier: uint 16,
  properties: Properties
}

UnsubackVariableHeader = {
  packetIdentifier: uint 16,
  reasonCodes: ReasonCode[]
}

DisconnectVariableHeader = {
  reasonCode: ReasonCode,
  properties: Properties
}

AuthVariableHeader = {
  reasonCode: ReasonCode,
  properties: Properties
}

Properties = {
  payloadFormatIndicator: uint 8?,
  messageExpiryInterval: uint 32?,
  contentType: UTF8String?,
  responseTopic: UTF8String?,
  correlationData: bytes?,
  userProperties: UserProperty[],
  sessionExpiryInterval: uint 32?,
  assignedClientIdentifier: UTF8String?,
  serverKeepAlive: uint 16?,
  subscriptionIdentifier: uint 32?,
  willDelayInterval: uint 32?,
  requestResponseInformation: uint 8?,
  requestProblemInformation: uint 8?,
  maximumPacketSize: uint 32?,
  wildcardSubscriptionAvailable: uint 8?,
  subscriptionIdentifiersAvailable: uint 8?,
  sharedSubscriptionAvailable: uint 8?,
  serverReference: UTF8String?,
  authenticationMethod: UTF8String?,
  authenticationData: bytes?,
  reasonString: UTF8String?,
  receiveMaximum: uint 16?,
  topicAliasMaximum: uint 16?,
  topicAlias: uint 16?,
  retainAvailable: uint 8?,
  maximumQoS: uint 8?,
  wildcardSubscriptionAvailable: uint 8?,
  subscriptionIdentifiersAvailable: uint 8?,
  sharedSubscriptionAvailable: uint 8?
}

UserProperty = {
  key: UTF8String,
  value: UTF8String
}

Payload = {
  case packetType of {
    "CONNECT": ConnectPayload,
    "PUBLISH": PublishPayload,
    "SUBSCRIBE": SubscribePayload,
    "UNSUBSCRIBE": UnsubscribePayload
  }
}

ConnectPayload = {
  clientIdentifier: UTF8String,
  willTopic: UTF8String?,
  willMessage: bytes?,
  userName: UTF8String?,
  password: bytes?
}

PublishPayload = {
  message: bytes
}

SubscribePayload = {
  topicFilters: TopicFilter[]
}

TopicFilter = {
  topic: UTF8String,
  options: SubscriptionOptions
}

SubscriptionOptions = {
  qos: uint 2,
  noLocal: bool,
  retainAsPublished: bool,
  retainHandling: uint 2
}

UnsubscribePayload = {
  topicFilters: UTF8String[]
}