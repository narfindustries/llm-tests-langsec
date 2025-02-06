format binary

group MQTTMessage {
  header: FixedHeader
  payload: MQTTMessageType
}

group FixedHeader {
  controlPacketType: uint8 :assert(in(1,15))
  dup: bool :default(false) :bit(0)
  qos: uint8 :bit(1,2) :enum(0=AT_MOST_ONCE, 1=AT_LEAST_ONCE, 2=EXACTLY_ONCE)
  retain: bool :default(false) :bit(3)
  remainingLength: uint32 :varint
}

group MQTTMessageType {
  type: uint8 :assert(in(1,15))

  CONNECT: Connect
  CONNACK: Connack
  PUBLISH: Publish
  PUBACK: Puback
  PUBREC: Pubrec
  PUBREL: Pubrel
  PUBCOMP: Pubcomp
  SUBSCRIBE: Subscribe
  SUBACK: Suback
  UNSUBSCRIBE: Unsubscribe
  UNSUBACK: Unsuback
  PINGREQ: Pingreq
  PINGRESP: Pingresp
  DISCONNECT: Disconnect
}

group Connect {
  protocolName: string(4) :assert("MQTT")
  protocolLevel: uint8 :assert(5)
  connectFlags: uint8 :bitfield(
    username: bool :bit(7)
    password: bool :bit(6)
    willRetain: bool :bit(5)
    willQos: uint8 :bit(4,3) :enum(0=AT_MOST_ONCE, 1=AT_LEAST_ONCE, 2=EXACTLY_ONCE)
    willFlag: bool :bit(2)
    cleanStart: bool :bit(1)
    reserved: bool :bit(0)
  )
  keepAlive: uint16
  properties: Properties :if(connectFlags & 0x01 != 0)
}

group Connack {
  sessionPresent: bool :default(false)
  reasonCode: uint8
  properties: Properties :if(sessionPresent != 0)
}

group Publish {
  topicName: string
  packetIdentifier: uint16 :if(flags & 0x01 != 0)
  properties: Properties :if(flags & 0x02 != 0)
  payload: bytes
}

group Puback {
  packetIdentifier: uint16
  reasonCode: uint8
  properties: Properties
}

group Pubrec {
  packetIdentifier: uint16
  reasonCode: uint8
  properties: Properties
}

group Pubrel {
  packetIdentifier: uint16
  reasonCode: uint8
  properties: Properties
}

group Pubcomp {
  packetIdentifier: uint16
  reasonCode: uint8
  properties: Properties
}

group Subscribe {
  packetIdentifier: uint16
  properties: Properties
  subscriptions: []Subscription
}

group Suback {
  packetIdentifier: uint16
  properties: Properties
  reasonCodes: []ReasonCode
}

group Unsubscribe {
  packetIdentifier: uint16
  properties: Properties
  topicFilters: []string
}

group Unsuback {
  packetIdentifier: uint16
  properties: Properties
  reasonCodes: []ReasonCode
}

group Pingreq {
}

group Pingresp {
}

group Disconnect {
  reasonCode: uint8
  properties: Properties
}

group Properties {
  sessionExpiryInterval: uint32 :if(flags & 0x01 != 0)
  receiveMaximum: uint16 :if(flags & 0x02 != 0)
  maximumPacketSize: uint32 :if(flags & 0x04 != 0)
  topicAliasMaximum: uint16 :if(flags & 0x08 != 0)
  requestResponseInformation: uint8 :if(flags & 0x10 != 0)
  requestProblemInformation: uint8 :if(flags & 0x20 != 0)
  userProperties: []UserProperty
  authenticationMethod: string :if(flags & 0x40 != 0)
  authenticationData: bytes :if(flags & 0x80 != 0)
}

group Subscription {
  topicFilter: string
  qos: uint8
  noLocal: bool :default(false) :if(flags & 0x01 != 0)
  retainAsPublished: bool :default(false) :if(flags & 0x02 != 0)
  retainHandling: uint8 :if(flags & 0x04 != 0)
}

group ReasonCode {
  code: uint8
}

group UserProperty {
  key: string
  value: string
}

endianness: big

default_uint: uint8