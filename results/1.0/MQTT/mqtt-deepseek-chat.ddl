-- MQTT Protocol Specification in Daedalus
-- This specification defines the structure of MQTT packets.

import Daedalus

-- MQTT Fixed Header
def MQTTFixedHeader =
  block
    let packetType = UInt8
    let flags = UInt8
    let remainingLength = MQTTVarByteInt

-- MQTT Variable Byte Integer
def MQTTVarByteInt =
  block
    let byte = UInt8
    case byte of
      0x80 .. 0xFF -> MQTTVarByteInt
      _ -> Accept

-- MQTT Connect Packet
def MQTTConnectPacket =
  block
    let fixedHeader = MQTTFixedHeader
    let protocolNameLength = UInt16BE
    let protocolName = Bytes protocolNameLength
    let protocolLevel = UInt8
    let connectFlags = UInt8
    let keepAlive = UInt16BE
    let clientIdLength = UInt16BE
    let clientId = Bytes clientIdLength
    let willTopicLength = UInt16BE
    let willTopic = Bytes willTopicLength
    let willMessageLength = UInt16BE
    let willMessage = Bytes willMessageLength
    let usernameLength = UInt16BE
    let username = Bytes usernameLength
    let passwordLength = UInt16BE
    let password = Bytes passwordLength

-- MQTT Publish Packet
def MQTTPublishPacket =
  block
    let fixedHeader = MQTTFixedHeader
    let topicLength = UInt16BE
    let topic = Bytes topicLength
    let packetIdentifier = UInt16BE
    let payload = Bytes remainingLength

-- MQTT Subscribe Packet
def MQTTSubscribePacket =
  block
    let fixedHeader = MQTTFixedHeader
    let packetIdentifier = UInt16BE
    let topicFilters = Many MQTTTopicFilter

-- MQTT Topic Filter
def MQTTTopicFilter =
  block
    let topicLength = UInt16BE
    let topic = Bytes topicLength
    let qos = UInt8

-- MQTT Unsubscribe Packet
def MQTTUnsubscribePacket =
  block
    let fixedHeader = MQTTFixedHeader
    let packetIdentifier = UInt16BE
    let topicFilters = Many MQTTTopicFilter

-- MQTT Disconnect Packet
def MQTTDisconnectPacket =
  block
    let fixedHeader = MQTTFixedHeader

-- MQTT Packet
def MQTTPacket =
  block
    let fixedHeader = MQTTFixedHeader
    case fixedHeader.packetType of
      0x10 -> MQTTConnectPacket
      0x30 -> MQTTPublishPacket
      0x80 -> MQTTSubscribePacket
      0xA0 -> MQTTUnsubscribePacket
      0xE0 -> MQTTDisconnectPacket
      _ -> Fail "Unknown MQTT Packet Type"