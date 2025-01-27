-- Daedalus specification for MQTT protocol
module MQTT where

import Daedalus

-- MQTT Fixed Header
def MQTTFixedHeader = block
  byte typeAndFlags : uint 8
  uint32 remainingLength : uint 32

-- MQTT Control Packet Types
def MQTTControlPacketType = enum
  CONNECT     = 1
  CONNACK     = 2
  PUBLISH     = 3
  PUBACK      = 4
  PUBREC      = 5
  PUBREL      = 6
  PUBCOMP     = 7
  SUBSCRIBE   = 8
  SUBACK      = 9
  UNSUBSCRIBE = 10
  UNSUBACK    = 11
  PINGREQ     = 12
  PINGRESP    = 13
  DISCONNECT  = 14

-- MQTT Connect Packet
def MQTTConnectPacket = block
  MQTTFixedHeader header
  uint16 protocolNameLength : uint 16
  string protocolName : bytes protocolNameLength
  uint8 protocolLevel : uint 8
  uint8 connectFlags : uint 8
  uint16 keepAlive : uint 16
  uint16 clientIdLength : uint 16
  string clientId : bytes clientIdLength

-- MQTT ConnAck Packet
def MQTTConnAckPacket = block
  MQTTFixedHeader header
  uint8 sessionPresent : uint 8
  uint8 returnCode : uint 8

-- MQTT Publish Packet
def MQTTPublishPacket = block
  MQTTFixedHeader header
  uint16 topicNameLength : uint 16
  string topicName : bytes topicNameLength
  uint16 packetIdentifier : uint 16
  uint32 payloadLength : uint 32
  string payload : bytes payloadLength

-- MQTT Subscribe Packet
def MQTTSubscribePacket = block
  MQTTFixedHeader header
  uint16 packetIdentifier : uint 16
  uint16 topicFilterLength : uint 16
  string topicFilter : bytes topicFilterLength
  uint8 requestedQoS : uint 8

-- MQTT SubAck Packet
def MQTTSubAckPacket = block
  MQTTFixedHeader header
  uint16 packetIdentifier : uint 16
  uint8 returnCode : uint 8

-- MQTT Unsubscribe Packet
def MQTTUnsubscribePacket = block
  MQTTFixedHeader header
  uint16 packetIdentifier : uint 16
  uint16 topicFilterLength : uint 16
  string topicFilter : bytes topicFilterLength

-- MQTT UnsubAck Packet
def MQTTUnsubAckPacket = block
  MQTTFixedHeader header
  uint16 packetIdentifier : uint 16

-- MQTT PingReq Packet
def MQTTPingReqPacket = block
  MQTTFixedHeader header

-- MQTT PingResp Packet
def MQTTPingRespPacket = block
  MQTTFixedHeader header

-- MQTT Disconnect Packet
def MQTTDisconnectPacket = block
  MQTTFixedHeader header

-- MQTT Packet
def MQTTPacket = union
  MQTTConnectPacket connect
  MQTTConnAckPacket connack
  MQTTPublishPacket publish
  MQTTSubscribePacket subscribe
  MQTTSubAckPacket suback
  MQTTUnsubscribePacket unsubscribe
  MQTTUnsubAckPacket unsuback
  MQTTPingReqPacket pingreq
  MQTTPingRespPacket pingresp
  MQTTDisconnectPacket disconnect