-- MQTT Protocol Specification in Daedalus
-- Improved and fixed version based on the error message

import Daedalus

def MQTTMessage =
  block
    header: MQTTHeader
    payload: MQTTMessagePayload

def MQTTHeader =
  block
    messageType: uint 4
    dupFlag: uint 1
    qosLevel: uint 2
    retainFlag: uint 1
    remainingLength: MQTTVarInt

def MQTTVarInt =
  block
    byte1: uint 8
    byte2: uint 8
    byte3: uint 8
    byte4: uint 8

def MQTTMessagePayload =
  block
    topicName: MQTTString
    messageId: uint 16
    payload: MQTTData

def MQTTString =
  block
    length: uint 16
    value: bytes length

def MQTTData =
  block
    length: uint 16
    data: bytes length

-- Entry point for parsing MQTT messages
def Main = MQTTMessage