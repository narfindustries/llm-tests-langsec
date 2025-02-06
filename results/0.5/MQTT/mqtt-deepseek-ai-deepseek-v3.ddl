MQTT = {
  ControlPacket: {
    FixedHeader: {
      PacketType: uint 4,
      Flags: uint 4,
      RemainingLength: varuint
    },
    VariableHeader: {
      PacketIdentifier: uint 16?
    }
  },
  CONNECT: {
    FixedHeader: {
      PacketType: uint 4 = 1,
      Flags: uint 4,
      RemainingLength: varuint
    },
    VariableHeader: {
      ProtocolName: utf8,
      ProtocolLevel: uint 8,
      ConnectFlags: uint 8,
      KeepAlive: uint 16
    },
    Payload: {
      ClientIdentifier: utf8,
      WillTopic: utf8?,
      WillPayload: bytes?,
      Username: utf8?,
      Password: bytes?
    }
  },
  CONNACK: {
    FixedHeader: {
      PacketType: uint 4 = 2,
      Flags: uint 4,
      RemainingLength: varuint
    },
    VariableHeader: {
      SessionPresent: uint 1,
      ReasonCode: uint 8
    },
    Properties: {
      SessionExpiryInterval: uint 32?,
      ReceiveMaximum: uint 16?,
      MaximumQoS: uint 8?,
      RetainAvailable: uint 8?,
      MaximumPacketSize: uint 32?,
      AssignedClientIdentifier: utf8?,
      ServerKeepAlive: uint 16?,
      ResponseInformation: utf8?,
      ServerReference: utf8?,
      AuthenticationMethod: utf8?,
      AuthenticationData: bytes?,
      UserProperties: { utf8: utf8 }*
    }
  },
  PUBLISH: {
    FixedHeader: {
      PacketType: uint 4 = 3,
      Flags: uint 4,
      RemainingLength: varuint
    },
    VariableHeader: {
      TopicName: utf8,
      PacketIdentifier: uint 16?
    },
    Properties: {
      PayloadFormatIndicator: uint 8?,
      MessageExpiryInterval: uint 32?,
      TopicAlias: uint 16?,
      ResponseTopic: utf8?,
      CorrelationData: bytes?,
      UserProperties: { utf8: utf8 }*,
      SubscriptionIdentifier: varuint?,
      ContentType: utf8?
    },
    Payload: bytes?
  },
  SUBSCRIBE: {
    FixedHeader: {
      PacketType: uint 4 = 8,
      Flags: uint 4,
      RemainingLength: varuint
    },
    VariableHeader: {
      PacketIdentifier: uint 16
    },
    Payload: {
      TopicFilters: {
        TopicFilter: utf8,
        SubscriptionOptions: uint 8
      }*
    }
  },
  DISCONNECT: {
    FixedHeader: {
      PacketType: uint 4 = 14,
      Flags: uint 4,
      RemainingLength: varuint
    },
    VariableHeader: {
      ReasonCode: uint 8
    },
    Properties: {
      SessionExpiryInterval: uint 32?,
      ReasonString: utf8?,
      UserProperties: { utf8: utf8 }*,
      ServerReference: utf8?
    }
  },
  AUTH: {
    FixedHeader: {
      PacketType: uint 4 = 15,
      Flags: uint 4,
      RemainingLength: varuint
    },
    VariableHeader: {
      ReasonCode: uint 8
    },
    Properties: {
      AuthenticationMethod: utf8?,
      AuthenticationData: bytes?,
      ReasonString: utf8?,
      UserProperties: { utf8: utf8 }*
    }
  }
}