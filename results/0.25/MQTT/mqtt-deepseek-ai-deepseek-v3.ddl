mqtt5 = {
  FixedHeader: {
    PacketType: UInt8,
    Flags: {
      Retain: Bit,
      QoS: Bits(2),
      Dup: Bit
    }
  },
  VariableHeader: {
    PacketType: UInt8,
    CONNECT: {
      ProtocolName: String,
      ProtocolLevel: UInt8,
      ConnectFlags: {
        UsernameFlag: Bit,
        PasswordFlag: Bit,
        WillRetain: Bit,
        WillQoS: Bits(2),
        WillFlag: Bit,
        CleanStart: Bit
      },
      KeepAlive: UInt16
    },
    CONNACK: {
      SessionPresent: Bit,
      ReasonCode: UInt8
    },
    PUBLISH: {
      TopicName: String,
      PacketIdentifier: UInt16
    },
    PUBACK: {
      PacketIdentifier: UInt16,
      ReasonCode: UInt8
    },
    PUBREC: {
      PacketIdentifier: UInt16,
      ReasonCode: UInt8
    },
    PUBREL: {
      PacketIdentifier: UInt16,
      ReasonCode: UInt8
    },
    PUBCOMP: {
      PacketIdentifier: UInt16,
      ReasonCode: UInt8
    },
    SUBSCRIBE: {
      PacketIdentifier: UInt16,
      SubscriptionOptions: {
        QoS: Bits(2),
        NoLocal: Bit,
        RetainAsPublished: Bit,
        RetainHandling: Bits(2)
      }
    },
    SUBACK: {
      PacketIdentifier: UInt16,
      ReasonCodes: Array(UInt8)
    },
    UNSUBSCRIBE: {
      PacketIdentifier: UInt16
    },
    UNSUBACK: {
      PacketIdentifier: UInt16,
      ReasonCodes: Array(UInt8)
    },
    DISCONNECT: {
      ReasonCode: UInt8
    },
    AUTH: {
      ReasonCode: UInt8
    }
  },
  Payload: {
    CONNECT: {
      ClientIdentifier: String,
      WillTopic: Optional(String),
      WillMessage: Optional(Bytes),
      Username: Optional(String),
      Password: Optional(Bytes)
    },
    PUBLISH: {
      Payload: Bytes
    },
    SUBSCRIBE: {
      TopicFilters: Array(String)
    },
    UNSUBSCRIBE: {
      TopicFilters: Array(String)
    }
  },
  Properties: {
    PayloadFormatIndicator: Optional(UInt8),
    MessageExpiryInterval: Optional(UInt32),
    ContentType: Optional(String),
    ResponseTopic: Optional(String),
    CorrelationData: Optional(Bytes),
    SubscriptionIdentifier: Optional(VarInt),
    SessionExpiryInterval: Optional(UInt32),
    AssignedClientIdentifier: Optional(String),
    ServerKeepAlive: Optional(UInt16),
    AuthenticationMethod: Optional(String),
    AuthenticationData: Optional(Bytes),
    RequestProblemInformation: Optional(UInt8),
    WillDelayInterval: Optional(UInt32),
    RequestResponseInformation: Optional(UInt8),
    ReceiveMaximum: Optional(UInt16),
    TopicAliasMaximum: Optional(UInt16),
    TopicAlias: Optional(UInt16),
    MaximumQoS: Optional(UInt8),
    RetainAvailable: Optional(UInt8),
    UserProperty: Optional(Array({ Key: String, Value: String })),
    MaximumPacketSize: Optional(UInt32),
    WildcardSubscriptionAvailable: Optional(UInt8),
    SubscriptionIdentifierAvailable: Optional(UInt8),
    SharedSubscriptionAvailable: Optional(UInt8)
  }
}