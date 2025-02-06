PacketType ::=
  | 0x1 : "CONNECT"
  | 0x2 : "CONNACK"
  | 0x3 : "PUBLISH"
  | 0x4 : "PUBACK"
  | 0x5 : "PUBREC"
  | 0x6 : "PUBREL"
  | 0x7 : "PUBCOMP"
  | 0x8 : "SUBSCRIBE"
  | 0x9 : "SUBACK"
  | 0xA : "UNSUBSCRIBE"
  | 0xB : "UNSUBACK"
  | 0xC : "PINGREQ"
  | 0xD : "PINGRESP"
  | 0xE : "DISCONNECT"
  | 0xF : "AUTH";

Flags ::=
  | 0x0 : "Reserved"
  | 0x1 : "DUP"
  | 0x2 : "QoS1"
  | 0x3 : "QoS2"
  | 0x4 : "RETAIN";

PacketIdentifier ::= UInt16;

ProtocolName ::= "MQTT";

ProtocolLevel ::= 0x5;

ConnectFlags ::= {
  UsernameFlag : Bit,
  PasswordFlag : Bit,
  WillRetain : Bit,
  WillQoS : Bits 2,
  WillFlag : Bit,
  CleanStart : Bit,
  Reserved : Bit
};

KeepAlive ::= UInt16;

ReasonCode ::=
  | 0x00 : "Success"
  | 0x80 : "UnspecifiedError"
  | 0x81 : "MalformedPacket"
  | 0x82 : "ProtocolError"
  | 0x83 : "ImplementationSpecificError"
  | 0x84 : "UnsupportedProtocolVersion"
  | 0x85 : "ClientIdentifierNotValid"
  | 0x86 : "BadUserNameOrPassword"
  | 0x87 : "NotAuthorized"
  | 0x88 : "ServerUnavailable"
  | 0x89 : "ServerBusy"
  | 0x8A : "Banned"
  | 0x8B : "BadAuthenticationMethod"
  | 0x8C : "TopicNameInvalid"
  | 0x8D : "PacketTooLarge"
  | 0x8E : "QuotaExceeded"
  | 0x8F : "PayloadFormatInvalid"
  | 0x90 : "RetainNotSupported"
  | 0x91 : "QoSNotSupported"
  | 0x92 : "UseAnotherServer"
  | 0x93 : "ServerMoved"
  | 0x94 : "ConnectionRateExceeded";

Properties ::= [Property];

Property ::= {
  PropertyIdentifier : UInt8,
  PropertyValue :
    | 0x11 : UInt32 -- SessionExpiryInterval
    | 0x21 : UInt16 -- ReceiveMaximum
    | 0x22 : UInt32 -- MaximumPacketSize
    | 0x27 : UInt16 -- TopicAliasMaximum
    | 0x12 : UTF8String -- RequestResponseInformation
    | 0x13 : UTF8String -- RequestProblemInformation
    | 0x26 : UTF8String -- UserProperty
    | 0x15 : UInt8 -- AuthenticationMethod
    | 0x16 : BinaryData -- AuthenticationData
    | 0x17 : UTF8String -- WillDelayInterval
    | 0x18 : UInt8 -- PayloadFormatIndicator
    | 0x19 : UTF8String -- MessageExpiryInterval
    | 0x1A : UTF8String -- ContentType
    | 0x1B : UTF8String -- ResponseTopic
    | 0x1C : BinaryData -- CorrelationData
    | 0x1D : UInt8 -- SubscriptionIdentifier
    | 0x23 : UInt32 -- WillDelayInterval
};

ClientIdentifier ::= UTF8String;

WillTopic ::= UTF8String;

WillMessage ::= BinaryData;

Username ::= UTF8String;

Password ::= BinaryData;

TopicName ::= UTF8String;

Message ::= BinaryData;

SubscriptionIdentifiers ::= [UInt32];

UserProperties ::= [UserProperty];

UserProperty ::= {
  Key : UTF8String,
  Value : UTF8String
};

SubscriptionOptions ::= {
  QoS : Bits 2,
  NoLocal : Bit,
  RetainAsPublished : Bit,
  RetainHandling : Bits 2
};

WillProperties ::= [WillProperty];

WillProperty ::= {
  WillPropertyIdentifier : UInt8,
  WillPropertyValue :
    | 0x18 : UInt32 -- WillDelayInterval
    | 0x19 : UInt8 -- PayloadFormatIndicator
    | 0x1A : UTF8String -- MessageExpiryInterval
    | 0x1B : UTF8String -- ContentType
    | 0x1C : UTF8String -- ResponseTopic
    | 0x1D : BinaryData -- CorrelationData
};

AuthenticationMethod ::= UTF8String;

AuthenticationData ::= BinaryData;

MQTT ::= {
  FixedHeader : {
    PacketType : PacketType,
    Flags : Flags
  },
  VariableHeader : {
    PacketIdentifier : PacketIdentifier,
    ProtocolName : ProtocolName,
    ProtocolLevel : ProtocolLevel,
    ConnectFlags : ConnectFlags,
    KeepAlive : KeepAlive,
    ReasonCode : ReasonCode,
    Properties : Properties
  },
  Payload : {
    ClientIdentifier : ClientIdentifier,
    WillTopic : WillTopic,
    WillMessage : WillMessage,
    Username : Username,
    Password : Password,
    TopicName : TopicName,
    Message : Message,
    SubscriptionIdentifiers : SubscriptionIdentifiers,
    UserProperties : UserProperties,
    SubscriptionOptions : SubscriptionOptions,
    WillProperties : WillProperties,
    AuthenticationMethod : AuthenticationMethod,
    AuthenticationData : AuthenticationData
  }
};