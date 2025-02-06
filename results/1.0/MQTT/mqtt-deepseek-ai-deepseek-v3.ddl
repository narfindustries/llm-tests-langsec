MQTT_Packet = {
  fixed_header: FixedHeader,
  variable_header: Switch(fixed_header.packet_type) {
    1: ConnectVarHeader,
    2: ConnAckVarHeader,
    3: PublishVarHeader,
    4: PubAckVarHeader,
    5: PubRecVarHeader,
    6: PubRelVarHeader,
    7: PubCompVarHeader,
    8: SubscribeVarHeader,
    9: SubAckVarHeader,
    10: UnsubscribeVarHeader,
    11: UnsubAckVarHeader,
    12: PingReqVarHeader,
    13: PingRespVarHeader,
    14: DisconnectVarHeader,
    15: AuthVarHeader
  },
  payload: Switch(fixed_header.packet_type) {
    1: ConnectPayload,
    3: PublishPayload,
    8: SubscribePayload,
    10: UnsubscribePayload,
    15: AuthPayload
  }
};

FixedHeader = {
  packet_type: UInt(4),
  flags: UInt(4),
  remaining_length: VarInt
};

ConnectVarHeader = {
  protocol_name: String,
  protocol_level: UInt(8),
  connect_flags: {
    username_flag: Bit,
    password_flag: Bit,
    will_retain: Bit,
    will_qos: UInt(2),
    will_flag: Bit,
    clean_start: Bit,
    reserved: Bit
  },
  keep_alive: UInt(16),
  properties: Properties
};

ConnAckVarHeader = {
  session_present: Bit,
  reason_code: UInt(8),
  properties: Properties
};

PublishVarHeader = {
  topic_name: String,
  packet_identifier: Optional(UInt(16)),
  properties: Properties
};

PubAckVarHeader = {
  packet_identifier: UInt(16),
  reason_code: UInt(8),
  properties: Properties
};

PubRecVarHeader = {
  packet_identifier: UInt(16),
  reason_code: UInt(8),
  properties: Properties
};

PubRelVarHeader = {
  packet_identifier: UInt(16),
  reason_code: UInt(8),
  properties: Properties
};

PubCompVarHeader = {
  packet_identifier: UInt(16),
  reason_code: UInt(8),
  properties: Properties
};

SubscribeVarHeader = {
  packet_identifier: UInt(16),
  properties: Properties
};

SubAckVarHeader = {
  packet_identifier: UInt(16),
  reason_codes: Array(UInt(8)),
  properties: Properties
};

UnsubscribeVarHeader = {
  packet_identifier: UInt(16),
  properties: Properties
};

UnsubAckVarHeader = {
  packet_identifier: UInt(16),
  reason_codes: Array(UInt(8)),
  properties: Properties
};

PingReqVarHeader = {};

PingRespVarHeader = {};

DisconnectVarHeader = {
  reason_code: UInt(8),
  properties: Properties
};

AuthVarHeader = {
  reason_code: UInt(8),
  properties: Properties
};

Properties = {
  payload_format_indicator: Optional(UInt(8)),
  message_expiry_interval: Optional(UInt(32)),
  session_expiry_interval: Optional(UInt(32)),
  receive_maximum: Optional(UInt(16)),
  maximum_packet_size: Optional(UInt(32)),
  topic_alias_maximum: Optional(UInt(16)),
  request_response_information: Optional(UInt(8)),
  request_problem_information: Optional(UInt(8)),
  authentication_method: Optional(String),
  authentication_data: Optional(Bytes)
};

ConnectPayload = {
  client_identifier: String,
  will_topic: Optional(String),
  will_message: Optional(Bytes),
  username: Optional(String),
  password: Optional(Bytes)
};

PublishPayload = Bytes;

SubscribePayload = Array({
  topic_filter: String,
  subscription_options: {
    qos: UInt(2),
    no_local: Bit,
    retain_as_published: Bit,
    retain_handling: UInt(2)
  }
});

UnsubscribePayload = Array(String);

AuthPayload = Optional(Bytes);

VarInt = Switch(UInt(8)) {
  0x80..0xFF: {
    least_significant_bits: UInt(7),
    more_bytes: VarInt
  },
  default: UInt(7)
};