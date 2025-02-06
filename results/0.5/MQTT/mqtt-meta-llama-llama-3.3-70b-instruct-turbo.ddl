type FixedHeader = {
  byte0: uint8,
  byte1: uint8,
}

type VariableHeader = {
  packet_identifier: uint16,
  properties: Properties,
}

type Properties = {
  payload_format_indicator: uint8,
  message_expiry_interval: uint32,
  content_type: string,
  correlation_data: bytes,
  subscription_identifier: uint32,
  session_expiry_interval: uint32,
  assigned_client_identifier: string,
  server_keep_alive: uint16,
  authentication_method: string,
  authentication_data: bytes,
  request_problem_information: uint8,
  request_response_information: uint8,
  response_topic: string,
  user_property: string,
}

type ConnectPacket = {
  fixed_header: FixedHeader,
  protocol_name: string,
  protocol_level: uint8,
  connect_flags: uint8,
  keep_alive: uint16,
  properties: Properties,
  username: string,
  password: bytes,
}

type ConnAckPacket = {
  fixed_header: FixedHeader,
  session_present: uint8,
  return_code: uint8,
  properties: Properties,
}

type PublishPacket = {
  fixed_header: FixedHeader,
  topic_name: string,
  packet_identifier: uint16,
  properties: Properties,
  payload: bytes,
}

type PubAckPacket = {
  fixed_header: FixedHeader,
  packet_identifier: uint16,
  reason_code: uint8,
  properties: Properties,
}

type PubRecPacket = {
  fixed_header: FixedHeader,
  packet_identifier: uint16,
  reason_code: uint8,
  properties: Properties,
}

type PubRelPacket = {
  fixed_header: FixedHeader,
  packet_identifier: uint16,
  reason_code: uint8,
  properties: Properties,
}

type PubCompPacket = {
  fixed_header: FixedHeader,
  packet_identifier: uint16,
  reason_code: uint8,
  properties: Properties,
}

type SubscribePacket = {
  fixed_header: FixedHeader,
  packet_identifier: uint16,
  properties: Properties,
  subscriptions: [Subscription],
}

type Subscription = {
  topic_filter: string,
  qos: uint8,
  no_local: uint8,
  retain_as_published: uint8,
  retain_handling: uint8,
}

type SubAckPacket = {
  fixed_header: FixedHeader,
  packet_identifier: uint16,
  reason_codes: [uint8],
  properties: Properties,
}

type UnsubscribePacket = {
  fixed_header: FixedHeader,
  packet_identifier: uint16,
  properties: Properties,
  topic_filters: [string],
}

type UnsubAckPacket = {
  fixed_header: FixedHeader,
  packet_identifier: uint16,
  reason_codes: [uint8],
  properties: Properties,
}

type DisconnectPacket = {
  fixed_header: FixedHeader,
  reason_code: uint8,
  properties: Properties,
}

type AuthPacket = {
  fixed_header: FixedHeader,
  reason_code: uint8,
  properties: Properties,
}

type MQTT = {
  fixed_header: FixedHeader,
  packet_type: uint8,
  packet: (
    connect: ConnectPacket,
    connack: ConnAckPacket,
    publish: PublishPacket,
    puback: PubAckPacket,
    pubrec: PubRecPacket,
    pubrel: PubRelPacket,
    pubcomp: PubCompPacket,
    subscribe: SubscribePacket,
    suback: SubAckPacket,
    unsubscribe: UnsubscribePacket,
    unsuback: UnsubAckPacket,
    disconnect: DisconnectPacket,
    auth: AuthPacket,
  ),
}