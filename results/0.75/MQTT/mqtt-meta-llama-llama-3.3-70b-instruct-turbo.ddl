format mqtt {
  message {
    fixed_header: fixed_header,
    variable_header: variable_header,
    payload: payload
  }
}

format fixed_header {
  message_type: uint(4),
  flags: uint(8)
}

format variable_header {
  protocol_name: string,
  protocol_version: uint(8),
  connect_flags: connect_flags,
  keep_alive: uint(16),
  packet_identifier: uint(16),
  reason_code: reason_code,
  properties: properties
}

format payload {
  will_topic: string,
  will_message: bytes,
  username: string,
  password: bytes,
  topic_name: string,
  topic_alias: uint(16),
  subscription_identifier: uint(16),
  user_properties: user_properties,
  data: bytes
}

format properties {
  authentication_method: string,
  authentication_data: bytes,
  will_delay_interval: uint(32),
  will_payload: bytes,
  retain_available: uint(8),
  user_property: user_property,
  maximum_packet_size: uint(32),
  receive_maximum: uint(16),
  topic_alias_maximum: uint(16),
  request_problem_information: uint(8),
  will_retain: uint(8),
  session_expiry_interval: uint(32),
  assigned_client_identifier: string,
  server_keep_alive: uint(16),
  response_information: string,
  server_reference: string,
  reason_string: string
}

format user_properties {
  user_property: user_property*
}

format user_property {
  key: string,
  value: string
}

format connect_flags {
  clean_start: uint(1),
  will_flag: uint(1),
  will_qos: uint(2),
  will_retain: uint(1),
  password_flag: uint(1),
  username_flag: uint(1)
}

enum reason_code {
  normal_disconnection = 0,
  connection_rate_exceeded = 8,
  unspecified_error = 128,
  malformed_packet = 129,
  protocol_error = 130,
  implementation_specific_error = 131,
  unsupported_protocol_version = 132,
  client_identifier_not_valid = 133,
  bad_username_or_password = 134,
  not_authorized = 135,
  server_unavailable = 136,
  server_busy = 137,
  banned = 138,
  server_shutting_down = 139,
  bad_authentication_method = 140,
  keep_alive_timeout = 141,
  session_taken_over = 142,
  topic_filter_invalid = 143,
  topic_name_invalid = 144,
  receive_maximum_exceeded = 145,
  data_rate_exceeded = 146,
  administrative_action = 147,
  payload_format_invalid = 148,
  retain_not_supported = 149,
  qos_not_supported = 150,
  use_another_server = 151,
  server_moved = 152,
  shared_subscription_not_supported = 153,
  connection_rate_exceeded = 154,
  maximum_connect_time = 155,
  subscription_identifiers_not_supported = 156,
  wildcard_subscriptions_not_supported = 157
}

enum message_type {
  reserved = 0,
  connect = 1,
  connack = 2,
  publish = 3,
  puback = 4,
  pubrec = 5,
  pubrel = 6,
  pubcomp = 7,
  subscribe = 8,
  suback = 9,
  unsubscribe = 10,
  unsuback = 11,
  pingreq = 12,
  pingresp = 13,
  disconnect = 14,
  auth = 15
}