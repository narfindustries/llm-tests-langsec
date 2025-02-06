type var_int = uint(32);
type utf8_string = string;
type byte_array = string;

type key_value_pair = {
  key: utf8_string,
  value: utf8_string
};

type fixed_header = {
  packet_type: uint(4),
  flags: uint(4),
  remaining_length: var_int
};

type connect_flags = {
  username_flag: bool,
  password_flag: bool,
  will_retain: bool,
  will_qos: uint(2),
  will_flag: bool,
  clean_start: bool,
  reserved: bool
};

type connect_properties = {
  session_expiry_interval: option(uint(32)),
  receive_maximum: option(uint(16)),
  maximum_packet_size: option(uint(32)),
  topic_alias_maximum: option(uint(16)),
  request_response_info: option(bool),
  request_problem_info: option(bool),
  user_properties: list(key_value_pair),
  authentication_method: option(utf8_string),
  authentication_data: option(byte_array)
};

type will_properties = {
  payload_format_indicator: option(bool),
  message_expiry_interval: option(uint(32)),
  content_type: option(utf8_string),
  response_topic: option(utf8_string),
  correlation_data: option(byte_array),
  user_properties: list(key_value_pair)
};

type connect_packet = {
  fixed_header: fixed_header,
  protocol_name: utf8_string,
  protocol_version: uint(8),
  connect_flags: connect_flags,
  keep_alive: uint(16),
  properties: connect_properties,
  client_id: utf8_string,
  will_properties: option(will_properties),
  will_topic: option(utf8_string),
  will_payload: option(byte_array),
  username: option(utf8_string),
  password: option(utf8_string)
};

type connect_acknowledge_flags = {
  session_present: bool,
  reserved: uint(7)
};

type connack_properties = {
  session_expiry_interval: option(uint(32)),
  receive_maximum: option(uint(16)),
  maximum_qos: option(uint(8)),
  retain_available: option(bool),
  maximum_packet_size: option(uint(32)),
  assigned_client_identifier: option(utf8_string),
  topic_alias_maximum: option(uint(16)),
  reason_string: option(utf8_string),
  user_properties: list(key_value_pair),
  wildcard_subscription_available: option(bool),
  subscription_identifiers_available: option(bool),
  shared_subscription_available: option(bool),
  server_keep_alive: option(uint(16)),
  response_information: option(utf8_string),
  server_reference: option(utf8_string),
  authentication_method: option(utf8_string),
  authentication_data: option(byte_array)
};

type connack_packet = {
  fixed_header: fixed_header,
  connect_acknowledge_flags: connect_acknowledge_flags,
  reason_code: uint(8),
  properties: connack_properties
};

type publish_properties = {
  payload_format_indicator: option(bool),
  message_expiry_interval: option(uint(32)),
  topic_alias: option(uint(16)),
  response_topic: option(utf8_string),
  correlation_data: option(byte_array),
  user_properties: list(key_value_pair),
  content_type: option(utf8_string)
};

type publish_packet = {
  fixed_header: fixed_header,
  topic_name: utf8_string,
  packet_identifier: option(uint(16)),
  properties: publish_properties,
  payload: byte_array
};

type puback_properties = {
  reason_string: option(utf8_string),
  user_properties: list(key_value_pair)
};

type puback_packet = {
  fixed_header: fixed_header,
  packet_identifier: uint(16),
  reason_code: uint(8),
  properties: puback_properties
};

type pubrec_properties = {
  reason_string: option(utf8_string),
  user_properties: list(key_value_pair)
};

type pubrec_packet = {
  fixed_header: fixed_header,
  packet_identifier: uint(16),
  reason_code: uint(8),
  properties: pubrec_properties
};

type pubrel_properties = {
  reason_string: option(utf8_string),
  user_properties: list(key_value_pair)
};

type pubrel_packet = {
  fixed_header: fixed_header,
  packet_identifier: uint(16),
  reason_code: uint(8),
  properties: pubrel_properties
};

type pubcomp_properties = {
  reason_string: option(utf8_string),
  user_properties: list(key_value_pair)
};

type pubcomp_packet = {
  fixed_header: fixed_header,
  packet_identifier: uint(16),
  reason_code: uint(8),
  properties: pubcomp_properties
};

type subscribe_properties = {
  subscription_identifier: option(var_int),
  user_properties: list(key_value_pair)
};

type subscription_options = {
  maximum_qos: uint(2),
  no_local: bool,
  retain_as_published: bool,
  retain_handling: uint(2)
};

type subscription = {
  topic_filter: utf8_string,
  subscription_options: subscription_options
};

type subscribe_packet = {
  fixed_header: fixed_header,
  packet_identifier: uint(16),
  properties: subscribe_properties,
  subscriptions: list(subscription)
};

type suback_properties = {
  reason_string: option(utf8_string),
  user_properties: list(key_value_pair)
};

type suback_packet = {
  fixed_header: fixed_header,
  packet_identifier: uint(16),
  properties: suback_properties,
  reason_codes: list(uint(8))
};

type unsubscribe_properties = {
  user_properties: list(key_value_pair)
};

type unsubscribe_packet = {
  fixed_header: fixed_header,
  packet_identifier: uint(16),
  properties: unsubscribe_properties,
  topic_filters: list(utf8_string)
};

type unsuback_properties = {
  reason_string: option(utf8_string),
  user_properties: list(key_value_pair)
};

type unsuback_packet = {
  fixed_header: fixed_header,
  packet_identifier: uint(16),
  properties: unsuback_properties,
  reason_codes: list(uint(8))
};

type disconnect_properties = {
  session_expiry_interval: option(uint(32)),
  reason_string: option(utf8_string),
  user_properties: list(key_value_pair),
  server_reference: option(utf8_string)
};

type disconnect_packet = {
  fixed_header: fixed_header,
  reason_code: uint(8),
  properties: disconnect_properties
};

type pingreq_packet = {
  fixed_header: fixed_header
};

type pingresp_packet = {
  fixed_header: fixed_header
};

type mqtt_packet =
  | Connect of connect_packet
  | Connack of connack_packet
  | Publish of publish_packet
  | Puback of puback_packet
  | Pubrec of pubrec_packet
  | Pubrel of pubrel_packet
  | Pubcomp of pubcomp_packet
  | Subscribe of subscribe_packet
  | Suback of suback_packet
  | Unsubscribe of unsubscribe_packet
  | Unsuback of unsuback_packet
  | Pingreq of pingreq_packet
  | Pingresp of pingresp_packet
  | Disconnect of disconnect_packet;