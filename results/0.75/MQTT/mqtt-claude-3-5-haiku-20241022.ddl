ddl MQTT {
  type PacketType = enum {
    CONNECT = 1,
    CONNACK = 2,
    PUBLISH = 3,
    PUBACK = 4,
    PUBREC = 5,
    PUBREL = 6,
    PUBCOMP = 7,
    SUBSCRIBE = 8,
    SUBACK = 9,
    UNSUBSCRIBE = 10,
    UNSUBACK = 11,
    PINGREQ = 12,
    PINGRESP = 13,
    DISCONNECT = 14
  }

  type VariableLengthInteger = {
    continuation_bit: bool,
    value: uint(7)
  }

  type FixedHeader = {
    packet_type: PacketType,
    flags: uint(4)
  }

  type ConnectFlags = {
    username_flag: bool,
    password_flag: bool,
    will_retain: bool,
    will_qos: uint(2),
    will_flag: bool,
    clean_start: bool
  }

  type ConnectProperties = {
    session_expiry_interval: option(uint(32)),
    receive_maximum: option(uint(16)),
    maximum_packet_size: option(uint(32)),
    topic_alias_maximum: option(uint(16)),
    request_response_information: option(bool),
    request_problem_information: option(bool),
    user_properties: list((string, string)),
    authentication_method: option(string),
    authentication_data: option(bytes)
  }

  type WillProperties = {
    payload_format_indicator: option(bool),
    message_expiry_interval: option(uint(32)),
    content_type: option(string),
    response_topic: option(string),
    correlation_data: option(bytes),
    user_properties: list((string, string))
  }

  type Packet = 
    | Connect of {
        fixed_header: FixedHeader,
        remaining_length: VariableLengthInteger,
        protocol_name: string,
        protocol_version: uint(8),
        connect_flags: ConnectFlags,
        keep_alive: uint(16),
        properties: ConnectProperties,
        client_id: string,
        will_properties: option(WillProperties),
        will_topic: option(string),
        will_payload: option(bytes),
        username: option(string),
        password: option(string)
      }
    | Connack of {
        fixed_header: FixedHeader,
        remaining_length: VariableLengthInteger,
        session_present: bool,
        reason_code: uint(8),
        properties: {
          session_expiry_interval: option(uint(32)),
          assigned_client_identifier: option(string),
          server_keep_alive: option(uint(16)),
          authentication_method: option(string),
          authentication_data: option(bytes),
          response_information: option(string),
          server_reference: option(string),
          reason_string: option(string),
          user_properties: list((string, string))
        }
      }
    | Publish of {
        fixed_header: FixedHeader,
        remaining_length: VariableLengthInteger,
        topic_name: string,
        packet_identifier: option(uint(16)),
        properties: {
          payload_format_indicator: option(bool),
          message_expiry_interval: option(uint(32)),
          topic_alias: option(uint(16)),
          response_topic: option(string),
          correlation_data: option(bytes),
          user_properties: list((string, string)),
          subscription_identifier: option(uint(32)),
          content_type: option(string)
        },
        payload: bytes
      }
    | Puback of {
        fixed_header: FixedHeader,
        remaining_length: VariableLengthInteger,
        packet_identifier: uint(16),
        reason_code: uint(8),
        properties: {
          reason_string: option(string),
          user_properties: list((string, string))
        }
      }
    | Pubrec of {
        fixed_header: FixedHeader,
        remaining_length: VariableLengthInteger,
        packet_identifier: uint(16),
        reason_code: uint(8),
        properties: {
          reason_string: option(string),
          user_properties: list((string, string))
        }
      }
    | Pubrel of {
        fixed_header: FixedHeader,
        remaining_length: VariableLengthInteger,
        packet_identifier: uint(16),
        reason_code: uint(8),
        properties: {
          reason_string: option(string),
          user_properties: list((string, string))
        }
      }
    | Pubcomp of {
        fixed_header: FixedHeader,
        remaining_length: VariableLengthInteger,
        packet_identifier: uint(16),
        reason_code: uint(8),
        properties: {
          reason_string: option(string),
          user_properties: list((string, string))
        }
      }
    | Subscribe of {
        fixed_header: FixedHeader,
        remaining_length: VariableLengthInteger,
        packet_identifier: uint(16),
        properties: {
          subscription_identifier: option(uint(32)),
          user_properties: list((string, string))
        },
        subscriptions: list({
          topic_filter: string,
          subscription_options: {
            maximum_qos: uint(2),
            no_local: bool,
            retain_as_published: bool,
            retain_handling: uint(2)
          }
        })
      }
    | Suback of {
        fixed_header: FixedHeader,
        remaining_length: VariableLengthInteger,
        packet_identifier: uint(16),
        properties: {
          reason_string: option(string),
          user_properties: list((string, string))
        },
        reason_codes: list(uint(8))
      }
    | Unsubscribe of {
        fixed_header: FixedHeader,
        remaining_length: VariableLengthInteger,
        packet_identifier: uint(16),
        properties: {
          user_properties: list((string, string))
        },
        topic_filters: list(string)
      }
    | Unsuback of {
        fixed_header: FixedHeader,
        remaining_length: VariableLengthInteger,
        packet_identifier: uint(16),
        properties: {
          reason_string: option(string),
          user_properties: list((string, string))
        },
        reason_codes: list(uint(8))
      }
    | Pingreq
    | Pingresp
    | Disconnect of {
        fixed_header: FixedHeader,
        remaining_length: VariableLengthInteger,
        reason_code: uint(8),
        properties: {
          session_expiry_interval: option(uint(32)),
          reason_string: option(string),
          server_reference: option(string),
          user_properties: list((string, string))
        }
      }
}