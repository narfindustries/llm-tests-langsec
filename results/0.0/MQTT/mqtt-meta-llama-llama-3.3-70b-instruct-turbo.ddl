format binary {
  type fixed_header {
    reserved: 4 bits;
    qos: 2 bits;
    dup: 1 bit;
    retain: 1 bit;
    remaining_length: 7-32 bits;
  }

  type variable_header {
    packet_identifier: 2 bytes;
    properties: properties;
  }

  type properties {
    payload_format_indicator: optional 1 byte;
    message_expiry_interval: optional 4 bytes;
    content_type: optional string;
    response_topic: optional string;
    correlation_data: optional bytes;
    subscription_identifier: optional 2 bytes;
    session_expiry_interval: optional 4 bytes;
    assigned_client_identifier: optional string;
    server_keep_alive: optional 2 bytes;
    authentication_method: optional string;
    authentication_data: optional bytes;
    request_problem_information: optional 1 byte;
    request_response_information: optional 1 byte;
    response_information: optional string;
    server_reference: optional string;
    reason_string: optional string;
    receive_maximum: optional 2 bytes;
    topic_alias_maximum: optional 2 bytes;
    topic_alias: optional 2 bytes;
    maximum_qos: optional 1 byte;
    retain_available: optional 1 byte;
    user_property: optional sequence of (string, string);
    maximum_packet_size: optional 4 bytes;
    wildcard_subscription_available: optional 1 byte;
    subscription_identifier_available: optional 1 byte;
    shared_subscription_available: optional 1 byte;
  }

  type connect {
    fixed_header: fixed_header;
    variable_header: {
      protocol_name: string;
      protocol_version: 1 byte;
      connect_flags: {
        clean_start: 1 bit;
        will_flag: 1 bit;
        will_qos: 2 bits;
        will_retain: 1 bit;
        password_flag: 1 bit;
        username_flag: 1 bit;
      };
      keep_alive: 2 bytes;
      properties: properties;
      will_topic: optional string;
      will_payload: optional bytes;
      username: optional string;
      password: optional bytes;
    };
  }

  type connack {
    fixed_header: fixed_header;
    variable_header: {
      session_present: 1 bit;
      connect_return_code: 1 byte;
      properties: properties;
    };
  }

  type publish {
    fixed_header: fixed_header;
    variable_header: {
      topic_name: string;
      packet_identifier: optional 2 bytes;
      properties: properties;
    };
    payload: bytes;
  }

  type puback {
    fixed_header: fixed_header;
    variable_header: {
      packet_identifier: 2 bytes;
      properties: properties;
    };
  }

  type pubrec {
    fixed_header: fixed_header;
    variable_header: {
      packet_identifier: 2 bytes;
      properties: properties;
    };
  }

  type pubrel {
    fixed_header: fixed_header;
    variable_header: {
      packet_identifier: 2 bytes;
      properties: properties;
    };
  }

  type pubcomp {
    fixed_header: fixed_header;
    variable_header: {
      packet_identifier: 2 bytes;
      properties: properties;
    };
  }

  type subscribe {
    fixed_header: fixed_header;
    variable_header: {
      packet_identifier: 2 bytes;
      properties: properties;
    };
    payload: sequence of (string, 1 byte);
  }

  type suback {
    fixed_header: fixed_header;
    variable_header: {
      packet_identifier: 2 bytes;
      properties: properties;
    };
    payload: sequence of 1 byte;
  }

  type unsubscribe {
    fixed_header: fixed_header;
    variable_header: {
      packet_identifier: 2 bytes;
      properties: properties;
    };
    payload: sequence of string;
  }

  type unsuback {
    fixed_header: fixed_header;
    variable_header: {
      packet_identifier: 2 bytes;
      properties: properties;
    };
  }

  type pingreq {
    fixed_header: fixed_header;
  }

  type pingresp {
    fixed_header: fixed_header;
  }

  type disconnect {
    fixed_header: fixed_header;
    variable_header: {
      reason_code: 1 byte;
      properties: properties;
    };
  }

  type message = choice {
    connect,
    connack,
    publish,
    puback,
    pubrec,
    pubrel,
    pubcomp,
    subscribe,
    suback,
    unsubscribe,
    unsuback,
    pingreq,
    pingresp,
    disconnect
  }
}