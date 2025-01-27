def Main = MQTT_Packet

def MQTT_Packet = {
  header = MQTT_Header;
  payload = MQTT_Payload header;
  FEnd
}

def MQTT_Header = {
  message_type = $byte;
  flags = $byte;
  remaining_length = MQTT_RemainingLength;
  §message_type >= 0 && message_type <= 15
}

def MQTT_RemainingLength = {
  value = 0;
  multiplier = 1;
  repeat {
    digit = $byte;
    value = value + ((digit & 127) * multiplier);
    multiplier = multiplier * 128;
    if (digit & 128) == 0 { break }
  }
  value
}

def MQTT_Payload header = {
  if header.message_type == 1 {
    MQTT_Connect
  } else if header.message_type == 2 {
    MQTT_ConnAck
  } else if header.message_type == 3 {
    MQTT_Publish header
  } else if header.message_type == 4 {
    MQTT_PubAck
  } else {
    MQTT_Other header.remaining_length
  }
}

def MQTT_Connect = {
  protocol_name = MQTT_String;
  protocol_level = $byte;
  connect_flags = $byte;
  keep_alive = $uint16BE;
  client_id = MQTT_String;
  §protocol_level == 4
}

def MQTT_ConnAck = {
  flags = $byte;
  return_code = $byte;
  §return_code >= 0 && return_code <= 5
}

def MQTT_Publish header = {
  topic = MQTT_String;
  packet_id = if (header.flags & 0x06) != 0 { $uint16BE };
  payload = MQTT_Binary(header.remaining_length - @stream - @start)
}

def MQTT_PubAck = {
  packet_id = $uint16BE
}

def MQTT_Other length = {
  data = MQTT_Binary(length)
}

def MQTT_String = {
  length = $uint16BE;
  value = $bytes(length)
}

def MQTT_Binary length = {
  $bytes(length)
}