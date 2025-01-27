def Main = MQTT_Packet

def MQTT_Packet = {
  fixed_header = Fixed_Header;
  remaining_length = Remaining_Length;
  variable_header_payload = @takeUntil $$ == remaining_length
}

def Fixed_Header = {
  packet_type = $uint8;
  Assert packet_type >= 0 && packet_type <= 15
}

def Remaining_Length = {
  value = 0;
  multiplier = 1;
  do {
    encodedByte = $uint8;
    value = value + ((encodedByte & 127) * multiplier);
    multiplier = multiplier * 128;
    if (multiplier > 128*128*128) {
      fail "malformed remaining length"
    }
  } until ((encodedByte & 128) == 0);
  value
}

def Connect = {
  protocol_name_length = $uint16;
  protocol_name = $takeN protocol_name_length;
  protocol_level = $uint8;
  connect_flags = $uint8;
  keep_alive = $uint16
}

def Publish = {
  topic_length = $uint16;
  topic_name = $takeN topic_length;
  message_id = if packet_type >= 2 then $uint16 else 0;
  payload = $takeAll
}

def Subscribe = {
  message_id = $uint16;
  payload = $takeAll
}

def Unsubscribe = {
  message_id = $uint16;
  payload = $takeAll
}

def PingReq = null

def PingResp = null

def Disconnect = null