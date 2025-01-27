def Main = MQTT_Packet

def MQTT_Packet = {
  fixed_header = Fixed_Header;
  remaining_length = Remaining_Length;
  variable_header_payload = @take(remaining_length)
}

def Fixed_Header = {
  packet_type = $uint8;
  @assert packet_type >= 0 && packet_type <= 15;
  flags = $uint8;
  @assert flags >= 0 && flags <= 15
}

def Remaining_Length = {
  multiplier = 1;
  value = 0;
  loop {
    encodedByte = $uint8;
    value = value + ((encodedByte & 127) * multiplier);
    multiplier = multiplier * 128;
    if (encodedByte & 128) == 0 {
      break
    };
    @assert multiplier <= 128*128*128
  };
  value
}