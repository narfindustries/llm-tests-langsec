def Main = MQTT_Packet

def MQTT_Packet = {
  fixed_header = Fixed_Header;
  remaining_length = Remaining_Length;
  variable_header_payload = @takeBytes(remaining_length)
}

def Fixed_Header = {
  packet_type = $byte;
  Assert (packet_type >= 0 && packet_type <= 15)
}

def Remaining_Length = {
  @local result = 0;
  @local multiplier = 1;
  @local encodedByte;
  @do {
    encodedByte = $byte;
    result = result + ((encodedByte & 127) * multiplier);
    multiplier = multiplier * 128;
    Assert (multiplier <= 128 * 128 * 128);
  } until ((encodedByte & 128) == 0);
  result
}