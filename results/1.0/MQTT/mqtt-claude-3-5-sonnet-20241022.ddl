def Main = MQTT_Packet

def MQTT_Packet = {
    fixed_header    = Fixed_Header;
    remaining_len   = RemainingLength;
    variable_header = @Take(remaining_len);
    $$ Success
}

def Fixed_Header = {
    packet_type = $Uint8;
    flags      = @Take(1);
    true
}

def RemainingLength = {
    value = 0;
    multiplier = 1;
    do {
        encodedByte = $Uint8;
        value       = value + ((encodedByte & 127) * multiplier);
        multiplier  = multiplier * 128;
        !((encodedByte & 128) != 0)
    };
    value
}