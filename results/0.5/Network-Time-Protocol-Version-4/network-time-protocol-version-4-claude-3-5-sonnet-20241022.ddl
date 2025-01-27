def Main = NTP_Packet

def NTP_Packet = {
  $li_vn_mode : uint8;
  $stratum : uint8;
  $poll : uint8;
  $precision : int8;
  $root_delay : uint32;
  $root_dispersion : uint32;
  $reference_id : uint32;
  $reference_timestamp : uint64;
  $origin_timestamp : uint64;
  $receive_timestamp : uint64;
  $transmit_timestamp : uint64;
  $optional_authenticator = Optional_Authenticator
}

def Optional_Authenticator = {
  $key_id : uint32;
  $message_digest : Array uint8 16
}?

def NTP_Control_Message = {
  $version : uint8;
  $mode : uint8;
  $response : uint8;
  $error : uint8;
  $more : uint8;
  $op_code : uint8;
  $sequence : uint16;
  $status : uint16;
  $association_id : uint16;
  $offset : uint16;
  $count : uint16;
  $data = if $count > 0 then Array uint8 $count else []
}

def NTP_Mode7_Message = {
  $version : uint8;
  $mode : uint8;
  $response : uint8;
  $error : uint8;
  $more : uint8;
  $sequence : uint8;
  $implementation : uint8;
  $request_code : uint8;
  $err : uint16;
  $count : uint16;
  $mbz : uint16;
  $auth_keyid : uint32;
  $data = if $count > 0 then Array uint8 $count else []
}