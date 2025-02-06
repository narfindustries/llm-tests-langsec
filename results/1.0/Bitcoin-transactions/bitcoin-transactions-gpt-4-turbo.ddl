bitcoindata = struct {
  version : uint32;
  vin_cnt : varint;
  inputs  : array[input] @length(vin_cnt);
  vout_cnt: varint;
  outputs : array[output] @length(vout_cnt);
  locktime: uint32;
}

varint = union {
  switch {
    | $peek(1) < 0xfd => raw:uint8;
    | $peek(1) == 0xfd => skip(1) ++ raw:uint16;
    | $peek(1) == 0xfe => skip(1) ++ raw:uint32;
    | $peek(1) == 0xff => skip(1) ++ raw:uint64;
  }
}

input = struct {
  txid      : bytes[32];
  vout      : uint32;
  scriptLen : varint;
  scriptSig : bytes[scriptLen];
  sequence  : uint32;
}

output = struct {
  value         : uint64;
  pubkeyScriptLen : varint;
  pubkeyScript : bytes[pubkeyScriptLen];
}

transaction = struct {
  base       : bitcoindata;
  witnesses? : array[witness] @length((base.version & 0x80000000) != 0 ? base.vin_cnt : 0);
  marker     : uint8 @optional @if((base.version & 0x80000000) != 0);
  flag       : uint8 @optional @if(marker == 0);
}

witness = struct {
  count       : varint;
  witnessData : array[bytes] @length(count);
}