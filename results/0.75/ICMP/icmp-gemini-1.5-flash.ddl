module ICMP = {
  type IcmpHeader = {
    type: UInt8;
    code: UInt8;
    checksum: UInt16;
    rest: Bytes;
  };

  type IcmpMessage = {
    header: IcmpHeader;
    data: Bytes;
  };

  parseUInt8 : Bytes -> (UInt8, Bytes)
  parseUInt8 bs = 
    case bs of
      [] -> (0, [])
      (x::xs) -> (x, xs)

  parseUInt16 : Bytes -> (UInt16, Bytes)
  parseUInt16 bs =
    case bs of
      [] -> (0, [])
      (x::y::xs) -> ((x << 8) + y, xs)
      _ -> (0, [])

  parseIcmpHeader : Bytes -> IcmpHeader
  parseIcmpHeader bs =
    let (type, bs') = parseUInt8 bs in
    let (code, bs'') = parseUInt8 bs' in
    let (checksum, rest) = parseUInt16 bs'' in
    { type, code, checksum, rest }

  parseIcmpMessage : Bytes -> IcmpMessage
  parseIcmpMessage bs =
    let header = parseIcmpHeader bs in
    { header, data = header.rest }
}
