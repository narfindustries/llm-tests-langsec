def Main = {
  DNS_Message
}

def DNS_Message = {
  header:DNS_Header
  questions:Many(DNS_Question, header.qdcount)
  answers:Many(DNS_Resource_Record, header.ancount)
  authority:Many(DNS_Resource_Record, header.nscount)
  additional:Many(DNS_Resource_Record, header.arcount)
}

def DNS_Header = {
  id:uint16
  qr:uint1
  opcode:uint4
  aa:uint1
  tc:uint1
  rd:uint1
  ra:uint1
  z:uint3
  rcode:uint4
  qdcount:uint16
  ancount:uint16
  nscount:uint16
  arcount:uint16
}

def DNS_Question = {
  qname:DNS_Name
  qtype:uint16
  qclass:uint16
}

def DNS_Resource_Record = {
  name:DNS_Name
  type:uint16
  class:uint16
  ttl:uint32
  rdlength:uint16
  rdata:Bytes(rdlength)
}

def DNS_Name = {
  labels:Many(DNS_Label, until = $$ == 0)
}

def DNS_Label = {
  length:uint8
  if length == 0 then
    return ()
  else if (length & 0xC0) == 0xC0 then {
    offset:uint8
    return ()
  } else {
    label:Bytes(length)
    return ()
  }
}