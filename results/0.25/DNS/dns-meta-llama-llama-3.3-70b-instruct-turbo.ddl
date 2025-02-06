format dns {
  header: {
    id: uint16,
    qr: bool,
    opcode: uint4,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    z: uint3,
    rcode: uint4
  },
  question: {
    qname: domain_name,
    qtype: uint16,
    qclass: uint16
  },
  answer: array(answer_section),
  authority: array(authority_section),
  additional: array(additional_section)
}

format domain_name {
  label: array(label_section)
}

format label_section {
  length: uint8,
  data: bytes(length)
}

format answer_section {
  name: domain_name,
  type: uint16,
  class: uint16,
  ttl: uint32,
  rdlength: uint16,
  rdata: bytes(rdlength)
}

format authority_section {
  name: domain_name,
  type: uint16,
  class: uint16,
  ttl: uint32,
  rdlength: uint16,
  rdata: bytes(rdlength)
}

format additional_section {
  name: domain_name,
  type: uint16,
  class: uint16,
  ttl: uint32,
  rdlength: uint16,
  rdata: bytes(rdlength)
}

format bytes(size: uint16) {
  data: array(uint8, size)
}