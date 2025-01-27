module DNS {
  type U8 = UInt(8)
  type U16 = UInt(16)
  type U32 = UInt(32)

  type DomainName = {
    labels: List(Label, Delim(0)),
    term: U8
  }

  type Label = {
    len: U8,
    name: Bytes(len)
  }

  type Header = {
    id: U16,
    qr: U1,
    opcode: U4,
    aa: U1,
    tc: U1,
    rd: U1,
    ra: U1,
    z: U3,
    rcode: U4,
    qdcount: U16,
    ancount: U16,
    nscount: U16,
    arcount: U16
  }

  type Question = {
    qname: DomainName,
    qtype: U16,
    qclass: U16
  }

  type ResourceRecord = {
    name: DomainName,
    type: U16,
    class: U16,
    ttl: U32,
    rdlength: U16,
    rdata: Bytes(rdlength)
  }

  type Message = {
    header: Header,
    questions: List(Question, header.qdcount),
    answers: List(ResourceRecord, header.ancount),
    authorities: List(ResourceRecord, header.nscount),
    additionals: List(ResourceRecord, header.arcount)
  }

  entrypoint parse_dns: Message
}