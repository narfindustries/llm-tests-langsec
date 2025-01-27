domain dns {
  import byteorder;

  type Message = struct {
    header: Header,
    questions: Question[],
    answers: ResourceRecord[],
    authorities: ResourceRecord[],
    additionals: ResourceRecord[]
  };

  type Header = struct {
    id: uint16,
    flags: Flags,
    qdcount: uint16,
    ancount: uint16,
    nscount: uint16,
    arcount: uint16
  };

  type Flags = bitfield {
    qr: 1,
    opcode: 4,
    aa: 1,
    tc: 1,
    rd: 1,
    ra: 1,
    z: 3,
    rcode: 4
  };

  type Question = struct {
    name: DomainName,
    type: uint16,
    class: uint16
  };

  type DomainName = string {
    encoding: dns_label,
    separator: '.'
  };

  type ResourceRecord = struct {
    name: DomainName,
    type: uint16,
    class: uint16,
    ttl: uint32,
    rdlength: uint16,
    rdata: RData
  };

  type RData = choice {
    A: uint32,
    NS: DomainName,
    CNAME: DomainName,
    SOA: SOA,
    PTR: DomainName,
    MX: MX
  };

  type SOA = struct {
    mname: DomainName,
    rname: DomainName,
    serial: uint32,
    refresh: uint32,
    retry: uint32,
    expire: uint32,
    minimum: uint32
  };

  type MX = struct {
    preference: uint16,
    exchange: DomainName
  };
}