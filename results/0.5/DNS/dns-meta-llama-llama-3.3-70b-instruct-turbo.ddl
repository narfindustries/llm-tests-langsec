domain dns {
  import binary

  type Message = struct {
    id: uint16,
    flags: uint16,
    qdcount: uint16,
    ancount: uint16,
    nscount: uint16,
    arcount: uint16,
    queries: array[Query, qdcount],
    answers: array[ResourceRecord, ancount],
    authorities: array[ResourceRecord, nscount],
    additionals: array[ResourceRecord, arcount]
  }

  type Query = struct {
    name: DomainName,
    type: uint16,
    class: uint16
  }

  type ResourceRecord = struct {
    name: DomainName,
    type: uint16,
    class: uint16,
    ttl: uint32,
    rdlength: uint16,
    rdata: binary(data: rdlength)
  }

  type DomainName = string(separatedBy: 0x2E, terminatedBy: 0x00)

  syntax message Syntax {
    id: uint16 bigEndian,
    flags: uint16 bigEndian,
    qdcount: uint16 bigEndian,
    ancount: uint16 bigEndian,
    nscount: uint16 bigEndian,
    arcount: uint16 bigEndian
  }

  syntax query Syntax {
    name: DomainName,
    type: uint16 bigEndian,
    class: uint16 bigEndian
  }

  syntax resource_record Syntax {
    name: DomainName,
    type: uint16 bigEndian,
    class: uint16 bigEndian,
    ttl: uint32 bigEndian,
    rdlength: uint16 bigEndian,
    rdata: binary(data: rdlength)
  }

  syntax domain_name Syntax {
    string(separatedBy: 0x2E, terminatedBy: 0x00)
  }
}