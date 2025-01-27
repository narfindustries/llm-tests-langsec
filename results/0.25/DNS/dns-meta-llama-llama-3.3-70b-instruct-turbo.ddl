domain dns {
  include "integer.ddl";
  include "string.ddl";

  type Header = struct {
    id: uint16,
    flags: uint16,
    qdcount: uint16,
    ancount: uint16,
    nscount: uint16,
    arcount: uint16,
  };

  type Question = struct {
    domain: Domain,
    type: uint16,
    class: uint16,
  };

  type Domain = seqof label: String, separator: uint8(0);

  type Answer = struct {
    domain: uint16, // reference to Question.domain
    type: uint16,
    class: uint16,
    ttl: uint32,
    rdlength: uint16,
    rdata: bytes,
  };

  protocol DNS = seqof header: Header, questions: Question+, answers: Answer*;
}