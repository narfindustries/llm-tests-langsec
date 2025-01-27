domain DNS {
  import "ip.proto";
  import "tcp.proto";

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
  };

  type Query = struct {
    name: DomainName,
    type: uint16,
    class: uint16
  };

  type DomainName = array[label: string, .length = sizeof(name) - 2];

  type ResourceRecord = struct {
    name: DomainName,
    type: uint16,
    class: uint16,
    ttl: uint32,
    rdlength: uint16,
    rdata: bytes[rdlength]
  };

  syntax dns = Message;

  on syntax dns {
    recovery {
      start: reset(ip, tcp) ->_iso;
      reset: drop -> start;
    };

    isolate name {
      label: [label: string, .length = sizeof(label)];
      domain_name: array[label, .length = sizeof(name) - 2];
    };

    recover {
      reset: drop label -> start;
    };
  };
}