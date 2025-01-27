domain DNS {
  doc "Domain for DNS protocol"

  type Message = struct {
    id: uint16,
    flags: uint16,
    qdcount: uint16,
    ancount: uint16,
    nscount: uint16,
    arcount: uint16,
  }

  type Question = struct {
    name: string,
    type: uint16,
    class: uint16,
  }

  type Answer = struct {
    name: uint16,
    type: uint16,
    class: uint16,
    ttl: uint32,
    rdlength: uint16,
    rdata: string,
  }

  grammar TOP {
    entry = message,
    message = Message,
    question = Question,
    answer = Answer,
  }

  grammar Message {
    id = uint16: big-endian,
    flags = uint16: big-endian,
    qdcount = uint16: big-endian,
    ancount = uint16: big-endian,
    nscount = uint16: big-endian,
    arcount = uint16: big-endian,
    questions = list(qdcount, question),
    answers = list(ancount, answer),
    authorities = list(nscount, answer),
    additionals = list(arcount, answer),
  }

  grammar Question {
    name = string { length = -1, encoding = "ascii" },
    type = uint16: big-endian,
    class = uint16: big-endian,
  }

  grammar Answer {
    name = uint16: big-endian,
    type = uint16: big-endian,
    class = uint16: big-endian,
    ttl = uint32: big-endian,
    rdlength = uint16: big-endian,
    rdata = string { length = rdlength, encoding = "ascii" },
  }
}