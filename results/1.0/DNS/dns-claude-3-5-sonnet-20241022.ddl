def Main = {
  def DomainName = {
    def Label = {
      size = ${uint8};
      if (size <= 63) ${Take size}
    }
    def Labels = Loop (label = Label, $"." ^^ {}) Until $0
    labels = Labels
  }

  def DNSHeader = {
    id = ${uint16};
    flags = ${uint16};
    qdcount = ${uint16};
    ancount = ${uint16};
    nscount = ${uint16};
    arcount = ${uint16}
  }

  def Question = {
    qname = DomainName;
    qtype = ${uint16};
    qclass = ${uint16}
  }

  def ResourceRecord = {
    name = DomainName;
    type = ${uint16};
    class = ${uint16};
    ttl = ${uint32};
    rdlength = ${uint16};
    rdata = ${Take rdlength}
  }

  header = DNSHeader;
  questions = Many header.qdcount Question;
  answers = Many header.ancount ResourceRecord;
  authorities = Many header.nscount ResourceRecord;
  additionals = Many header.arcount ResourceRecord
}