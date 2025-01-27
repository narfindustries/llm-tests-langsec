def Main = {
  def DNSMessage = {
    header = Header;
    questions = Many (header.qdcount) Question;
    answers = Many (header.ancount) ResourceRecord;
    authorities = Many (header.nscount) ResourceRecord;
    additionals = Many (header.arcount) ResourceRecord
  }

  def Header = {
    id = uint16;
    flags = uint16;
    qdcount = uint16;
    ancount = uint16;
    nscount = uint16;
    arcount = uint16
  }

  def Question = {
    qname = DomainName;
    qtype = uint16;
    qclass = uint16
  }

  def ResourceRecord = {
    name = DomainName;
    type = uint16;
    class = uint16;
    ttl = uint32;
    rdlength = uint16;
    rdata = Take rdlength uint8
  }

  def DomainName = {
    labels = Many1 Label;
    $$ = 0
  }

  def Label = {
    length = uint8;
    if length == 0 then {
      $$ = 0
    } else {
      chars = Take length uint8;
      $$ = length
    }
  }

  DNSMessage
}