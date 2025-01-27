def Main = {
  def domain_name = {
    def label = {
      $len = uint8;
      Take len Many byte
    };
    def labels = {
      $first = label;
      Many {
        $next = label;
        first ^ "." ^ next
      }
    };
    labels
  }

  def record_type = {
    Choose {
      1 -> Pure "A"
      2 -> Pure "NS"
      5 -> Pure "CNAME"
      15 -> Pure "MX"
      16 -> Pure "TXT"
      28 -> Pure "AAAA"
    }
  }

  def record_class = {
    Choose {
      1 -> Pure "IN"
      2 -> Pure "CS"
      3 -> Pure "CH"
      4 -> Pure "HS"
    }
  }

  def resource_record = {
    $name = domain_name;
    $type = uint16;
    $class = uint16;
    $ttl = uint32;
    $rdlength = uint16;
    $rdata = Take rdlength Many byte;
    {
      name = name,
      type = type,
      class = class,
      ttl = ttl,
      rdlength = rdlength,
      rdata = rdata
    }
  }

  def header = {
    $id = uint16;
    $flags = uint16;
    $qdcount = uint16;
    $ancount = uint16;
    $nscount = uint16;
    $arcount = uint16;
    {
      id = id,
      flags = flags,
      qdcount = qdcount,
      ancount = ancount,
      nscount = nscount,
      arcount = arcount
    }
  }

  def question = {
    $qname = domain_name;
    $qtype = uint16;
    $qclass = uint16;
    {
      qname = qname,
      qtype = qtype,
      qclass = qclass
    }
  }

  def dns_message = {
    $hdr = header;
    $questions = Repeat hdr.qdcount question;
    $answers = Repeat hdr.ancount resource_record;
    $authorities = Repeat hdr.nscount resource_record;
    $additionals = Repeat hdr.arcount resource_record;
    {
      header = hdr,
      questions = questions,
      answers = answers,
      authorities = authorities,
      additionals = additionals
    }
  }

  dns_message
}