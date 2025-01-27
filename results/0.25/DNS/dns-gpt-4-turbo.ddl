module DNS {

  import DAEDALUS::BitManip;
  import DAEDALUS::Bytes;

  type U8  = UInt(8);
  type U16 = UInt(16);
  type U32 = UInt(32);

  type DomainName = {
    labels : List(String) &UntilEmpty,
    term   : U8 = 0
  } &Layout(LeftRecursive);

  type Question = {
    qName  : DomainName,
    qType  : U16,
    qClass : U16
  };

  type ResourceRecord = {
    rrName     : DomainName,
    rrType     : U16,
    rrClass    : U16,
    ttl        : U32,
    rdLength   : U16,
    rData      : Bytes(rdLength)
  };

  type Header = {
    id      : U16,
    qr      : U1,
    opcode  : U4,
    aa      : U1,
    tc      : U1,
    rd      : U1,
    ra      : U1,
    z       : U3,
    rcode   : U4,
    qdCount : U16,
    anCount : U16,
    nsCount : U16,
    arCount : U16
  };

  type DNSPacket = {
    header     : Header,
    questions  : List(Question, header.qdCount),
    answers    : List(ResourceRecord, header.anCount),
    authority  : List(ResourceRecord, header.nsCount),
    additional : List(ResourceRecord, header.arCount)
  };

  entrypoint parseDNS : DNSPacket;
}