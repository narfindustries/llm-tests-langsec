def Main = {
  Header *
  Records
}

def Header = {
  ID:$uint16
  Flags:$uint16
  QDCount:$uint16
  ANCount:$uint16
  NSCount:$uint16
  ARCount:$uint16
}

def Records = {
  Questions:   (Question   * QDCount)
  Answers:     (RRecord    * ANCount)
  Authorities: (RRecord    * NSCount)
  Additional:  (RRecord    * ARCount)
}

def Question = {
  Name:DomainName
  Type:$uint16
  Class:$uint16
}

def RRecord = {
  Name:DomainName
  Type:$uint16
  Class:$uint16
  TTL:$uint32
  RDLength:$uint16
  RData:$bytes RDLength
}

def DomainName = {
  @local len
  @local result = []
  loop {
    len = $uint8
    if len == 0 then break
    if (len & 0xC0) == 0xC0 then {
      ptr = ((len & 0x3F) << 8) | $uint8
      break
    }
    label = $bytes len
    result = result ++ [label]
  }
  result
}