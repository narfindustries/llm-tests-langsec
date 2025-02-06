type DnsMessage = {
  header: {
    transaction_id: u16,
    flags: u16,
    question_count: u16,
    answer_count: u16,
    authority_count: u16,
    additional_count: u16
  },
  questions: [
    {
      name: [
        {
          length: u8,
          data: bytes(length)
        }
      ],
      type: u16,
      class: u16
    }
  ] if question_count > 0,
  answers: [
    {
      name: [
        {
          length: u8,
          data: bytes(length)
        }
      ],
      type: u16,
      class: u16,
      ttl: s32,
      rdlength: u16,
      rdata: 
        if type == 1 then bytes(4)
        else if type == 2 then [{ length: u8, data: bytes(length) }]
        else if type == 5 then [{ length: u8, data: bytes(length) }]
        else if type == 6 then {
          primary_ns: [{ length: u8, data: bytes(length) }],
          responsible_mailbox: [{ length: u8, data: bytes(length) }],
          serial: u32,
          refresh: s32,
          retry: s32,
          expire: s32,
          minimum: u32
        }
        else if type == 12 then [{ length: u8, data: bytes(length) }]
        else if type == 15 then {
          preference: u16,
          exchange: [{ length: u8, data: bytes(length) }]
        }
        else if type == 28 then bytes(16)
        else bytes(rdlength)
    }
  ] if answer_count > 0,
  authorities: [
    {
      name: [
        {
          length: u8,
          data: bytes(length)
        }
      ],
      type: u16,
      class: u16,
      ttl: s32,
      rdlength: u16,
      rdata: 
        if type == 1 then bytes(4)
        else if type == 2 then [{ length: u8, data: bytes(length) }]
        else if type == 5 then [{ length: u8, data: bytes(length) }]
        else if type == 6 then {
          primary_ns: [{ length: u8, data: bytes(length) }],
          responsible_mailbox: [{ length: u8, data: bytes(length) }],
          serial: u32,
          refresh: s32,
          retry: s32,
          expire: s32,
          minimum: u32
        }
        else if type == 12 then [{ length: u8, data: bytes(length) }]
        else if type == 15 then {
          preference: u16,
          exchange: [{ length: u8, data: bytes(length) }]
        }
        else if type == 28 then bytes(16)
        else bytes(rdlength)
    }
  ] if authority_count > 0,
  additionals: [
    {
      name: [
        {
          length: u8,
          data: bytes(length)
        }
      ],
      type: u16,
      class: u16,
      ttl: s32,
      rdlength: u16,
      rdata: 
        if type == 1 then bytes(4)
        else if type == 2 then [{ length: u8, data: bytes(length) }]
        else if type == 5 then [{ length: u8, data: bytes(length) }]
        else if type == 6 then {
          primary_ns: [{ length: u8, data: bytes(length) }],
          responsible_mailbox: [{ length: u8, data: bytes(length) }],
          serial: u32,
          refresh: s32,
          retry: s32,
          expire: s32,
          minimum: u32
        }
        else if type == 12 then [{ length: u8, data: bytes(length) }]
        else if type == 15 then {
          preference: u16,
          exchange: [{ length: u8, data: bytes(length) }]
        }
        else if type == 28 then bytes(16)
        else bytes(rdlength)
    }
  ] if additional_count > 0
}