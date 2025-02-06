let dns_header = 
  { transaction_id: uint16
  , qr: bool
  , opcode: uint4
  , aa: bool
  , tc: bool
  , rd: bool
  , ra: bool
  , z: uint3
  , rcode: uint4
  , question_count: uint16
  , answer_count: uint16
  , authority_count: uint16
  , additional_count: uint16
  }

let dns_name = 
  | Compressed of uint16
  | Uncompressed of list<string>

let dns_question = 
  { name: dns_name
  , type: uint16
  , class: uint16
  }

let dns_rr_type = 
  | A
  | NS
  | CNAME
  | SOA
  | PTR
  | MX
  | AAAA

let dns_rr_class = 
  | IN
  | CH
  | HS

let dns_rdata = 
  | A_Record of bytes(4)
  | AAAA_Record of bytes(16)
  | NS_Record of dns_name
  | CNAME_Record of dns_name
  | PTR_Record of dns_name
  | MX_Record of 
    { preference: uint16
    , exchange: dns_name
    }
  | SOA_Record of 
    { mname: dns_name
    , rname: dns_name
    , serial: uint32
    , refresh: uint32
    , retry: uint32
    , expire: uint32
    , minimum: uint32
    }

let dns_resource_record = 
  { name: dns_name
  , type: dns_rr_type
  , class: dns_rr_class
  , ttl: uint32
  , rdlength: uint16
  , rdata: dns_rdata
  }

let dns_message = 
  { header: dns_header
  , questions: list<dns_question>
  , answers: list<dns_resource_record>
  , authorities: list<dns_resource_record>
  , additionals: list<dns_resource_record>
  }