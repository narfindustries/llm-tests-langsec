meta:
  id: tls_client_hello
  title: TLS Client Hello
  file-extension: bin
  endian: be
  application: TLS
  license: MIT

seq:
  - id: record_length
    type: u2
  - id: message_type
    type: u1
  - id: version
    type: u2
  - id: random
    type: random

types:
  random:
    seq:
      - id: gmt_unix_time
        type: u4
      - id: random_bytes
        size: 28