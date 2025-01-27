meta:
  id: dns-gemini-1
  title: DNS Gemini 1.5 Flash
  homepage: ""
  file-extension: ""
  license: ""
  compiler-options:
    python:
      outdir: "generated/999999/0.75/DNS/output_kaitai/dns-gemini-1"

types:
  - id: header
    seq:
      - id: magic
        type: u4
      - id: version
        type: u2
      - id: flags
        type: u2
      - id: timestamp
        type: u4
      - id: size
        type: u4

  - id: entry
    seq:
      - id: type
        type: u2
      - id: data
        type: u4

  - id: data_block
    seq:
      - id: entries
        type: entry
        repeat: expr
        repeat-expr: header.size

seq:
  - id: header
    type: header
  - id: data
    type: data_block
