meta:
  id: sqlite3-db-gemini-1
  title: SQLite3 DB Gemini 1.5 Flash
  homepage: https://kaitai.io
types:
  - id: header
    seq:
      - id: magic
        type: u4
        enum:
          - 0x53514c69: SQLITE
      - id: version
        type: u4
      - id: page_size
        type: u4
      - id: reserved
        type: u4
  - id: page
    seq:
      - id: header
        type: header
      - id: data
        type: bytes
        size: lambda: self.header.page_size - 100
  - id: db
    seq:
      - id: header
        type: header
      - id: pages
        type: page
        repeat: expr
        expr: lambda: self.header.page_size * 1000 # Example, adjust as needed

