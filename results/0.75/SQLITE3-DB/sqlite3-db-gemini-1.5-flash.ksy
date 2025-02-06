types:
  sqlite3_header:
    seq:
      - id: magic
        type: str
        size: 16
      - id: pageSize
        type: u4
      - id: writeVersion
        type: u4
      - id: readOnly
        type: u4
      - id: reservedSpace
        type: u4
      - id: maxPageCount
        type: u4
      - id: textEncoding
        type: u4
      - id: userVersion
        type: u4
      - id: incrementalVacuum
        type: u4
      - id: applicationId
        type: u8
      - id: versionValid
        type: u4
      - id: pageSize64
        type: u8
  sqlite3_page_header:
    seq:
      - id: pageType
        type: u1
      - id: pageNumber
        type: u4
      - id: freeBlockCount
        type: u2
      - id: cells
        type: sqlite3_cell_array
  sqlite3_cell_header:
    seq:
      - id: payloadSize
        type: u4
      - id: headerSize
        type: u4
      - id: rowId
        type: u8
        optional: true
  sqlite3_cell:
    seq:
      - id: header
        type: sqlite3_cell_header
      - id: payload
        type: bytes
        size: lambda: self.header.payloadSize

  sqlite3_cell_array:
    seq:
      - id: num_cells
        type: u2
      - id: cells
        type: sqlite3_cell
        repeat: expr
        expr: self.num_cells

  sqlite_db:
    seq:
      - id: header
        type: sqlite3_header
      - id: pages
        type: sqlite3_page_header
        repeat: eos
