type: struct
id: sqlite3_db
endian: be

seq:
  - id: magic
    type: str
    size: 16
  - id: pageSize
    type: u2
  - id: writeVersion
    type: u4
  - id: readVersion
    type: u4
  - id: reservedSpace
    type: u4
  - id: maxPageCount
    type: u4
  - id: changeCount
    type: u4
  - id: format
    type: u4
  - id: textEncoding
    type: u4
  - id: userVersion
    type: u4
  - id: incrementalVacuum
    type: u4
  - id: applicationId
    type: u8
  - id: versionValidFor
    type: u4
  - id: sqliteVersion
    type: u4
  - id: pages
    type: seq
    size: lambda: self.maxPageCount
    read: r
    elemType: page

types:
  page:
    seq:
      - id: pageType
        type: u1
      - id: pageNumber
        type: u4
      - id: freelistTrunkPage
        type: u4
      - id: cells
        type: seq
        size: lambda: self.num_cells
        elemType: cell
      - id: freelist
        type: seq
        size: lambda: self.num_freelist_entries
        elemType: u4

  cell:
    seq:
      - id: payloadSize
        type: uvarint
      - id: rowId
        type: uvarint
      - id: offset
        type: uvarint
      - id: payload
        type: bytes
        size: payloadSize

