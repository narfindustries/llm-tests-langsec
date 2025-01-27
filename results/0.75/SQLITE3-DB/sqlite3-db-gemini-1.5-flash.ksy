meta:
  id: sqlite3-db-gemini-1
  endian: be

types:
  header:
    seq:
      - id: magic
        type: u4
      - id: version
        type: u4
      - id: page_size
        type: u4
      - id: reserved
        type: u4
  page:
    seq:
      - id: header
        type: header
      - id: cell_count
        type: u2
      - id: free_bytes
        type: u2
      - id: cells
        type: cells
  cell:
    seq:
      - id: data
        type: bytes
  cells:
    seq:
      - id: cell
        type: cell
        repeat: expr
        expr: this.parent.header.cell_count

  sqlite3_db:
    seq:
      - id: pages
        type: page
        repeat: expr
        expr: 10 # adjust as needed

