domain zip {
  type zip_file {
    byte-order: little-endian
    structure {
      local-file-headers: list[local-file-header] count: num-entries
      central-directory: central-directory-header
      end-of-central-directory: end-of-central-directory-record
    }
  }

  type local-file-header {
    byte-order: little-endian
    structure {
      signature: uint32
      version: uint16
      flags: uint16
      compression-method: uint16
      last-modified-time: uint16
      last-modified-date: uint16
      crc-32: uint32
      compressed-size: uint32
      uncompressed-size: uint32
      file-name-length: uint16
      extra-field-length: uint16
      file-name: string(file-name-length)
      extra-field: bytes(extra-field-length)
    }
  }

  type central-directory-header {
    byte-order: little-endian
    structure {
      signature: uint32
      version: uint16
      version-needed-to-extract: uint16
      flags: uint16
      compression-method: uint16
      last-modified-time: uint16
      last-modified-date: uint16
      crc-32: uint32
      compressed-size: uint32
      uncompressed-size: uint32
      file-name-length: uint16
      extra-field-length: uint16
      file-comment-length: uint16
      disk-number: uint16
      internal-attributes: uint16
      external-attributes: uint32
      header-offset: uint32
      file-name: string(file-name-length)
      extra-field: bytes(extra-field-length)
      file-comment: bytes(file-comment-length)
    }
  }

  type end-of-central-directory-record {
    byte-order: little-endian
    structure {
      signature: uint32
      disk-number: uint16
      central-directory-start-disk: uint16
      num-entries-on-this-disk: uint16
      total-num-entries: uint16
      central-directory-size: uint32
      central-directory-offset: uint32
      comment-length: uint16
      comment: bytes(comment-length)
    }
  }

  type num-entries {
    byte-order: little-endian
    structure {
      num-entries: uint16
    }
  }
}