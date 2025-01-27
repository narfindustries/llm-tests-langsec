domain zip {
  type zip_file {
    structure {
      local_file_header: local_file_header,
      file_data: bytes(file_descriptor.size),
      data_descriptor: optional(data_descriptor),
      file_extra_field: optional(file_extra_field)
    }
  }

  type local_file_header {
    structure {
      signature: uint32(0x04034b50),
      version_needed: uint16,
      flags: uint16,
      compression_method: uint16,
      last_modified_time: uint16,
      last_modified_date: uint16,
      crc32: uint32,
      compressed_size: uint32,
      uncompressed_size: uint32,
      file_name_length: uint16,
      extra_field_length: uint16,
      file_name: string(file_name_length),
      extra_field: bytes(extra_field_length)
    }
  }

  type file_descriptor {
    structure {
      signature: uint32(0x08074b50),
      crc32: uint32,
      compressed_size: uint32,
      uncompressed_size: uint32
    }
  }

  type data_descriptor {
    structure {
      signature: uint32(0x08074b50),
      crc32: uint32,
      compressed_size: uint32,
      uncompressed_size: uint32
    }
  }

  type file_extra_field {
    structure {
      header_id: uint16,
      data_size: uint16,
      data: bytes(data_size)
    }
  }
}