types:
  nitf_header:
    seq:
      - id: signature
        type: str
        size: 12
      - id: version
        type: str
        size: 4
      - id: offset_to_next_header
        type: u4
      - id: header_length
        type: u4
      - id: security_classification
        type: str
        size: 1
      # ... Add remaining fields as needed ...


  image_header:
    seq:
      - id: signature
        type: str
        size: 12
      - id: image_data_type
        type: u2
      - id: image_dimensions
        type: struct
        fields:
          - id: width
            type: u4
          - id: height
            type: u4
      - id: bits_per_pixel
        type: u2
      - id: compression_type
        type: u2
      - id: image_data_location
        type: u4
      # ... Add remaining fields as needed ...


  data_extension:
    seq:
      - id: extension_type
        type: str
        size: 4
      - id: extension_length
        type: u4
      - id: extension_data
        type: bytes
        size: lambda: self.extension_length


  nitf_file:
    seq:
      - id: file_header
        type: nitf_header
      - id: image_headers
        type: array
        type: image_header
        size: lambda: self.num_images
      - id: data_extensions
        type: array
        type: data_extension
        size: lambda: self.num_extensions
      - id: image_data
        type: bytes
        size: lambda: self.image_data_length

