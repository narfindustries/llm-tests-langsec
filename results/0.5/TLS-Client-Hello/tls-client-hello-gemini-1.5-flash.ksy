meta:
  id: tls-client-hello-gemini-1
  title: TLS Client Hello (Gemini 1.5 Flash)
  url: https://example.com/tls-client-hello-gemini-1.ksy  # Replace with actual URL if available

types:
  # Define any custom types if needed

seq:
  - id: record_layer
    type: record_layer_t

  # Add other sequences as needed


structs:
  record_layer_t:
    seq:
      - id: content_type
        type: u8
      - id: version
        type: u16be
      - id: length
        type: u16be
      - id: fragment
        type: bytes
        size: length


# Add more structs as needed

