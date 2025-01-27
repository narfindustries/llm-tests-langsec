meta:
  id: nitf
  file-extension: nitf
  endian: big
  imports:
    - ./nitf-meta-llama-llama-3
    - ./nitf-header
    - ./nitf-tre
    - ./nitf-image
  encoding: ASCII

seq:
  - id: file-header
    type: nitf-header
  - id: image-segments
    type: nitf-image
    repeat: until eos