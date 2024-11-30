```yaml
metatags:
  title: PNG image file format
  description: PNG (Portable Network Graphics) is a widely used raster graphics file format that supports lossless data compression.
  url: https://en.wikipedia.org/wiki/Portable_Network_Graphics
  mimetype: image/png
  extension: png
  authors:
    - Kevin Bellah
layout:
  # PNG signature
  signature: {size: 8, desc: 'PNG image file header signature'}
  # Header
  header:
    critical: {}
    ancillary: {}
    header_crc: {size: 4, desc: 'CRC value of the header'}
  # IHDR chunk
  ihdr_chunk:
    type: {size: 4, const: 'IHDR', desc: 'IHDR chunk type'}
    IHDR_chunk_size: {size: 4, desc: 'length of the following data field'}
    width: {size: 4, desc: 'Image width in pixels'}
    height: {size: 4, desc: 'Image height in pixels'}
    bit_depth: {size: 1, desc: 'Bit depth of the image'}
    color_type: {size: 1, desc: 'Color type of the image'}
    compression_method: {size: 1, desc: 'Compression method used in the image'}
    filter_method: {size: 1, desc: 'Filter method used in the image'}
    interlace_method: {size: 1, desc: 'Interlace method used in the image'}
    crc: {size: 4, desc: 'CRC value of the IHDR chunk'}
  # PLTE chunk
  plte_chunk:
    type: {size: 4, const: 'PLTE', desc: 'PLTE chunk type'}
    PLTE_chunk_size: {size: 4, desc: 'length of the following data field'}
    palette_entries:
      idx:
      - {size: 3, desc: 'Red palette entry'}
      - {size: 3, desc: 'Green palette entry'}
      - {size: 3, desc: 'Blue palette entry'}
      - {size: 1, desc: 'unused (must be 0)'}
    crc: {size: 4, desc: 'CRC value of the PLTE chunk'}
  # IDAT chunk
  idat_chunk:
    type: {size: 4, const: 'IDAT', desc: 'IDAT chunk type'}
    IDAT_chunk_size: {size: 4, desc: 'length of the following data field'}
    image_data: {} # Raw image data
    crc: {size: 4, desc: 'CRC value of the IDAT chunk'}
  # IEND chunk
  iend_chunk:
    type: {size: 4, const: 'IEND', desc: 'IEND chunk type'}
    crc: {size: 4, desc: 'CRC value of the IEND chunk'}
```