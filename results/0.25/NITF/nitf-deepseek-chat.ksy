meta:
  id: nitf_deepseek_chat
  file-extension: nitf
  endian: be
  imports:
    - nitf

seq:
  - id: header
    type: nitf.header
  - id: image_segments
    type: nitf.image_segment
    repeat: eos
  - id: text_segments
    type: nitf.text_segment
    repeat: eos
  - id: data_extension_segments
    type: nitf.data_extension_segment
    repeat: eos
  - id: reserved_extension_segments
    type: nitf.reserved_extension_segment
    repeat: eos

types:
  header:
    seq:
      - id: file_profile_name
        type: str
        size: 4
      - id: file_version
        type: str
        size: 5
      - id: complexity_level
        type: str
        size: 2
      - id: standard_type
        type: str
        size: 4
      - id: originating_station_id
        type: str
        size: 10
      - id: file_date_and_time
        type: str
        size: 14
      - id: file_title
        type: str
        size: 80
      - id: file_security
        type: security
      - id: file_copy_number
        type: str
        size: 5
      - id: file_number_of_copies
        type: str
        size: 5
      - id: file_background_color
        type: str
        size: 3
      - id: originators_name
        type: str
        size: 24
      - id: originators_phone
        type: str
        size: 18

  security:
    seq:
      - id: classification
        type: str
        size: 1
      - id: codewords
        type: str
        size: 40
      - id: control_and_handling
        type: str
        size: 40
      - id: release_instructions
        type: str
        size: 40
      - id: declassification_type
        type: str
        size: 2
      - id: declassification_date
        type: str
        size: 8
      - id: declassification_exemption
        type: str
        size: 4
      - id: downgrade
        type: str
        size: 1
      - id: downgrade_date
        type: str
        size: 8
      - id: classification_text
        type: str
        size: 43
      - id: classification_authority_type
        type: str
        size: 1
      - id: classification_authority
        type: str
        size: 40
      - id: classification_reason
        type: str
        size: 1
      - id: security_source_date
        type: str
        size: 8
      - id: security_control_number
        type: str
        size: 15

  image_segment:
    seq:
      - id: image_subheader
        type: image_subheader
      - id: image_data
        type: image_data

  image_subheader:
    seq:
      - id: image_id
        type: str
        size: 10
      - id: image_date_and_time
        type: str
        size: 14
      - id: target_id
        type: str
        size: 17
      - id: image_title
        type: str
        size: 80
      - id: image_security
        type: security
      - id: image_compression
        type: str
        size: 2
      - id: image_representation
        type: str
        size: 8
      - id: image_category
        type: str
        size: 8
      - id: image_data_type
        type: str
        size: 2
      - id: image_resolution_level
        type: str
        size: 1
      - id: image_coverage
        type: str
        size: 22
      - id: image_magnification
        type: str
        size: 4
      - id: image_compression_rate_code
        type: str
        size: 4
      - id: image_color
        type: str
        size: 1
      - id: image_bands
        type: str
        size: 1
      - id: image_band_info
        type: str
        size: 12
      - id: image_band_combination
        type: str
        size: 4
      - id: image_band_compression
        type: str
        size: 4
      - id: image_band_compression_rate_code
        type: str
        size: 4
      - id: image_band_compression_algorithm
        type: str
        size: 4
      - id: image_band_compression_quality
        type: str
        size: 4
      - id: image_band_compression_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_algorithm
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_quality
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_ratio_code
        type: str
        size: 4
      - id: image_band_compression_ratio_ratio