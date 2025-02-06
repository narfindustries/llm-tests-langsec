types:
  jpeg_marker:
    seq:
      - id: marker_id
        type: u2
      - id: marker_length
        type: u2
      - id: marker_data
        type: bytes
        size: marker_length - 2

  jpeg_soi:
    seq:
      - id: soi_marker
        type: jpeg_marker
        id_field: marker_id
        id_value: 0xd8

  jpeg_eoi:
    seq:
      - id: eoi_marker
        type: jpeg_marker
        id_field: marker_id
        id_value: 0xd9

  jpeg_appn:
    seq:
      - id: appn_marker
        type: jpeg_marker
        id_field: marker_id
        id_value: 0xe0
        id_range: 0xef
      - id: appn_data
        type: bytes
        size: appn_marker.marker_length - 2

  jpeg_dqt:
    seq:
      - id: dqt_marker
        type: jpeg_marker
        id_field: marker_id
        id_value: 0xdb
      - id: dqt_data
        type: repeat
        size: dqt_marker.marker_length - 2
        element:
          seq:
            - id: precision
              type: u1
            - id: table_id
              type: u1
            - id: quantization_table
              type: u2
              size: 64

  jpeg_dht:
    seq:
      - id: dht_marker
        type: jpeg_marker
        id_field: marker_id
        id_value: 0xc4
      - id: dht_data
        type: repeat
        size: dht_marker.marker_length - 2
        element:
          seq:
            - id: table_class
              type: u1
            - id: table_id
              type: u1
            - id: code_lengths
              type: u1
              size: 16
            - id: huffman_symbols
              type: u1
              size: code_lengths.sum()

  jpeg_dri:
    seq:
      - id: dri_marker
        type: jpeg_marker
        id_field: marker_id
        id_value: 0xdd
      - id: restart_interval
        type: u2

  jpeg_sos:
    seq:
      - id: sos_marker
        type: jpeg_marker
        id_field: marker_id
        id_value: 0xda
      - id: num_components
        type: u1
      - id: component_specifiers
        type: repeat
        size: num_components
        element:
          seq:
            - id: component_id
              type: u1
            - id: dc_huffman_table_id
              type: u1
            - id: ac_huffman_table_id
              type: u1
      - id: start_spectral_selection
        type: u1
      - id: end_spectral_selection
        type: u1
      - id: successive_approximation
        type: u1
      - id: sos_data
        type: bytes

  jpeg_rstn:
    seq:
      - id: rstn_marker
        type: jpeg_marker
        id_field: marker_id
        id_value: 0xd0
        id_range: 0xd7

  jpeg_com:
    seq:
      - id: com_marker
        type: jpeg_marker
        id_field: marker_id
        id_value: 0xfe
      - id: comment_data
        type: str
        encoding: UTF-8

  jpeg_file:
    seq:
      - id: soi
        type: jpeg_soi
      - id: segments
        type: repeat
        element:
          switch: marker_id
            cases:
              0xe0:
                - id: appn
                  type: jpeg_appn
              0xdb:
                - id: dqt
                  type: jpeg_dqt
              0xc4:
                - id: dht
                  type: jpeg_dht
              0xdd:
                - id: dri
                  type: jpeg_dri
              0xda:
                - id: sos
                  type: jpeg_sos
              0xd0:
                - id: rstn
                  type: jpeg_rstn
                id_range: 0xd7
              0xfe:
                - id: com
                  type: jpeg_com
      - id: eoi
        type: jpeg_eoi

