meta:
  id: dicom
  endian: le
seq:
  - id: preamble
    size: 128
  - id: prefix
    type: str
    size: 4
    encoding: ascii
    enum: DICM
  - id: header
    type: dicom_header
types:
  dicom_header:
    seq:
      - id: group_0002
        type: group
      - id: transfer_syntax_uid
        type: ui
      - id: implementation_class_uid
        type: ui
      - id: transfer_syntax_uid_2
        type: ui
      - id: implementation_class_uid_2
        type: ui
      - id: source_application_entity_title
        type: ae
      - id: sending_application_entity_title
        type: ae
      - id: receiving_application_entity_title
        type: ae
      - id: private_0002
        type: private_creator
      - id: file_meta_information_version
        type: ui
      - id: media_storage_sop_instance_uid
        type: ui
      - id: media_storage_sop_class_uid
        type: ui
      - id: implementation_version_name
        type: sh
      - id: source_application_entity_title_2
        type: ae
      - id: sending_application_entity_title_2
        type: ae
      - id: receiving_application_entity_title_2
        type: ae
      - id: private_0002_2
        type: private_creator
      - id: file_meta_information_group_length
        type: ul
      - id: file_set_id
        type: ui
      - id: file_set_descriptor
        type: ui
      - id: specific_character_set
        type: cs
      - id: offset_table
        type: ob
      - id: offset_table_length
        type: ul
      - id: private_0002_3
        type: private_creator
      - id: file_set_id_2
        type: ui
      - id: file_set_descriptor_2
        type: ui
      - id: specific_character_set_2
        type: cs
      - id: offset_table_2
        type: ob
      - id: offset_table_length_2
        type: ul
      - id: private_0002_4
        type: private_creator
  group:
    seq:
      - id: group_number
        type: ui
      - id: element_number
        type: ui
      - id: value_representation
        type: vr
      - id: value_length
        type: ul
      - id: value
        type:
          switch-on: value_representation
          cases:
            AE: ae
            AS: as
            AT: at
            CS: cs
            DA: da
            DS: ds
            DT: dt
            FL: fl
            FD: fd
            IS: is
            LO: lo
            LT: lt
            OB: ob
            OD: od
            OF: of
            OL: ol
            OW: ow
            PN: pn
            SH: sh
            SL: sl
            SQ: sq
            SS: ss
            ST: st
            TM: tm
            UI: ui
            UL: ul
            UN: un
            UR: ur
            US: us
            UV: uv
  private_creator:
    seq:
      - id: private_creator_id
        type: lo
      - id: private_element_number
        type: ui
      - id: value_representation
        type: vr
      - id: value_length
        type: ul
      - id: value
        type:
          switch-on: value_representation
          cases:
            AE: ae
            AS: as
            AT: at
            CS: cs
            DA: da
            DS: ds
            DT: dt
            FL: fl
            FD: fd
            IS: is
            LO: lo
            LT: lt
            OB: ob
            OD: od
            OF: of
            OL: ol
            OW: ow
            PN: pn
            SH: sh
            SL: sl
            SQ: sq
            SS: ss
            ST: st
            TM: tm
            UI: ui
            UL: ul
            UN: un
            UR: ur
            US: us
            UV: uv
  ae:
    type: str
    size: 16
    encoding: ascii
  as:
    type: str
    size: 4
    encoding: ascii
  at:
    type: ui
  cs:
    type: str
    size: 16
    encoding: ascii
  da:
    type: str
    size: 8
    encoding: ascii
  ds:
    type: str
    size: 16
    encoding: ascii
  dt:
    type: str
    size: 26
    encoding: ascii
  fl:
    type: f4
  fd:
    type: f8
  is:
    type: i4
  lo:
    type: str
    size: 64
    encoding: ascii
  lt:
    type: str
    size: 10240
    encoding: ascii
  ob:
    type: b
  od:
    type: f8
  of:
    type: f4
  ol:
    type: i8
  ow:
    type: u4
  pn:
    type: str
    size: 64
    encoding: ascii
  sh:
    type: str
    size: 16
    encoding: ascii
  sl:
    type: i4
  sq:
    type: seq_of_items
  ss:
    type: i2
  st:
    type: str
    size: 1024
    encoding: ascii
  tm:
    type: str
    size: 14
    encoding: ascii
  ui:
    type: ui
  ul:
    type: u4
  un:
    type: un
  ur:
    type: ur
  us:
    type: u2
  uv:
    type: uv
  seq_of_items:
    seq:
      - id: item
        type: item
        repeat: expr
  item:
    seq:
      - id: item_delimiter
        type: ui
      - id: item_data
        type: item_data
  item_data:
    seq:
      - id: value_representation
        type: vr
      - id: value_length
        type: ul
      - id: value
        type:
          switch-on: value_representation
          cases:
            AE: ae
            AS: as
            AT: at
            CS: cs
            DA: da
            DS: ds
            DT: dt
            FL: fl
            FD: fd
            IS: is
            LO: lo
            LT: lt
            OB: ob
            OD: od
            OF: of
            OL: ol
            OW: ow
            PN: pn
            SH: sh
            SL: sl
            SQ: sq
            SS: ss
            ST: st
            TM: tm
            UI: ui
            UL: ul
            UN: un
            UR: ur
            US: us
            UV: uv
  vr:
    type: str
    size: 2
    encoding: ascii
    enum: [ "AE", "AS", "AT", "CS", "DA", "DS", "DT", "FL", "FD", "IS", "LO", "LT", "OB", "OD", "OF", "OL", "OW", "PN", "SH", "SL", "SQ", "SS", "ST", "TM", "UI", "UL", "UN", "UR", "US", "UV" ]