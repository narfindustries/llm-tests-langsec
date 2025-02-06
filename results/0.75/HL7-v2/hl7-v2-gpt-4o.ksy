meta:
  id: hl7_v2
  title: "HL7 Version 2.x Message"
  file-extension: hl7
  license: CC0-1.0
  endian: le

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: fields
        type: field
        repeat: until
        repeat-until: _.is_end_of_segment

  field:
    seq:
      - id: value
        type: strz
        encoding: ascii
        terminator: 0x7C
        include: true

    instances:
      is_end_of_field:
        value: _io.pos >= _io.size or _io.read_u1 == 0x7C
        pos: _io.pos

      is_end_of_segment:
        value: _io.pos >= _io.size or _io.read_u1 == 0x0D
        pos: _io.pos

enums:
  administrative_sex:
    M: male
    F: female
    U: unknown

  result_status:
    F: final
    C: corrected
    P: preliminary

  allergy_type:
    DA: drug_allergy
    FA: food_allergy
    EA: environment_allergy

  severity:
    MI: mild
    MO: moderate
    SE: severe