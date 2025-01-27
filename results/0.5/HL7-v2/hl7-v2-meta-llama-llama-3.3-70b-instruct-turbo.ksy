meta:
  id: hl7-v2-meta
  title: HL7 v2 Meta
  doc: https://hl7.org/implement/standards/product_brief.cfm?product_id=185
seq:
  - id: segment
    type: segment
    repeat: until eos

types:
  segment:
    seq:
      - id: segment_type
        type: str
        size: 3
      - id: fields
        type: field
        repeat: until eos
    doc: An HL7 segment

  field:
    seq:
      - id: component
        type: component
        repeat: until eos
    doc: An HL7 field

  component:
    seq:
      - id: sub_component
        type: sub_component
        repeat: until eos
    doc: An HL7 component

  sub_component:
    seq:
      - id: value
        type: str
    doc: An HL7 sub-component