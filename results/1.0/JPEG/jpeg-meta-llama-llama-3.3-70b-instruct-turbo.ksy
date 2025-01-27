meta:
  id: jpeg
  file-extension: jpg
  fields:
    - name: magic
      seq:
        - id: soi
          type: u1
        - id: app0
          type: u1
        - id: length
          type: u2
        - id: identifier
          type: str
          size: 5
        - id: version
          type: u2
        - id: units
          type: u1
        - id: x-density
          type: u2
        - id: y-density
          type: u2
        - id: thumb-width
          type: u1
        - id: thumb-height
          type: u1
    - name: sofas
      seq:
        - id: marker
          type: u2
        - id: length
          type: u2
        - id: component-count
          type: u1
        - id: components
          type: component
          repeat: expr
          repeat-expr: component-count
    - name: sos
      seq:
        - id: marker
          type: u2
        - id: length
          type: u2
        - id: component-count
          type: u1
        - id: components
          type: component
          repeat: expr
          repeat-expr: component-count
    - name: eoi
      seq:
        - id: marker
          type: u2
  types:
    component:
      seq:
        - id: id
          type: u1
        - id: sampling-factor
          type: u1
        - id: quantization-table
          type: u1