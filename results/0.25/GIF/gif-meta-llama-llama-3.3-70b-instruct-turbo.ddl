components {
  gif: {
    type: "sequence",
    elements: [
      {
        name: "header",
        type: "bytes",
        length: 3,
        validate: "eq(b'GIF')"
      },
      {
        name: "version",
        type: "bytes",
        length: 3
      },
      {
        name: "width",
        type: "uint16",
        byte_order: "little"
      },
      {
        name: "height",
        type: "uint16",
        byte_order: "little"
      },
      {
        name: "flags",
        type: "uint8"
      },
      {
        name: "bgcolor",
        type: "uint8"
      },
      {
        name: "pixels_per_unit",
        type: "uint8"
      },
      {
        name: "aspect_ratio",
        type: "uint8"
      },
      {
        name: "data",
        type: "sequence",
        until: "eos",
        elements: [
          {
            name: "block",
            type: "choice",
            arms: [
              {
                name: "image",
                test: "eq(b'\x2C')",
                then: {
                  name: "image_data",
                  type: "bytes"
                }
              },
              {
                name: "extension",
                test: "eq(b'\x21')",
                then: {
                  name: "extension_data",
                  type: "bytes"
                }
              },
              {
                name: "trailer",
                test: "eq(b'\x3B')",
                then: {}
              }
            ]
          }
        ]
      }
    ]
  }
}