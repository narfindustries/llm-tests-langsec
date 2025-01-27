module HL7v2GPT4Turbo {
  import std::ascii

  // Define the structure of an HL7 message segment
  type Segment = struct {
    name: ascii::StringN(3),
    fields: [Field] using DelimitedBy('|', ascii::String)
  }

  // Define the structure of an HL7 message
  type Message = struct {
    segments: [Segment] using DelimitedBy('\r', Segment)
  }

  // Define the structure of a field, which can contain multiple components
  type Field = struct {
    components: [ascii::String] using DelimitedBy('^', ascii::String)
  }

  // Entry point for parsing
  let parse_hl7 = Message
}