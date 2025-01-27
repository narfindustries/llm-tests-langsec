module HL7v2GPT4Turbo {
  import std::ascii

  // Define the structure of a typical HL7 message segment
  type Segment = struct {
    segmentType : ascii::StringN(3);
    fields      : [Field];
  }

  // Define the structure of fields in a segment
  type Field = struct {
    separator   : ascii::Char('=');
    content     : ascii::StringUntil(['\r', '\n', '|']);
  }

  // Define the overall HL7 message structure
  type Message = struct {
    segments : [Segment] &until($ == Segment { segmentType = "END" });
    endSegment : Segment;
  }

  // Entry point for parsing
  let parseHL7Message = parse Message;
}