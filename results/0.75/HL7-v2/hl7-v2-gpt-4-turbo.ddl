module HL7v2GPT4Turbo {

  -- Define the Segment structure
  struct Segment {
    segType : String(3);
    segContent : UntilEOL;
  }

  -- Define a Message containing an array of segments, stops when the EOF is reached
  struct Message {
    segments : [Segment];
  }

  -- Top-level structure for HL7 messages
  struct HL7v2Messages {
    messages : [Message]('\r');
  }

}