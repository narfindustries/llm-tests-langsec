grammar HL7v2;

message_type Message {
  segments: Segment[]
}

segment Segment {
  tag: String &size 3;
  fields: Field[];
}

field Field {
  components: Component[];
}

component Component {
  subcomponents: Subcomponent[];
}

subcomponent Subcomponent {
  value: String;
}

Root {
  message: Message;
}

# HL7 specific rules
string String {
  charset: "UTF-8";
  terminator: "\r";
}

# Additional rules might be required, depending on the specific HL7v2 variant
# Consider extending grammar with specific segment types, 
# component rules, etc. as per HL7 v2.