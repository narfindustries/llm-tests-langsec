grammar HL7v2 {
  Main = Segment+
  Segment = SegmentHeader FieldSeparator FieldList CR
  SegmentHeader = Alpha{3}
  FieldSeparator = "|"
  FieldList = Field (FieldSeparator Field)*
  Field = Component (ComponentSeparator Component)* | ""
  Component = SubComponent (SubComponentSeparator SubComponent)* | ""
  SubComponent = Value | ""
  Value = (!Separator !CR Alpha+) | ""
  ComponentSeparator = "^"
  SubComponentSeparator = "&"
  Separator = FieldSeparator | ComponentSeparator | SubComponentSeparator
  CR = "\r" | "\n" | "\r\n"
  Alpha = [a-zA-Z0-9 ._-]
}