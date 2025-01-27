def Main = {
  result: HL7Message;
  result = HL7Message;
}

def HL7Message = {
  segments: [Segment];
  segments = @array_of(Segment, $"^");
}

def Segment = {
  segmentType: Text;
  fields: [Field];
  segmentType = @take_until($"|");
  @match($"|");
  fields = @array_of(Field, $"|");
}

def Field = {
  components: [Component];
  components = @array_of(Component, $"^");
}

def Component = {
  subComponents: [SubComponent];
  subComponents = @array_of(SubComponent, $"&");
}

def SubComponent = {
  value: Text;
  value = @take_until_one_of($"|^&\n");
}

def Text = @take_until_one_of($"|^&\n")