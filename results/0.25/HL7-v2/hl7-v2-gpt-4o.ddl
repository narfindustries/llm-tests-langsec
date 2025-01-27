module HL7v2

import daedalus::core::ascii

// Define a segment as a sequence of fields
segment Segment {
    id: ascii::String(3),
    fields: List<Field>
}

// Define a field as a sequence of components
field Field {
    components: List<Component>
}

// Define a component as a sequence of subcomponents
component Component {
    subcomponents: List<Subcomponent>
}

// Define a subcomponent as a simple string
subcomponent Subcomponent {
    value: ascii::String
}

// Define the HL7 message structure
message HL7Message {
    segments: List<Segment>
}

// Define the HL7 message parser
parser HL7Parser {
    message: HL7Message = parseMessage
}

// Function to parse an HL7 message
function parseMessage(): HL7Message {
    segments = parseSegments()
    return HL7Message { segments }
}

// Function to parse segments
function parseSegments(): List<Segment> {
    segments = []
    while (not endOfInput()) {
        segment = parseSegment()
        segments.append(segment)
    }
    return segments
}

// Function to parse a single segment
function parseSegment(): Segment {
    id = parseSegmentId()
    fields = parseFields()
    return Segment { id, fields }
}

// Function to parse a segment ID
function parseSegmentId(): ascii::String(3) {
    return ascii::take(3)
}

// Function to parse fields
function parseFields(): List<Field> {
    fields = []
    while (not endOfSegment()) {
        field = parseField()
        fields.append(field)
    }
    return fields
}

// Function to parse a single field
function parseField(): Field {
    components = parseComponents()
    return Field { components }
}

// Function to parse components
function parseComponents(): List<Component> {
    components = []
    while (not endOfField()) {
        component = parseComponent()
        components.append(component)
    }
    return components
}

// Function to parse a single component
function parseComponent(): Component {
    subcomponents = parseSubcomponents()
    return Component { subcomponents }
}

// Function to parse subcomponents
function parseSubcomponents(): List<Subcomponent> {
    subcomponents = []
    while (not endOfComponent()) {
        subcomponent = parseSubcomponent()
        subcomponents.append(subcomponent)
    }
    return subcomponents
}

// Function to parse a single subcomponent
function parseSubcomponent(): Subcomponent {
    value = parseSubcomponentValue()
    return Subcomponent { value }
}

// Function to parse a subcomponent value
function parseSubcomponentValue(): ascii::String {
    return ascii::takeUntil(['|', '^', '&', '\r', '\n'])
}

// Helper functions to determine end of segments, fields, components
function endOfSegment(): Bool {
    return peek() == '\r' || peek() == '\n'
}

function endOfField(): Bool {
    return peek() == '|'
}

function endOfComponent(): Bool {
    return peek() == '^'
}