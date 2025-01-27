module HL7-v2

type Message {
    segments: List<Segment>
}

type Segment {
    name: String,
    fields: List<Field>
}

type Field {
    value: String,
    components: List<Component>?
}

type Component {
    value: String,
    subcomponents: List<Subcomponent>?
}

type Subcomponent {
    value: String
}

type Patient {
    id: String,
    name: Name,
    demographics: Demographics
}

type Name {
    first: String,
    last: String,
    middle: String?
}

type Demographics {
    birthDate: String,
    gender: String,
    race: String?
}

type Encounter {
    id: String,
    type: String,
    date: String
}

type Observation {
    code: String,
    value: String,
    units: String?
}

parse Message from input: {
    segments = parse_segments(input)
}

parse Segment from input: {
    name = parse_segment_name(input),
    fields = parse_fields(input)
}

parse Field from input: {
    value = parse_field_value(input),
    components = parse_components(input)?
}

parse Component from input: {
    value = parse_component_value(input),
    subcomponents = parse_subcomponents(input)?
}

parse Subcomponent from input: {
    value = parse_subcomponent_value(input)
}