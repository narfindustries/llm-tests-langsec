module HL7V2

import Text

type Message = {
    segments: List Segment
}

type Segment = {
    id: String(3),
    fields: List Field
}

type Field = {
    components: List Component
}

type Component = {
    subcomponents: List Subcomponent
}

type Subcomponent = {
    value: String
}

parser HL7V2Parser {
    message: Message = {
        segments: many1(segment)
    }

    segment: Segment = {
        id: take(3),
        fields: many1(field)
    }

    field: Field = {
        components: many1(component)
    }

    component: Component = {
        subcomponents: many1(subcomponent)
    }

    subcomponent: Subcomponent = {
        value: takeWhile1(notChar('|'))
    }

    takeWhile1: (Char -> Bool) -> String = \pred -> {
        result <- takeWhile(pred)
        if (length(result) == 0) {
            fail("Expected at least one character")
        } else {
            return result
        }
    }

    notChar: Char -> Char -> Bool = \c1 c2 -> c1 /= c2
}