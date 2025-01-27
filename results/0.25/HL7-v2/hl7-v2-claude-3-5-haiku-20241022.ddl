module HL7-v2

type Message {
    segments: List<Segment>
}

type Segment {
    name: String,
    fields: List<Field>
}

type Field {
    name: String,
    value: String,
    repetitions: List<String>?
}

type Patient {
    id: String,
    name: String,
    birthDate: String,
    gender: String
}

type Observation {
    code: String,
    value: String,
    units: String?
}

type MSH_Segment inherits Segment {
    sendingApplication: String,
    sendingFacility: String,
    receivingApplication: String,
    receivingFacility: String,
    dateTimeOfMessage: String,
    messageType: String
}

type PID_Segment inherits Segment {
    patientIdentifier: String,
    patientName: String,
    dateOfBirth: String,
    administrativeSex: String
}

type OBX_Segment inherits Segment {
    observationIdentifier: String,
    observationValue: String,
    units: String?
}

function parseHL7Message(rawMessage: String) -> Message {
    // Parsing logic for HL7 v2 message
    segments = split(rawMessage, "\r")
    
    return Message {
        segments: segments.map(parseSegment)
    }
}

function parseSegment(segmentText: String) -> Segment {
    fields = split(segmentText, "|")
    
    match fields[0] {
        "MSH" => return MSH_Segment {
            name: "MSH",
            fields: fields.map(createField),
            sendingApplication: fields[2],
            sendingFacility: fields[3],
            receivingApplication: fields[4],
            receivingFacility: fields[5],
            dateTimeOfMessage: fields[6],
            messageType: fields[8]
        },
        "PID" => return PID_Segment {
            name: "PID", 
            fields: fields.map(createField),
            patientIdentifier: fields[3],
            patientName: fields[5],
            dateOfBirth: fields[7],
            administrativeSex: fields[8]
        },
        "OBX" => return OBX_Segment {
            name: "OBX",
            fields: fields.map(createField),
            observationIdentifier: fields[3],
            observationValue: fields[5],
            units: fields[6]
        },
        _ => return Segment {
            name: fields[0],
            fields: fields.map(createField)
        }
    }
}

function createField(fieldValue: String) -> Field {
    return Field {
        name: "",  // Field name can be added if needed
        value: fieldValue,
        repetitions: fieldValue.contains("~") 
            ? split(fieldValue, "~") 
            : null
    }
}