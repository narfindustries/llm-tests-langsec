module HL7V2;

message HL7Message {
    segments: Segment[];
}

segment Segment {
    id: string(3);
    fields: Field[];
}

field Field {
    components: Component[];
}

component Component {
    subcomponents: Subcomponent[];
}

subcomponent Subcomponent {
    value: string;
}

// Define specific segment types based on HL7 v2.x standards
segment MSH extends Segment {
    id: "MSH";
    fields: [
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "FieldSeparator" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "EncodingCharacters" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "SendingApplication" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "SendingFacility" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "ReceivingApplication" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "ReceivingFacility" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "DateTimeOfMessage" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "Security" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "MessageType" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "MessageControlID" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "ProcessingID" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "VersionID" } ] } ] }
    ];
}

// Example of a PID segment
segment PID extends Segment {
    id: "PID";
    fields: [
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "SetID" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "PatientID" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "PatientIdentifierList" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "AlternatePatientID" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "PatientName" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "MotherMaidenName" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "DateTimeOfBirth" } ] } ] },
        Field { components: [ Component { subcomponents: [ Subcomponent { value: "AdministrativeSex" } ] } ] }
    ];
}

// Additional segments can be defined similarly