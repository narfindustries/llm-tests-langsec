module HL7V2 {
    use Prelude;

    type Delimiters = record {
        field_separator: u8,
        component_separator: u8,
        repetition_separator: u8,
        escape_character: u8,
        subcomponent_separator: u8
    };

    enum MessageType {
        ADT,
        ORM,
        ORU,
        RDE
    }

    type PersonName = record {
        last_name: string,
        first_name: string,
        middle_name: option<string>,
        suffix: option<string>
    };

    type Address = record {
        street: string,
        city: string,
        state: string,
        postal_code: string,
        country: option<string>
    };

    type MessageHeader = record {
        sending_application: string,
        sending_facility: string,
        receiving_application: string,
        receiving_facility: string,
        timestamp: datetime,
        message_type: MessageType,
        message_control_id: string,
        processing_id: string,
        version: string
    };

    type PatientIdentification = record {
        patient_id: string,
        patient_name: PersonName,
        date_of_birth: option<datetime>,
        gender: string,
        address: Address,
        phone_number: option<string>
    };

    type ObservationResult = record {
        set_id: u32,
        value_type: string,
        observation_identifier: string,
        observation_value: string,
        units: option<string>,
        reference_range: option<string>
    };

    type HL7Message = record {
        delimiters: Delimiters,
        header: MessageHeader,
        patient: option<PatientIdentification>,
        observations: list<ObservationResult>
    };

    fn parse_hl7_message(input: &[u8]) -> result<HL7Message, ParseError> {
        unimplemented!()
    }
}