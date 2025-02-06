module hl7v2 {
    enum segment_type {
        MSH, PID, PV1, OBR, OBX
    }

    type delimiters = struct {
        field_separator: char,
        encoding_characters: [char; 4]
    }

    type message_header = struct {
        segment_type: segment_type,
        sending_application: string,
        sending_facility: string,
        receiving_application: string,
        receiving_facility: string,
        timestamp: u64,
        message_type: struct {
            type: [char; 3],
            trigger_event: [char; 3]
        },
        processing_id: char,
        version: [char; 3]
    }

    type person_name = struct {
        last: string,
        first: string,
        middle: option<string>,
        suffix: option<string>
    }

    type address = struct {
        street: string,
        city: string,
        state: string,
        postal_code: string,
        country: option<string>
    }

    type patient = struct {
        id: string,
        name: person_name,
        birth_date: u64,
        gender: char,
        address: address,
        phone_numbers: list<string>
    }

    type observation = struct {
        identifier: string,
        value: string,
        units: string,
        reference_range: option<string>,
        status: option<string>
    }

    type insurance = struct {
        plan_name: string,
        policy_number: string,
        group_number: option<string>,
        subscriber_id: string
    }

    type hl7_message = struct {
        delimiters: delimiters,
        header: message_header,
        patient: patient,
        observations: list<observation>,
        insurance: option<insurance>
    }
}