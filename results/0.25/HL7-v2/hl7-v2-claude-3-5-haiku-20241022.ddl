module HL7V2 {
    enum MessageType {
        ADT,
        ORM,
        ORU,
        RDE
    }

    type Separator = u8

    type Delimiters = {
        field: Separator,
        component: Separator,
        repetition: Separator,
        escape: Separator,
        subcomponent: Separator
    }

    type Timestamp = {
        year: u16,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
        second: u8
    }

    type PersonName = {
        family_name: string,
        given_name: string,
        middle_name: optional string,
        suffix: optional string,
        prefix: optional string
    }

    type Address = {
        street: string,
        city: string,
        state: string,
        postal_code: string,
        country: optional string
    }

    type MessageHeader = {
        sending_application: string,
        sending_facility: string,
        receiving_application: string,
        receiving_facility: string,
        timestamp: Timestamp,
        message_type: MessageType,
        message_control_id: string,
        processing_id: string,
        version: string
    }

    type PatientIdentification = {
        patient_id: string,
        patient_name: PersonName,
        date_of_birth: Timestamp,
        gender: string,
        address: optional Address,
        phone_number: optional string
    }

    type PatientVisit = {
        visit_number: string,
        patient_class: string,
        assigned_location: {
            ward: string,
            room: string,
            bed: string
        }
    }

    type ObservationResult = {
        identifier: string,
        value: string,
        units: optional string,
        reference_range: optional string,
        status: string,
        timestamp: Timestamp
    }

    type Message = {
        delimiters: Delimiters,
        header: MessageHeader,
        patient: PatientIdentification,
        visit: optional PatientVisit,
        observations: optional [ObservationResult]
    }
}