HL7v2 {
    MSH {
        separator: char;
        encoding_characters: string;
        sending_application: string;
        sending_facility: string;
        receiving_application: string;
        receiving_facility: string;
        datetime_of_message: string;
        security: string optional;
        message_type: string;
        message_control_id: string;
        processing_id: string;
        version_id: string;
        sequence_number: int optional;
        continuation_pointer: string optional;
        accept_acknowledgment_type: string optional;
        application_acknowledgment_type: string optional;
        country_code: string optional;
        character_set: string optional;
        principal_language_of_message: string optional;
    }

    PID {
        set_id: int optional;
        patient_id: string optional;
        patient_identifier_list: string;
        alternate_patient_id: string optional;
        patient_name: string;
        mother's_maiden_name: string optional;
        datetime_of_birth: string;
        administrative_sex: string optional;
        patient_alias: string optional;
        race: string optional;
        patient_address: string optional;
        county_code: string optional;
        phone_number_home: string optional;
        phone_number_business: string optional;
        primary_language: string optional;
        marital_status: string optional;
        religion: string optional;
        patient_account_number: string optional;
        ssn_number_patient: string optional;
        driver's_license_number_patient: string optional;
        mother's_identifier: string optional;
    }

    OBR {
        set_id: int optional;
        placer_order_number: string optional;
        filler_order_number: string optional;
        universal_service_identifier: string;
        priority: string optional;
        requested_datetime: string optional;
        observation_datetime: string optional;
        observation_end_datetime: string optional;
        collection_volume: string optional;
        collector_identifier: string optional;
        specimen_action_code: string optional;
        danger_code: string optional;
        relevant_clinical_information: string optional;
        specimen_received_datetime: string optional;
        specimen_source: string optional;
        ordering_provider: string optional;
        order_callback_phone_number: string optional;
        placer_field_1: string optional;
        placer_field_2: string optional;
        filler_field_1: string optional;
        filler_field_2: string optional;
        results_rpt_status_chng_datetime: string optional;
        charge_to_practice: string optional;
        diagnostic_serv_sect_id: string optional;
        result_status: string optional;
    }

    OBX {
        set_id: int optional;
        value_type: string;
        observation_identifier: string;
        observation_sub_id: string optional;
        observation_value: string;
        units: string optional;
        references_range: string optional;
        abnormal_flags: string optional;
        probability: string optional;
        nature_of_abnormal_test: string optional;
        observation_result_status: string optional;
        effective_date_of_reference_range: string optional;
        user_defined_access_checks: string optional;
        datetime_of_the_observation: string optional;
        producer_id: string optional;
        responsible_observer: string optional;
    }

    AL1 {
        set_id: int optional;
        allergen_type_code: string;
        allergen_code_description: string;
        allergy_severity_code: string optional;
        allergy_reaction_code: string optional;
        identification_date: string optional;
    }
}