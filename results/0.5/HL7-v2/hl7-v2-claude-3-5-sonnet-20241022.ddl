struct HL7Message {
  MSH: {
    field_separator: u8 = '|';
    encoding_chars: u8[4] = '^~\\&';
    sending_app: string;
    sending_facility: string;
    receiving_app: string;
    receiving_facility: string;
    datetime: string;
    security: string;
    message_type: string;
    message_control_id: string;
    processing_id: string;
    version_id: string;
    sequence_number: string;
    continuation_pointer: string;
    accept_ack_type: string;
    application_ack_type: string;
    country_code: string;
    character_set: string;
  };

  EVN?: {
    event_type_code: string;
    recorded_datetime: string;
    planned_datetime: string;
    event_reason_code: string;
    operator_id: string;
    event_occurred: string;
  };

  PID?: {
    set_id: string;
    patient_id: string;
    patient_identifier_list: string;
    alternate_patient_id: string;
    patient_name: string;
    mother_maiden_name: string;
    datetime_of_birth: string;
    administrative_sex: string;
    patient_alias: string;
    race: string;
    patient_address: string;
    county_code: string;
    phone_number_home: string;
    phone_number_business: string;
    primary_language: string;
    marital_status: string;
    religion: string;
    patient_account_number: string;
    ssn_number: string;
    drivers_license_number: string;
    mothers_identifier: string;
    ethnic_group: string;
    birth_place: string;
    multiple_birth_indicator: string;
    birth_order: string;
    citizenship: string;
    veterans_military_status: string;
    nationality: string;
    patient_death_date_time: string;
    patient_death_indicator: string;
  };

  PV1?: {
    set_id: string;
    patient_class: string;
    assigned_patient_location: string;
    admission_type: string;
    preadmit_number: string;
    prior_patient_location: string;
    attending_doctor: string;
    referring_doctor: string;
    consulting_doctor: string;
    hospital_service: string;
    temporary_location: string;
    preadmit_test_indicator: string;
    readmission_indicator: string;
    admit_source: string;
    ambulatory_status: string;
    vip_indicator: string;
    admitting_doctor: string;
    patient_type: string;
    visit_number: string;
    financial_class: string;
    charge_price_indicator: string;
    courtesy_code: string;
    credit_rating: string;
    contract_code: string;
    contract_effective_date: string;
    contract_amount: string;
    contract_period: string;
    interest_code: string;
    transfer_to_bad_debt_code: string;
    transfer_to_bad_debt_date: string;
    bad_debt_agency_code: string;
    bad_debt_transfer_amount: string;
    bad_debt_recovery_amount: string;
    delete_account_indicator: string;
    delete_account_date: string;
    discharge_disposition: string;
    discharged_to_location: string;
    diet_type: string;
    servicing_facility: string;
    bed_status: string;
    account_status: string;
    pending_location: string;
    prior_temporary_location: string;
    admit_datetime: string;
    discharge_datetime: string;
    current_patient_balance: string;
    total_charges: string;
    total_adjustments: string;
    total_payments: string;
    alternate_visit_id: string;
    visit_indicator: string;
    other_healthcare_provider: string;
  };
}