#!daedalus
message HL7v2 {
    message = segment+;
    segment = msh | pid | pv1 | obr | obx | nte | al1 | dg1 | orc | evn;

    msh = "MSH" field_sep encoding_chars sending_app sending_facility recv_app recv_facility message_datetime security message_type message_control_id processing_id version_id accept_ack_type app_ack_type country_code char_set principal_lang alt_char_set;

    pid = "PID" set_id patient_id patient_id_list alt_patient_id patient_name mother_maiden_name birthdate admin_sex patient_alias race address phone_home phone_business primary_lang marital_status religion patient_acct_num ssn drivers_license mothers_id ethnic_group birthplace multiple_birth birth_order citizenship vet_status nationality death_datetime death_indicator;

    pv1 = "PV1" set_id patient_class assigned_location admission_type preadmit_num prior_location attending_doc referring_doc consulting_doc hospital_service temp_location preadmit_test readmission_ind admit_source ambulatory_status vip_indicator admitting_doc patient_type visit_number financial_class charge_price_ind courtesy_code credit_rating contract_code contract_effective_date contract_amount contract_period interest_code transfer_to_bad_debt transfer_to_bad_debt_date bad_debt_agency_code bad_debt_transfer_amount bad_debt_recovery_amount delete_account delete_account_date discharge_disposition discharge_to_location diet_type servicing_facility bed_status account_status pending_location prior_temp_location admit_datetime discharge_datetime current_patient_balance total_charges total_adjustments total_payments alternate_visit_id visit_indicator;

    obr = "OBR" set_id placer_order_num filler_order_num universal_service_id priority observation_datetime observation_end_datetime collection_volume collector_identifier specimen_action_code relevant_clinical_info;

    obx = "OBX" set_id value_type observation_id observation_sub_id observation_value units reference_range abnormal_flags probability nature_of_abnormal_test observation_result_status;

    nte = "NTE" set_id source_of_comment comment;

    al1 = "AL1" set_id allergen_type_code allergen_code_mnemonic_descr allergy_severity_code allergy_reaction_code identification_date;

    dg1 = "DG1" set_id diagnosis_coding_method diagnosis_code diagnosis_description diagnosis_datetime diagnosis_type;

    orc = "ORC" order_control order_number filler_order_num placer_group_num order_status response_flag quantity_timing parent_order datetime_of_transaction;

    evn = "EVN" event_type_code recorded_datetime planned_event_datetime event_reason_code operator_id event_occurred event_facility;

    field_sep = /\|/;
    encoding_chars = /\^~\\\&/;
    datetime = /[0-9]{14}/;
    string = /[^|\^~\\\&]*/;
    number = /[0-9]+/;
    
    name_components = string ("^" string){0,5};
    address_components = string ("^" string){0,8};
    phone_number = string ("^" string){0,3};
    code_value = string ("^" string){0,2};

    set_id = number;
    sending_app = string;
    sending_facility = string;
    recv_app = string;
    recv_facility = string;
    message_datetime = datetime;
    security = string;
    message_type = string;
    message_control_id = string;
    processing_id = string;
    version_id = string;
    accept_ack_type = string;
    app_ack_type = string;
    country_code = string;
    char_set = string;
    principal_lang = string;
    alt_char_set = string;
}