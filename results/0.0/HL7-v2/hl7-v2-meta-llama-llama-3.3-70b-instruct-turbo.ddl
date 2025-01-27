domain HL7v2 {
  types {
    byte: uint8
    word: uint16
    dword: uint32
    field: string(256)
    segment: string(3)
  }

  grammar {
    HL7Message: Segment*;
    Segment: 
      [seg_id: segment]
      [fields: field*];
  }

  rules {
    MS_ADT_A01: 
      [seg_id: "MSH"]
      [fields: 
        [field_sep: field]
        [encoding_chars: field]
        [sending_fac: field]
        [sending_app: field]
        [msg_date: field]
        [msg_time: field]
        [security: field]
        [msg_type: field]
        [msg_ctrl_id: field]
        [proc_id: field]
        [version_id: field]
        [seq_num: field]
        [cont_ptr: field]
        [accept_ack_type: field]
        [app_ack_type: field]
        [country_code: field]
        [char_set: field]
        [principle_language: field]
      ]
      [seg_id: "EVN"]
      [fields: 
        [event_type_code: field]
        [recorded_date: field]
        [recorded_time: field]
        [event_reason_code: field]
        [operator_id: field]
      ]
      [seg_id: "PID"]
      [fields: 
        [patient_id: field]
        [patient_id_visit: field]
        [alt_patient_id: field]
        [patient_name: field]
        [mother_maiden_name: field]
        [date_of_birth: field]
        [sex: field]
        [patient_alias: field]
        [race: field]
        [patient_address: field]
        [county_code: field]
        [phone_number_home: field]
        [phone_number_business: field]
        [primary_language: field]
        [marital_status: field]
        [religion: field]
        [patient_account_number: field]
        [ssn: field]
        [driver_s_license: field]
        [mothers_identifier: field]
      ]
      [seg_id: "PV1"]
      [fields: 
        [visit_number: field]
        [patient_class: field]
        [visit_number_other: field]
        [admitting_doctor: field]
        [visit_type: field]
        [admit_date: field]
        [admit_time: field]
        [discharge_disposition: field]
        [discharge_date: field]
        [discharge_time: field]
        [visit_description: field]
      ];
  }
}