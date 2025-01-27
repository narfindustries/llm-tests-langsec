def Main = Grammar {
  main = HL7Message
  
  HL7Message = {
    segments: @+SegmentDef
  }

  SegmentDef = {
    MSH |
    EVN |
    PID |
    PV1 |
    OBR |
    OBX
  }

  MSH = @{
    'MSH' ^'|' field_sep:@'|' encoding_chars:@'~\\&' sending_app:@Text '|'
    sending_facility:@Text '|' receiving_app:@Text '|' receiving_facility:@Text '|'
    datetime:@Datetime '|' security:@Text '|' message_type:@MessageType '|'
    message_control_id:@Text '|' processing_id:@Text '|' version_id:@Text
  }

  EVN = @{
    'EVN' '|' event_type:@Text '|' datetime:@Datetime '|' planned_datetime:@Datetime? '|'
    event_reason:@Text? '|' operator_id:@Text? '|' event_occurred:@Datetime?
  }

  PID = @{
    'PID' '|' set_id:@Number? '|' patient_id:@Text '|' patient_id_list:@Text? '|'
    alt_patient_id:@Text? '|' patient_name:@PersonName '|' mother_maiden_name:@Text? '|'
    birth_date:@Date? '|' sex:@('M'|'F'|'O'|'U') '|' patient_alias:@Text? '|'
    race:@Text? '|' address:@Address? '|' county_code:@Text?
  }

  PV1 = @{
    'PV1' '|' set_id:@Number? '|' patient_class:@Text '|' assigned_location:@Text? '|'
    admission_type:@Text? '|' preadmit_number:@Text? '|' prior_location:@Text? '|'
    attending_doctor:@Text? '|' referring_doctor:@Text? '|' consulting_doctor:@Text? '|'
    hospital_service:@Text?
  }

  OBR = @{
    'OBR' '|' set_id:@Number? '|' placer_order_number:@Text? '|' 
    filler_order_number:@Text? '|' universal_service_id:@Text '|'
    priority:@Text? '|' requested_datetime:@Datetime? '|'
    observation_datetime:@Datetime? '|' observation_end_datetime:@Datetime?
  }

  OBX = @{
    'OBX' '|' set_id:@Number? '|' value_type:@Text '|' observation_id:@Text '|'
    observation_sub_id:@Text? '|' observation_value:@Text '|' units:@Text? '|'
    reference_range:@Text? '|' abnormal_flags:@Text? '|' probability:@Number? '|'
    nature_of_abnormal_test:@Text?
  }

  MessageType = @{
    message_code:@Text '^' trigger_event:@Text '^' message_structure:@Text
  }

  PersonName = @{
    family_name:@Text '^' given_name:@Text ('^' middle_name:@Text)? 
    ('^' suffix:@Text)? ('^' prefix:@Text)?
  }

  Address = @{
    street:@Text '^' other_designation:@Text? '^' city:@Text '^' 
    state:@Text '^' zip:@Text '^' country:@Text?
  }

  Number = @/[0-9]+/
  Text = @/[^|^~\\&\r\n]*/
  Date = @/[0-9]{8}/
  Time = @/[0-9]{6}/
  Datetime = @{ @Date @Time }
}