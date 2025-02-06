HL7v2Message = { segments: Segment+ }

Segment = MSH | PID | PV1 // Add other segments as needed

MSH = {
  field_separator: ST,
  encoding_characters: ST,
  sending_application: HD,
  sending_facility: HD,
  receiving_application: HD,
  receiving_facility: HD,
  date_time_of_message: DTM,
  security: ST?,
  message_type: MSG,
  message_control_id: ST,
  processing_id: PT,
  version_id: VID,
  sequence_number: NM?,
  continuation_pointer: ST?,
  accept_acknowledgment_type: ID?,
  application_acknowledgment_type: ID?,
  country_code: ID?,
  character_set: ID?,
  principal_language_of_message: CE?
}

PID = {
  set_id: SI,
  patient_id: CX?,
  patient_identifier_list: CX+,
  alternate_patient_id: CX?,
  patient_name: XPN+,
  mother_maiden_name: XPN?,
  date_time_of_birth: DTM?,
  sex: IS?,
  patient_alias: XPN?,
  race: CE?,
  patient_address: XAD?,
  county_code: IS?,
  phone_number_home: XTN?,
  phone_number_business: XTN?,
  primary_language: CE?,
  marital_status: CE?,
  religion: CE?,
  patient_account_number: CX?,
  ssn_number: ST?,
  driver_license_number: DLN?,
  mother_identifier: CX?,
  ethnic_group: CE?,
  birth_place: ST?,
  multiple_birth_indicator: ID?,
  birth_order: NM?,
  citizenship: CE?,
  veterans_military_status: CE?,
  nationality: CE?,
  patient_death_date_and_time: DTM?,
  patient_death_indicator: ID?,
  identity_unknown_indicator: ID?,
  identity_reliability_code: IS?,
  last_update_date_time: DTM?,
  last_update_facility: HD?,
  species_code: CE?,
  breed_code: CE?,
  strain: ST?,
  production_class_code: CE?,
  tribal_citizenship: CE?,
  patient_telecommunication_information: XTN?
}

PV1 = {
  set_id: SI,
  patient_class: IS,
  assigned_patient_location: PL?,
  admission_type: IS?,
  preadmit_number: CX?,
  prior_patient_location: PL?,
  attending_doctor: XCN?,
  referring_doctor: XCN?,
  consulting_doctor: XCN?,
  hospital_service: IS?,
  temporary_location: PL?,
  preadmit_test_indicator: IS?,
  re_admission_indicator: IS?,
  admit_source: IS?,
  ambulatory_status: IS?,
  vip_indicator: IS?,
  admitting_doctor: XCN?,
  patient_type: IS?,
  visit_number: CX?,
  financial_class: FC?,
  charge_price_indicator: IS?,
  courtesy_code: IS?,
  credit_rating: IS?,
  contract_code: IS?,
  contract_effective_date: DT?,
  contract_amount: NM?,
  contract_period: NM?,
  interest_code: IS?,
  transfer_to_bad_debt_code: IS?,
  transfer_to_bad_debt_date: DT?,
  bad_debt_agency_code: IS?,
  bad_debt_transfer_amount: NM?,
  bad_debt_recovery_amount: NM?,
  delete_account_indicator: IS?,
  delete_account_date: DT?,
  discharge_disposition: IS?,
  discharged_to_location: DLD?,
  diet_type: CE?,
  servicing_facility: IS?,
  bed_status: IS?,
  account_status: IS?,
  pending_location: PL?,
  prior_temporary_location: PL?,
  admit_date_time: DTM?,
  discharge_date_time: DTM?,
  current_patient_balance: NM?,
  total_charges: NM?,
  total_adjustments: NM?,
  total_payments: NM?,
  alternate_visit_id: CX?,
  visit_indicator: IS?,
  other_healthcare_provider: XCN?,
  service_episode_description: ST?,
  service_episode_identifier: CX?
}

ST = String
HD = HierarchicDesignator
DTM = DateTime
MSG = MessageType
PT = ProcessingType
VID = VersionID
NM = Numeric
ID = CodedValue
CE = CodedElement
SI = SequenceID
CX = ExtendedCompositeID
XPN = ExtendedPersonName
XAD = ExtendedAddress
XTN = ExtendedTelecommunicationNumber
DLN = DriverLicenseNumber
PL = PointOfCare
FC = FinancialClass
DT = Date
DLD = DischargeLocation
IS = CodedValueForUserDefinedTables