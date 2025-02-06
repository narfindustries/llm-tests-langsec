syntax = "HL7v2"
structure = "message"

segment MSH {
  field FS {
    type = "string"
    length = 1
    value = "|"
  }
  field EncodingCharacters {
    type = "string"
    length = 3
    value = "^~\&"
  }
  field SendingFacility {
    type = "string"
    length = "variable"
  }
  field SendingFacilityNamespaceID {
    type = "string"
    length = "variable"
  }
  field ReceivingFacility {
    type = "string"
    length = "variable"
  }
  field ReceivingFacilityNamespaceID {
    type = "string"
    length = "variable"
  }
  field DateTimeOfMessage {
    type = "datetime"
    format = "YYYYMMDDHHMMSS"
  }
  field Security {
    type = "string"
    length = "variable"
  }
  field MessageType {
    type = "string"
    length = "variable"
  }
  field MessageControlID {
    type = "string"
    length = "variable"
  }
  field ProcessingID {
    type = "string"
    length = 1
    value = ["P", "T", "D"]
  }
  field VersionID {
    type = "string"
    length = "variable"
  }
}

segment PID {
  field SetIDPatientID {
    type = "string"
    length = "variable"
  }
  field PatientID {
    type = "string"
    length = "variable"
  }
  field PatientIDIdentifier {
    type = "string"
    length = "variable"
  }
  field AlternatePatientID {
    type = "string"
    length = "variable"
  }
  field PatientName {
    type = "string"
    length = "variable"
  }
  field MothersMaidenName {
    type = "string"
    length = "variable"
  }
  field DateOfBirth {
    type = "date"
    format = "YYYYMMDD"
  }
  field Sex {
    type = "string"
    length = 1
    value = ["M", "F", "U", "A", "N", "O"]
  }
  field PatientAlias {
    type = "string"
    length = "variable"
  }
  field Race {
    type = "string"
    length = "variable"
  }
  field PatientAddress {
    type = "string"
    length = "variable"
  }
  field CountyCode {
    type = "string"
    length = "variable"
  }
  field PhoneNumberHome {
    type = "string"
    length = "variable"
  }
  field PhoneNumberBusiness {
    type = "string"
    length = "variable"
  }
}

segment PV1 {
  field SetIDPatientVisit {
    type = "string"
    length = "variable"
  }
  field VisitNumber {
    type = "string"
    length = "variable"
  }
  field VisitIndicator {
    type = "string"
    length = 1
    value = ["I", "O", "E"]
  }
  field AdmissionType {
    type = "string"
    length = "variable"
  }
  field PreAdmitNumber {
    type = "string"
    length = "variable"
  }
  field PriorPatientLocation {
    type = "string"
    length = "variable"
  }
  field AttendingDoctor {
    type = "string"
    length = "variable"
  }
  field ReferringDoctor {
    type = "string"
    length = "variable"
  }
  field ConsultingDoctor {
    type = "string"
    length = "variable"
  }
  field HospitalService {
    type = "string"
    length = "variable"
  }
}

segment ORC {
  field OrderControl {
    type = "string"
    length = 2
    value = ["NW", "OK", "CA"]
  }
  field OrderControlIdentifier {
    type = "string"
    length = "variable"
  }
  field OrderControlPriorValue {
    type = "string"
    length = "variable"
  }
  field OrderControlPlacerIdentifier {
    type = "string"
    length = "variable"
  }
  field OrderControlFillerIdentifier {
    type = "string"
    length = "variable"
  }
}

segment OBR {
  field SetIDObservationRequest {
    type = "string"
    length = "variable"
  }
  field PlacerOrderNumber {
    type = "string"
    length = "variable"
  }
  field FillerOrderNumber {
    type = "string"
    length = "variable"
  }
  field UniversalServiceIdentifier {
    type = "string"
    length = "variable"
  }
  field Priority {
    type = "string"
    length = "variable"
    value = ["STAT", "ASAP", "RUSH"]
  }
}

segment OBX {
  field SetIDObservationResult {
    type = "string"
    length = "variable"
  }
  field ValueType {
    type = "string"
    length = 2
    value = ["NM", "TX", "CE"]
  }
  field ObservationResult {
    type = "string"
    length = "variable"
  }
  field Units {
    type = "string"
    length = "variable"
  }
  field ReferenceRange {
    type = "string"
    length = "variable"
  }
}

segment OCD {
  field SetIDObservationComponent {
    type = "string"
    length = "variable"
  }
  field ObservationComponentIdentifier {
    type = "string"
    length = "variable"
  }
  field ObservationComponentValue {
    type = "string"
    length = "variable"
  }
}