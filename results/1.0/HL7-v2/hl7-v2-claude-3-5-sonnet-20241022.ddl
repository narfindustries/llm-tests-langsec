grammar HL7v2;

MSH = "MSH|" EncodingChars SendingApp SendingFacility ReceivingApp ReceivingFacility DateTime Security MsgType MsgControlId ProcessingId VersionId [SequenceNum] [ContinuationPtr] [AcceptAckType] [AppAckType] [CountryCode] [Charset];

EncodingChars = /[^|]{4}/;
SendingApp = /[^|]*/;
SendingFacility = /[^|]*/;
ReceivingApp = /[^|]*/;
ReceivingFacility = /[^|]*/;
DateTime = /\d{8}(\d{6})?/;
Security = /[^|]*/;
MsgType = /[^|]*/;
MsgControlId = /[^|]*/;
ProcessingId = /[^|]*/;
VersionId = /[^|]*/;
SequenceNum = /[^|]*/;
ContinuationPtr = /[^|]*/;
AcceptAckType = /[^|]*/;
AppAckType = /[^|]*/;
CountryCode = /[^|]*/;
Charset = /[^|]*/;

PID = "PID|" SetId PatientId PatientIdList AlternateId PatientName MotherMaidenName DOB Sex PatientAlias Race Address County PhoneHome PhoneBusiness PrimaryLanguage MaritalStatus Religion PatientAcctNum SSN DriversLicense MothersId EthnicGroup BirthPlace MultipleBirth BirthOrder Citizenship VeteransStatus Nationality DeathDate DeathIndicator;

SetId = /[^|]*/;
PatientId = /[^|]*/;
PatientIdList = /[^|]*/;
AlternateId = /[^|]*/;
PatientName = /[^|]*/;
MotherMaidenName = /[^|]*/;
DOB = /[^|]*/;
Sex = /[^|]*/;
PatientAlias = /[^|]*/;
Race = /[^|]*/;
Address = /[^|]*/;
County = /[^|]*/;
PhoneHome = /[^|]*/;
PhoneBusiness = /[^|]*/;
PrimaryLanguage = /[^|]*/;
MaritalStatus = /[^|]*/;
Religion = /[^|]*/;
PatientAcctNum = /[^|]*/;
SSN = /[^|]*/;
DriversLicense = /[^|]*/;
MothersId = /[^|]*/;
EthnicGroup = /[^|]*/;
BirthPlace = /[^|]*/;
MultipleBirth = /[^|]*/;
BirthOrder = /[^|]*/;
Citizenship = /[^|]*/;
VeteransStatus = /[^|]*/;
Nationality = /[^|]*/;
DeathDate = /[^|]*/;
DeathIndicator = /[^|]*/;

PV1 = "PV1|" SetId PatientClass AssignedLoc AdmissionType PreadmitNum PriorLoc AttendingDoc ReferringDoc ConsultingDoc HospitalService TempLoc PreadmitTestInd ReadmissionInd AdmitSource AmbulatoryStatus VipIndicator AdmittingDoc PatientType VisitNumber FinancialClass ChargePriceInd CourtesyCode CreditRating ContractCode ContractEffective ContractAmount ContractPeriod InterestCode;

PatientClass = /[^|]*/;
AssignedLoc = /[^|]*/;
AdmissionType = /[^|]*/;
PreadmitNum = /[^|]*/;
PriorLoc = /[^|]*/;
AttendingDoc = /[^|]*/;
ReferringDoc = /[^|]*/;
ConsultingDoc = /[^|]*/;
HospitalService = /[^|]*/;
TempLoc = /[^|]*/;
PreadmitTestInd = /[^|]*/;
ReadmissionInd = /[^|]*/;
AdmitSource = /[^|]*/;
AmbulatoryStatus = /[^|]*/;
VipIndicator = /[^|]*/;
AdmittingDoc = /[^|]*/;
PatientType = /[^|]*/;
VisitNumber = /[^|]*/;
FinancialClass = /[^|]*/;
ChargePriceInd = /[^|]*/;
CourtesyCode = /[^|]*/;
CreditRating = /[^|]*/;
ContractCode = /[^|]*/;
ContractEffective = /[^|]*/;
ContractAmount = /[^|]*/;
ContractPeriod = /[^|]*/;
InterestCode = /[^|]*/;

OBR = "OBR|" SetId PlacerOrderNum FillerOrderNum UniversalServiceId Priority RequestedDate ObservationDate ObservationEndDate CollectionVolume CollectorIdentifier SpecimenActionCode DangerCode RelevantClinicalInfo SpecimenReceivedDate SpecimenSource OrderingProvider OrderCallbackPhone;

PlacerOrderNum = /[^|]*/;
FillerOrderNum = /[^|]*/;
UniversalServiceId = /[^|]*/;
Priority = /[^|]*/;
RequestedDate = /[^|]*/;
ObservationDate = /[^|]*/;
ObservationEndDate = /[^|]*/;
CollectionVolume = /[^|]*/;
CollectorIdentifier = /[^|]*/;
SpecimenActionCode = /[^|]*/;
DangerCode = /[^|]*/;
RelevantClinicalInfo = /[^|]*/;
SpecimenReceivedDate = /[^|]*/;
SpecimenSource = /[^|]*/;
OrderingProvider = /[^|]*/;
OrderCallbackPhone = /[^|]*/;

OBX = "OBX|" SetId ValueType ObservationId SubId ObservationValue Units References AbnormalFlags Probability NatureOfAbnormal ObservationStatus EffectiveDate UserDefinedAccess DateTime ProducerId ResponsibleObserver ObservationMethod;

ValueType = /[^|]*/;
ObservationId = /[^|]*/;
SubId = /[^|]*/;
ObservationValue = /[^|]*/;
Units = /[^|]*/;
References = /[^|]*/;
AbnormalFlags = /[^|]*/;
Probability = /[^|]*/;
NatureOfAbnormal = /[^|]*/;
ObservationStatus = /[^|]*/;
EffectiveDate = /[^|]*/;
UserDefinedAccess = /[^|]*/;
ProducerId = /[^|]*/;
ResponsibleObserver = /[^|]*/;
ObservationMethod = /[^|]*/;

Message = MSH PID? PV1? OBR* OBX*;

start = Message;