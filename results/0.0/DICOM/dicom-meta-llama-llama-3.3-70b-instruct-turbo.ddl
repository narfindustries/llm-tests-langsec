domain DICOM {
  types {
    UI: string(64);
    SH: string(16);
    LO: string(64);
    ST: string(1024);
    LT: string(10240);
    PN: string(256);
    DA: string(8);
    TM: string(14);
    DT: string(26);
    AS: string(16);
    IS: integer;
    SS: integer;
    SL: integer;
    UL: unsigned integer;
    US: unsigned short;
    OB: bytes;
    OW: bytes;
    OF: bytes;
    SQ: sequence;
    UN: bytes;
  }

  meta-information {
    MediaStorageSOPInstanceUID: UI;
    MediaStorageSOPClassUID: UI;
    ImplementationClassUID: UI;
    TransferSyntaxUID: UI;
    ImplementationVersionName: SH;
    SourceApplicationEntityTitle: AE;
    SendingApplicationEntityTitle: AE;
    ReceivingApplicationEntityTitle: AE;
    PrivateInformation: bytes;
    PrivateInformationCreatorUID: UI;
  }

  module dicom-meta-llama-llama-3 {
    sequence meta {
      item MediaStorageSOPInstanceUID: UI;
      item MediaStorageSOPClassUID: UI;
      item ImplementationClassUID: UI;
      item TransferSyntaxUID: UI;
      item ImplementationVersionName: SH;
      item SourceApplicationEntityTitle: AE;
      item SendingApplicationEntityTitle: AE;
      item ReceivingApplicationEntityTitle: AE;
      item PrivateInformation: bytes;
      item PrivateInformationCreatorUID: UI;
    }
  }
}

domain AE {
  types {
    AE: string(16);
  }
}