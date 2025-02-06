data DICOM = DICOM { metaHeader :: Maybe MetaHeader, dataElements :: [DataElement] }

data MetaHeader = MetaHeader { dicm :: Maybe String, metaGroupLength :: Maybe Integer, fileMetaInformationVersion :: Maybe (Integer, Integer), mediaStorageSOPClassUID :: Maybe String, mediaStorageSOPInstanceUID :: Maybe String, transferSyntaxUID :: Maybe String, implementationClassUID :: Maybe String, implementationVersionName :: Maybe String, sourceApplicationEntityTitle :: Maybe String, sendingApplicationEntityTitle :: Maybe String, receivingApplicationEntityTitle :: Maybe String, privateInformationCreatorUID :: Maybe String, privateInformation :: Maybe [DataElement] }

data DataElement = DataElement { tag :: (Integer, Integer), vr :: String, value :: Maybe Value }

data Value = StringValue String | IntegerValue Integer | FloatValue Float | SequenceValue [DataElement] | OtherValue ByteString | DateValue Date | TimeValue Time | PersonNameValue PersonName | CodeStringValue String | UniqueIdentifierValue String | ShortStringValue String | LongStringValue String | LongTextValue String | ShortTextValue String | UnsignedShortValue Integer | UnsignedLongValue Integer | SignedShortValue Integer | SignedLongValue Integer | DecimalStringValue String

data PersonName = PersonName { familyName :: Maybe String, givenName :: Maybe String, middleName :: Maybe String, prefix :: Maybe String, suffix :: Maybe String }
