specification DICOM {
  def Main = { header PDU* }

  def header = {
    "DICM" Version
  }

  def Version = {
    uint8 uint8
  }

  def PDU = {
    PDUType PDULength PDUContent
  }

  def PDUType = {
    uint8
  }

  def PDULength = {
    uint32
  }

  def PDUContent = {
    | AssociateRQ   when $PDUType == 1
    | AssociateAC   when $PDUType == 2
    | AssociateRJ   when $PDUType == 3
    | PDataTF       when $PDUType == 4
    | ReleaseRQ     when $PDUType == 5
    | ReleaseRP     when $PDUType == 6
    | AbortRQ       when $PDUType == 7
  }

  def AssociateRQ = {
    ProtocolVersion Reserved ApplicationContext
    PresentationContexts UserInfo
  }

  def AssociateAC = {
    ProtocolVersion Reserved ApplicationContext
    PresentationContexts UserInfo
  }

  def AssociateRJ = {
    Reserved Result Source
  }

  def PDataTF = {
    PDVItem+
  }

  def ReleaseRQ = {
    Reserved
  }

  def ReleaseRP = {
    Reserved
  }

  def AbortRQ = {
    Reserved Source Reason
  }

  def ProtocolVersion = {
    uint16
  }

  def Reserved = {
    uint16
  }

  def ApplicationContext = {
    ItemType ItemLength ItemValue
  }

  def PresentationContexts = {
    PresentationContext*
  }

  def PresentationContext = {
    ItemType ItemLength ContextID Reserved
    AbstractSyntax TransferSyntaxes
  }

  def UserInfo = {
    ItemType ItemLength UserInfoItems
  }

  def UserInfoItems = {
    UserInfoItem*
  }

  def UserInfoItem = {
    ItemType ItemLength ItemValue
  }

  def PDVItem = {
    PDVLength PDVValue
  }

  def PDVLength = {
    uint32
  }

  def PDVValue = {
    byte{$PDVLength}
  }

  def ItemType = {
    uint8
  }

  def ItemLength = {
    uint16
  }

  def ItemValue = {
    byte{$ItemLength}
  }

  def ContextID = {
    uint8
  }

  def AbstractSyntax = {
    ItemType ItemLength ItemValue
  }

  def TransferSyntaxes = {
    TransferSyntax*
  }

  def TransferSyntax = {
    ItemType ItemLength ItemValue
  }

  def Source = {
    uint8
  }

  def Result = {
    uint8
  }

  def Reason = {
    uint8
  }
}