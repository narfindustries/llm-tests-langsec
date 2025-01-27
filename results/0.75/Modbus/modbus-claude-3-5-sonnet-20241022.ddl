format Modbus {
  def Main = { Header TCP_Data }

  def Header = {
    transaction_id : uint16
    protocol_id : uint16
    length : uint16
    unit_id : uint8
  }

  def TCP_Data = {
    function_code : uint8
    $function_code == 0x03 | $function_code == 0x04 => ReadHoldingRegisters |
    $function_code == 0x10 => WriteMultipleRegisters
  }

  def ReadHoldingRegisters = {
    byte_count : uint8
    @byte_count / 2 registers : uint16[]
  }

  def WriteMultipleRegisters = {
    start_address : uint16
    register_count : uint16
    byte_count : uint8
    @register_count values : uint16[]
  }
}