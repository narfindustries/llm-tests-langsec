specification DICOM {
  def Main = {
    Header;
    DataSet*;
    $$ end
  }

  def Header = {
    "DICM";
    $$ commit
  }

  def DataSet = {
    Element;
    $$ commit
  }

  def Element = {
    Tag;
    VR?;
    Length;
    Value;
    $$ commit
  }

  def Tag = {
    Group;
    Element;
    $$ commit
  }

  def Group = {
    /[0-9A-Fa-f]{4}/;
    $$ commit
  }

  def Element = {
    /[0-9A-Fa-f]{4}/;
    $$ commit
  }

  def VR = {
    /[A-Z]{2}/;
    $$ commit
  }

  def Length = {
    /[0-9A-Fa-f]{4}/;
    $$ commit
  }

  def Value = {
    /[0-9A-Fa-f]*/;
    $$ commit
  }
}