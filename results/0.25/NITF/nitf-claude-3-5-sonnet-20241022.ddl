def Main = {
  Sonnet;
}

def Sonnet = {
  Title;
  Author;
  Lines;
}

def Title = {
  $"Title: ";
  @text_line;
}

def Author = {
  $"Author: ";
  @text_line;
}

def Lines = {
  Line[14];
}

def Line = {
  @text_line;
}

def text_line = {
  text_char*;
  $"\n";
}

def text_char = {
  !($ "\n") @uint8;
}