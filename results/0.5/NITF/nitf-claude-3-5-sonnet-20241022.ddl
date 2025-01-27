def Main = {
    BitStream;
    Sonnet
}

def Sonnet = {
    Title;
    Author;
    Lines;
    DateWritten
}

def Title = {
    $"Title: ";
    @text until $"\n"
}

def Author = {
    $"Author: ";
    @text until $"\n"
}

def Lines = {
    14 * Line
}

def Line = {
    @text until $"\n"
}

def DateWritten = {
    $"Date: ";
    @text until $"\n"
}

def BitStream = {
    @bytes *
}