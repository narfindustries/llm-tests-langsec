specification BitcoinTransaction {
    lexer {
        Name = /[a-zA-Z][a-zA-Z0-9_]*/
        Amount = /[0-9]+(\.[0-9]+)?/
        WS = /[ \t\n\r]+/ -> skip
    }

    type Transaction = {
        sender: Name
        recipient: Name 
        amount: Amount
        timestamp: Timestamp
    }

    type Timestamp = {
        date: Date
        time: Time
    }

    type Date = {
        year: Year 
        month: Month
        day: Day
    }

    type Time = {
        hour: Hour
        minute: Minute
        second: Second
    }

    type Year = /[0-9]{4}/
    type Month = /0[1-9]|1[0-2]/
    type Day = /0[1-9]|[12][0-9]|3[01]/
    type Hour = /[01][0-9]|2[0-3]/
    type Minute = /[0-5][0-9]/
    type Second = /[0-5][0-9]/

    start BitcoinTransactions = Transaction+

    rule Transaction = {
        sender = Name
        "->"
        recipient = Name
        ":"
        amount = Amount
        "BTC"
        "@"
        timestamp = Timestamp
    }

    rule Timestamp = {
        date = Date
        time = Time
    }

    rule Date = {
        year = Year
        "-" 
        month = Month
        "-"
        day = Day
    }

    rule Time = {
        hour = Hour
        ":"
        minute = Minute 
        ":" 
        second = Second
    }
}