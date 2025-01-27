#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_sqlite3_db_parser() {
    // Basic tokens
    HParser *whitespace = h_whitespace(h_many1);
    HParser *identifier = h_many1(h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), 
                                         h_ch_range('0', '9'), h_ch('_'), NULL));
    
    // Keywords
    HParser *create = h_token("CREATE", strlen("CREATE"));
    HParser *table = h_token("TABLE", strlen("TABLE"));
    HParser *insert = h_token("INSERT", strlen("INSERT"));
    HParser *into = h_token("INTO", strlen("INTO"));
    HParser *values = h_token("VALUES", strlen("VALUES"));
    HParser *select = h_token("SELECT", strlen("SELECT"));
    HParser *from = h_token("FROM", strlen("FROM"));
    HParser *where = h_token("WHERE", strlen("WHERE"));
    
    // Symbols
    HParser *semicolon = h_ch(';');
    HParser *comma = h_ch(',');
    HParser *lparen = h_ch('(');
    HParser *rparen = h_ch(')');
    HParser *equals = h_ch('=');
    HParser *star = h_ch('*');
    
    // Data types
    HParser *integer = h_token("INTEGER", strlen("INTEGER"));
    HParser *text = h_token("TEXT", strlen("TEXT"));
    HParser *datatype = h_choice(integer, text, NULL);
    
    // Column definition
    HParser *column_def = h_sequence(identifier, whitespace, datatype, NULL);
    HParser *column_list = h_sepBy1(column_def, h_sequence(comma, whitespace, NULL));
    
    // Values
    HParser *string_literal = h_in_quotes(h_many1(h_choice(h_ch_range('a', 'z'), 
                                                         h_ch_range('A', 'Z'),
                                                         h_ch_range('0', '9'),
                                                         h_ch('_'), h_ch(' '), NULL)), '\'');
    HParser *number = h_many1(h_ch_range('0', '9'));
    HParser *value = h_choice(string_literal, number, NULL);
    HParser *value_list = h_sepBy1(value, h_sequence(comma, whitespace, NULL));
    
    // Statements
    HParser *create_stmt = h_sequence(create, whitespace, table, whitespace, identifier,
                                    whitespace, lparen, column_list, rparen, semicolon, NULL);
    
    HParser *insert_stmt = h_sequence(insert, whitespace, into, whitespace, identifier,
                                    whitespace, lparen, h_sepBy1(identifier, h_sequence(comma, whitespace, NULL)),
                                    rparen, whitespace, values, whitespace, lparen, value_list,
                                    rparen, semicolon, NULL);
    
    HParser *select_stmt = h_sequence(select, whitespace, 
                                    h_choice(star, h_sepBy1(identifier, h_sequence(comma, whitespace, NULL)), NULL),
                                    whitespace, from, whitespace, identifier,
                                    h_optional(h_sequence(whitespace, where, whitespace, 
                                                       identifier, equals, value, NULL)),
                                    semicolon, NULL);
    
    // Complete SQL parser
    return h_choice(create_stmt, insert_stmt, select_stmt, NULL);
}

HParser* sqlite3_db_parser = NULL;

void init_parser() {
    sqlite3_db_parser = init_sqlite3_db_parser();
}