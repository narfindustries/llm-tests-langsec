#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <sqlite3.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Database connection and error handling
sqlite3 *db = NULL;
char *errMsg = 0;

// Parser for creating database
static HParser *create_database_parser() {
    return h_choice(
        h_sequence(
            h_token_ci("CREATE"),
            h_whitespace(h_whitespace_standard()),
            h_token_ci("DATABASE"),
            h_whitespace(h_whitespace_standard()),
            h_many1(h_alpha()),
            NULL
        ),
        NULL
    );
}

// Parser for inserting data
static HParser *insert_data_parser() {
    return h_sequence(
        h_token_ci("INSERT"),
        h_whitespace(h_whitespace_standard()),
        h_token_ci("INTO"),
        h_whitespace(h_whitespace_standard()),
        h_many1(h_alpha()),
        h_whitespace(h_whitespace_standard()),
        h_token_ci("VALUES"),
        NULL
    );
}

// Main parsing and database operation function
int parse_and_execute(const char *input) {
    HParser *create_parser = create_database_parser();
    HParser *insert_parser = insert_data_parser();
    
    HParseResult *create_result = h_parse(create_parser, (const uint8_t*)input, strlen(input));
    HParseResult *insert_result = h_parse(insert_parser, (const uint8_t*)input, strlen(input));

    if (create_result && create_result->ast) {
        // Database creation logic
        int rc = sqlite3_open("example.db", &db);
        if (rc) {
            fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
            return -1;
        }
        return 0;
    }

    if (insert_result && insert_result->ast) {
        // Insert data logic
        const char *sql = "INSERT INTO table_name (column) VALUES ('value')";
        rc = sqlite3_exec(db, sql, 0, 0, &errMsg);
        if (rc != SQLITE_OK) {
            fprintf(stderr, "SQL error: %s\n", errMsg);
            sqlite3_free(errMsg);
            return -1;
        }
        return 0;
    }

    return -1;
}

int main() {
    const char *test_input = "CREATE DATABASE mydb";
    int result = parse_and_execute(test_input);

    if (db) {
        sqlite3_close(db);
    }

    return result;
}