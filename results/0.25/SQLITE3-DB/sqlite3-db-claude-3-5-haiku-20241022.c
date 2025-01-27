#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <sqlite3.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Database connection and error handling structure
typedef struct {
    sqlite3 *db;
    char *error_message;
} DatabaseContext;

// Parser for creating database
static HParser *create_database_parser() {
    return h_choice(
        h_sequence(
            h_token_ci("CREATE"),
            h_whitespace(),
            h_token_ci("DATABASE"),
            h_whitespace(),
            h_many1(h_alpha()),
            NULL
        ),
        NULL
    );
}

// Parser for inserting data
static HParser *create_insert_parser() {
    return h_sequence(
        h_token_ci("INSERT"),
        h_whitespace(),
        h_token_ci("INTO"),
        h_whitespace(),
        h_many1(h_alpha()),
        h_whitespace(),
        h_token_ci("VALUES"),
        h_whitespace(),
        h_token("("),
        h_many1(h_choice(h_int(), h_quoted_string())),
        h_token(")"),
        NULL
    );
}

// Callback for database operations
static int sqlite_callback(void *data, int argc, char **argv, char **col_names) {
    DatabaseContext *context = (DatabaseContext *)data;
    for (int i = 0; i < argc; i++) {
        printf("%s = %s\n", col_names[i], argv[i] ? argv[i] : "NULL");
    }
    return 0;
}

// Execute SQLite operation
static int execute_sqlite_operation(DatabaseContext *context, const char *sql) {
    char *error = NULL;
    int rc = sqlite3_exec(context->db, sql, sqlite_callback, context, &error);
    
    if (rc != SQLITE_OK) {
        context->error_message = strdup(error);
        sqlite3_free(error);
        return -1;
    }
    return 0;
}

// Main parsing and database interaction function
int main(int argc, char **argv) {
    // Initialize Hammer
    h_init();

    // Create parsers
    HParser *create_db_parser = create_database_parser();
    HParser *insert_parser = create_insert_parser();

    // Database context
    DatabaseContext context = {0};
    
    // Open database
    int rc = sqlite3_open("example.db", &context.db);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(context.db));
        return -1;
    }

    // Example SQL operations
    const char *create_table_sql = 
        "CREATE TABLE IF NOT EXISTS users ("
        "id INTEGER PRIMARY KEY, "
        "name TEXT, "
        "age INTEGER)";
    
    if (execute_sqlite_operation(&context, create_table_sql) != 0) {
        fprintf(stderr, "Error creating table: %s\n", context.error_message);
        sqlite3_close(context.db);
        return -1;
    }

    // Insert sample data
    const char *insert_sql = 
        "INSERT INTO users (name, age) VALUES ('John Doe', 30)";
    
    if (execute_sqlite_operation(&context, insert_sql) != 0) {
        fprintf(stderr, "Error inserting data: %s\n", context.error_message);
        sqlite3_close(context.db);
        return -1;
    }

    // Close database
    sqlite3_close(context.db);

    // Cleanup
    h_destroy();
    
    return 0;
}