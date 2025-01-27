#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>

// Structure to represent a Gemini document
typedef struct {
    char *title;
    char *content;
} GeminiDoc;


// Function to create a Gemini document from a SQLite row
GeminiDoc* createGeminiDoc(sqlite3_stmt *stmt) {
    GeminiDoc *doc = (GeminiDoc *)malloc(sizeof(GeminiDoc));
    if (doc == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        return NULL;
    }
    doc->title = (char *)sqlite3_column_text(stmt, 0);
    doc->content = (char *)sqlite3_column_text(stmt, 1);
    return doc;
}


int main() {
    sqlite3 *db;
    char *zErrMsg = 0;
    int rc;
    const char *sql;
    sqlite3_stmt *stmt;

    rc = sqlite3_open("gemini.db", &db); // Replace gemini.db with your database file

    if (rc) {
        fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
        return 1;
    }

    sql = "SELECT title, content FROM gemini_docs"; // Replace gemini_docs with your table name

    rc = sqlite3_prepare_v2(db, sql, -1, &stmt, 0);
    if (rc != SQLITE_OK ) {
        fprintf(stderr, "SQL error: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        return 1;
    }

    while ((rc = sqlite3_step(stmt)) == SQLITE_ROW) {
        GeminiDoc *doc = createGeminiDoc(stmt);
        if (doc != NULL) {
            printf("Title: %s\n", doc->title);
            printf("Content: %s\n\n", doc->content);
            free(doc); //Free allocated memory
        }
    }

    sqlite3_finalize(stmt);
    sqlite3_close(db);

    return 0;
}
