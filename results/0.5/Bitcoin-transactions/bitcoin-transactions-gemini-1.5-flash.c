#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent a Bitcoin transaction
typedef struct {
    char txid[65]; // Transaction ID (64 hex characters + null terminator)
    double amount;
    char address[35]; // Bitcoin address (max length 34 + null terminator)
} Transaction;


// Function to read transactions from a file
Transaction* readTransactions(const char* filename, int* numTransactions) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        perror("Error opening transactions file");
        return NULL;
    }

    char line[1024];
    *numTransactions = 0;
    Transaction* transactions = NULL;

    while (fgets(line, sizeof(line), file) != NULL) {
        // Simple parsing assuming comma-separated values
        char* txid = strtok(line, ",");
        char* amountStr = strtok(NULL, ",");
        char* address = strtok(NULL, ",");

        if (txid == NULL || amountStr == NULL || address == NULL) {
            fprintf(stderr, "Error parsing transaction line: %s", line);
            continue; // Skip malformed lines
        }

        //Reallocate memory for transactions array.  Error handling included.
        Transaction* tempTransactions = realloc(transactions, (*numTransactions + 1) * sizeof(Transaction));
        if (tempTransactions == NULL) {
            perror("Memory allocation failed");
            fclose(file);
            free(transactions);
            return NULL;
        }
        transactions = tempTransactions;


        strncpy(transactions[*numTransactions].txid, txid, sizeof(transactions[*numTransactions].txid) -1);
        transactions[*numTransactions].txid[sizeof(transactions[*numTransactions].txid) -1] = '\0'; //Ensure null termination

        transactions[*numTransactions].amount = atof(amountStr);
        strncpy(transactions[*numTransactions].address, address, sizeof(transactions[*numTransactions].address) -1);
        transactions[*numTransactions].address[sizeof(transactions[*numTransactions].address) -1] = '\0'; //Ensure null termination

        (*numTransactions)++;
    }

    fclose(file);
    return transactions;
}


int main() {
    int numTransactions;
    Transaction* transactions = readTransactions("transactions.txt", &numTransactions);

    if (transactions == NULL) {
        return 1; // Indicate an error
    }

    // Process transactions (example: print them)
    for (int i = 0; i < numTransactions; i++) {
        printf("Transaction ID: %s, Amount: %.2f, Address: %s\n",
               transactions[i].txid, transactions[i].amount, transactions[i].address);
    }

    free(transactions); // Free dynamically allocated memory
    return 0;
}

transactions.txt
e2e798a6987f966a61e77d68622a9a951f32036871877569121846e42625126b,1.234,bc1q9d8z7wun49av0v47g78z6w306428c697569
a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2,5.678,bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq
