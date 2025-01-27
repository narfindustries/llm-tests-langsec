#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent a Bitcoin transaction
typedef struct {
    char txid[65]; // Transaction ID (64 hex characters + null terminator)
    double amount;
    char address[35]; // Bitcoin address (34 characters + null terminator)

} Transaction;


// Function to read transactions from a file
Transaction* readTransactions(const char* filename, int* numTransactions) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        perror("Error opening file");
        return NULL;
    }

    char line[256];
    Transaction* transactions = NULL;
    *numTransactions = 0;

    while (fgets(line, sizeof(line), file) != NULL) {
        // Basic parsing assuming comma-separated values
        char* txid = strtok(line, ",");
        char* amountStr = strtok(NULL, ",");
        char* address = strtok(NULL, ",");


        if (txid == NULL || amountStr == NULL || address == NULL) {
            fprintf(stderr, "Error parsing line: %s", line);
            continue; //Skip the line if parsing fails
        }

        Transaction newTransaction;
        strncpy(newTransaction.txid, txid, sizeof(newTransaction.txid) -1);
        newTransaction.txid[sizeof(newTransaction.txid) -1] = '\0'; //Null termination for safety

        newTransaction.amount = atof(amountStr);
        strncpy(newTransaction.address, address, sizeof(newTransaction.address)-1);
        newTransaction.address[sizeof(newTransaction.address)-1] = '\0';

        transactions = realloc(transactions, (*numTransactions + 1) * sizeof(Transaction));
        if (transactions == NULL) {
            perror("Memory allocation failed");
            fclose(file);
            return NULL;
        }
        transactions[*numTransactions] = newTransaction;
        (*numTransactions)++;
    }

    fclose(file);
    return transactions;
}


int main() {
    int numTransactions;
    Transaction* transactions = readTransactions("transactions.csv", &numTransactions);

    if (transactions == NULL) {
        return 1; // Indicate an error
    }

    // Process the transactions (example: print them)
    for (int i = 0; i < numTransactions; i++) {
        printf("Transaction ID: %s, Amount: %.2f, Address: %s\n",
               transactions[i].txid, transactions[i].amount, transactions[i].address);
    }

    free(transactions); //Free allocated memory
    return 0;
}

**transactions.csv example:**

e2e74100a696e22157426857d9272c159209582169c16c6748696361,1.5,1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2
a75d804c404696889a30605a68107a6968a0c21341513e968a1b33f5,0.2,3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy
