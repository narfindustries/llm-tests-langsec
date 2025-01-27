database sqlite3_db {
    // Table definition for users
    table users {
        id              INTEGER PRIMARY KEY AUTOINCREMENT,
        username        TEXT NOT NULL UNIQUE,
        email           TEXT NOT NULL UNIQUE,
        password_hash   TEXT NOT NULL,
        created_at      DATETIME DEFAULT CURRENT_TIMESTAMP,
        last_login      DATETIME
    }

    // Table definition for posts
    table posts {
        id              INTEGER PRIMARY KEY AUTOINCREMENT, 
        user_id         INTEGER NOT NULL,
        title           TEXT NOT NULL,
        content         TEXT NOT NULL,
        created_at      DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at      DATETIME,
        FOREIGN KEY(user_id) REFERENCES users(id)
    }

    // Table definition for comments
    table comments {
        id              INTEGER PRIMARY KEY AUTOINCREMENT,
        post_id         INTEGER NOT NULL, 
        user_id         INTEGER NOT NULL,
        content         TEXT NOT NULL,
        created_at      DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY(post_id) REFERENCES posts(id),
        FOREIGN KEY(user_id) REFERENCES users(id)
    }

    // Indexes for performance optimization
    index idx_users_username ON users(username);
    index idx_posts_user_id ON posts(user_id);
    index idx_comments_post_id ON comments(post_id);
}