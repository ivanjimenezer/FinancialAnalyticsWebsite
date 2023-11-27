create database anfin;
use anfin;
CREATE TABLE users (
    user_id INT AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(255) NOT NULL,
    password VARCHAR(255) NOT NULL,
    last_login TIMESTAMP,
    UNIQUE (username)
);

INSERT INTO users (username, password, last_login)
VALUES
    ('john32', 'admin234', '2023-01-01 08:00:00'),
    ('jane5', 'admin234', '2023-01-02 12:30:00');
