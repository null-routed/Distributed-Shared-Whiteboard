DROP DATABASE IF EXISTS shared_whiteboards;
CREATE DATABASE IF NOT EXISTS shared_whiteboards
CHARACTER SET utf8mb4
COLLATE utf8mb4_unicode_ci;

USE shared_whiteboards;

-- Users Table
CREATE TABLE Users (
    UserID INT AUTO_INCREMENT PRIMARY KEY,
    Username VARCHAR(50) NOT NULL UNIQUE,
    Password VARCHAR(64) NOT NULL,
    Name VARCHAR(50) NOT NULL,
    Surname VARCHAR(100) NOT NULL,
    Email VARCHAR(100) NOT NULL,
    CreatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Whiteboards Table
CREATE TABLE Whiteboards (
	WhiteboardID INT AUTO_INCREMENT PRIMARY KEY,
    Name VARCHAR(50) NOT NULL,
    Description TEXT,
    ReadOnly BOOLEAN NOT NULL		-- True: only the creator user can write on the whiteboard
    -- ShareLink VARCHAR(200)  		-> ????????????
);

-- Whiteboard members and creator Table
CREATE TABLE WhiteboardParticipants (
	WhiteboardID INT NOT NULL,
    UserID INT NOT NULL,
    IsOwner BOOLEAN NOT NULL		-- True: UserID is the owner of WhiteboardID
);
