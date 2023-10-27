import sqlite3


# Queries for table insertion
sql_comment = """
DROP TABLE IF EXISTS comments;
CREATE TABLE comments (
    ItemCode TEXT,
    Sats TEXT,
    Boost TEXT,
    Comments TEXT,
    Author TEXT,
    Tag TEXT,
    Timestamp TEXT,
    CommentsItemCode TEXT,
    PRIMARY KEY (ItemCode))
"""

sql_post = """
DROP TABLE IF EXISTS post;
CREATE TABLE post (
    Title TEXT,
    Category TEXT,
    ItemCode TEXT,
    Sats TEXT,
    Boost TEXT,
    Comments TEXT,
    Author TEXT,
    Tag TEXT,
    Timestamp TEXT,
    MainLink TEXT,
    BodyLinks TEXT,
    SatsReceivedComments TEXT,
    CommentsItemCode TEXT,
    PRIMARY KEY (ItemCode))
"""

sql_exception = """
DROP TABLE IF EXISTS exceptions;
CREATE TABLE exceptions(
    RequestResult TEXT,
    ItemCode TEXT,
    Soup TEXT,
    PRIMARY KEY (ItemCode))
"""

sql_user = """
DROP TABLE IF EXISTS user;
CREATE TABLE user (
    User TEXT,
    TotalStacked TEXT,
    FirstItem TEXT,
    HatStreak TEXT,
    NumItems TEXT,
    PRIMARY KEY (User))
"""

# Create database connection
conn = sqlite3.connect('data/stacker_news.sqlite')
cur = conn.cursor()

# Insert tables into database
cur.executescript(sql_comment)
cur.executescript(sql_post)
cur.executescript(sql_exception)
cur.executescript(sql_user)

# Commit table creation and wait 1 second
conn.commit()

# Close connection to DB
cur.close()
conn.close()
