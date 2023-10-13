from bs4 import BeautifulSoup
import requests as requests
import item
import discussion
import link
import comment
from tqdm import tqdm
import sqlite3
import time


# Sampling the items to scrape
from random import sample
sampled_items = sample([*range(1,250000)], 1000)

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

# Queries for entry insertion in tables
insert_comment = """
INSERT OR IGNORE INTO comments (
    ItemCode,
    Sats,
    Boost,
    Comments,
    Author,
    Tag,
    Timestamp,
    CommentsItemCode
    ) values (?, ?, ?, ?, ?, ?, ?, ?)
"""

insert_post = """
INSERT OR IGNORE INTO post (
    Title,
    Category,
    ItemCode,
    Sats,
    Boost,
    Comments,
    Author,
    Tag,
    Timestamp,
    MainLink,
    BodyLinks,
    SatsReceivedComments,
    CommentsItemCode
    ) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
"""

insert_exception = """
INSERT OR IGNORE INTO exceptions (
    RequestResult,
    ItemCode,
    Soup
    ) values (?, ?, ?)
"""

# Create database connection
conn = sqlite3.connect('data/stacker_news.sqlite')
cur = conn.cursor()

# Insert tables into database
cur.executescript(sql_comment)
cur.executescript(sql_post)
cur.executescript(sql_exception)

# Commit table creation and wait 1 second
conn.commit()
time.sleep(1)

# Loop for item scraping
for i in tqdm(sampled_items):
    try:
        # Provided a string returns a bs4.BeautifulSoup object
        url_posts = f'https://stacker.news/items/{i}'
        response = requests.get(url_posts)
        response.raise_for_status()
        soup = BeautifulSoup(response.text, 'html.parser')

        if item.detect_item_type(i, soup) == 'comment':
            # Insert every new entry into a new row in the provided DB
            entry = (str(i),
                     str(comment.extract_banner(soup)['sats']),
                     str(comment.extract_banner(soup)['boost']),
                     str(comment.extract_banner(soup)['comments']),
                     str(comment.extract_banner(soup)['author']),
                     str(comment.extract_banner(soup)['tag']),
                     str(comment.extract_banner(soup)['timestamp']),
                     str(comment.extract_comment_item_code(soup))
                     )
            try:
                cur.execute(insert_comment, entry)
            except:
                print(f'Error while inserting the comment item {i} in the database')

        elif item.detect_item_type(i, soup) == 'link':
            # Appends every new profile to a csv file in the provided path
            entry = (str(link.extract_title(soup)),
                     str(item.detect_item_type(i, soup)),
                     str(i),
                     str(link.extract_banner(soup)['sats']),
                     str(link.extract_banner(soup)['boost']),
                     str(link.extract_banner(soup)['comments']),
                     str(link.extract_banner(soup)['author']),
                     str(link.extract_banner(soup)['tag']),
                     str(link.extract_banner(soup)['timestamp']),
                     str(link.extract_link(soup)),
                     str(link.extract_body_links(soup)),
                     str(link.extract_comment_stacked(soup)),
                     str(link.extract_comment_item_code(soup))
                     )
            try:
                cur.execute(insert_post, entry)
            except:
                print(f'Error while inserting the link item {i} in the database')

        elif item.detect_item_type(i, soup) in ['discussion', 'poll', 'bounty']:
            entry = (str(discussion.extract_title(soup)),
                     str(item.detect_item_type(i, soup)),
                     str(i),
                     str(discussion.extract_banner(soup)['sats']),
                     str(discussion.extract_banner(soup)['boost']),
                     str(discussion.extract_banner(soup)['comments']),
                     str(discussion.extract_banner(soup)['author']),
                     str(discussion.extract_banner(soup)['tag']),
                     str(discussion.extract_banner(soup)['timestamp']),
                     None,
                     str(discussion.extract_body_links(soup)),
                     str(discussion.extract_comment_stacked(soup)),
                     str(discussion.extract_comment_item_code(soup))
                     )

            # Appends every new profile to a csv file in the provided path
            try:
                cur.execute(insert_post, entry)
            except:
                print(f'Error while inserting the post item {i} in the database')

        if i % 5000 == 0:
            conn.commit()
            continue
    except:
        try:
            exception_entry = (
                str(response),
                str(i),
                str(soup)
            )

            cur.execute(insert_exception, exception_entry)

        except:
            continue

# Final commit
conn.commit()

# Close connection to DB
cur.close()
conn.close()

