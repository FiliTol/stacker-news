from scripts import user
from tqdm import tqdm
import sqlite3
import pandas as pd


# Query for author retrieval from DB
query = """
SELECT DISTINCT Author
FROM (
    SELECT Author
    FROM comments
    UNION ALL
    SELECT Author
    FROM post
     );
"""

insert_user = """
INSERT OR IGNORE INTO user (
    User,
    TotalStacked,
    FirstItem,
    HatStreak,
    NumItems
    ) values (?, ?, ?, ?, ?)
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

conn = sqlite3.connect("../data/stacker_news.sqlite")
cur = conn.cursor()

cur.executescript(sql_user)

sql_query = pd.read_sql(query, conn)
result = pd.DataFrame(sql_query, columns=["Author"])

for i in tqdm(result["Author"]):
    try:
        profile_data = user.get_profile(i)
        entry = (
            str(profile_data[0]),
            str(profile_data[1]),
            str(profile_data[2]),
            str(profile_data[3]),
            str(profile_data[4]),
        )
        try:
            cur.execute(insert_user, entry)
        except:
            continue
    except:
        continue

cur.close()
conn.close()