from bs4 import BeautifulSoup
import requests as requests
import re
from datetime import datetime
from scripts import item
import csv


NA = None


# Function that extracts the item title from the provided page
def extract_title(page):
    # Produces a string
    try:
        title = page.find('a', class_='item_title__FH7AS text-reset me-2').get_text()
    except:
        title = NA
    return title


# Function that extracts the item banner from the provided page
def extract_banner(page):

    # Produces a dict of banner items
    try:
        banner = page.find('div', class_='item_other__MjgP3')
    except:
        banner = NA

    partial_banner_data = [i.text for i in banner.find_all('span')]

    final_banner = {'sats': NA,
                    'boost': NA,
                    'comments': NA,
                    'author': NA,
                    'tag': NA,
                    'timestamp': NA,
                    }

    # Extract data in the banner
    username_pattern = r'@([a-zA-Z0-9]+)'
    for b in partial_banner_data:
        if "boost" in b:
            final_banner['boost'] = b
        elif "sats" in b or "sat" in b:
            final_banner['sats'] = b
        elif "@" in b:
            match = re.search(username_pattern, b).group(1)
            final_banner['author'] = match

    # Extract the data not extracted yet
    try:
        final_banner['comments'] = page.find('a', class_='text-reset position-relative').get_text()
    except:
        pass

    try:
        final_banner['tag'] = page.find('span', class_='item_newComment__HSNhq badge').get_text()
    except:
        pass

    # The try except is already in function definition
    final_banner['timestamp'] = item.get_timedate(page)

    return final_banner


# Function that extracts the item body from the provided page
def extract_body_links(page):

    # Return a list of items provided a soup
    try:
        ex_link = [li['href'] for li in page.find_all('a', target="_blank", rel="noreferrer nofollow noopener")]
    except:
        ex_link = NA

    return ex_link


# Function that extracts the comments data
# Data include both the `total sat collected by comments` and the `comment item number`
def extract_comment_stacked(page):

    # Produces a string then inserted in a csv
    try:
        amount = page.find('div', class_="text-muted nav-item").get_text()
    except:
        amount = NA

    return amount


# Function that extracts the item codes of comments in an item discussion
def extract_comment_item_code(page):

    # Produces a list of integers

    try:
        commenters = []
        for i in page.find_all('a', class_='text-reset position-relative'):
            c = i.get('href')
            r = r'(\D+)'
            res = int(re.sub(r, '', c))

            commenters.append(res)
        return commenters[1:]
    except:
        return NA


# Function that collects the outputs of all the extraction functions for discussion item
# def scrape_discussion(item_code, page):
#
#     # Produces a string then inserted in a csv
#     entry = [extract_title(page),
#              str(item_code),
#              extract_banner(page),
#              extract_body_links(page),
#              extract_comment_stacked(page),
#              extract_comment_item_code(page)
#              ]
#
#     # Appends every new profile to a csv file in the provided path
#     file_path = "../data/discussion.csv"
#     row_head = ["Title",
#                 "Item code",
#                 "Banner data",
#                 "Body links",
#                 "Sats received by comments",
#                 "Comments item code",
#                 ]
#
#     with open(file_path, 'w', encoding='utf_8_sig', newline="") as csvfile:
#         csvwriter = csv.writer(csvfile)
#         csvwriter.writerow(row_head)
#
#     try:
#         with open(file_path, 'a', encoding='utf_8_sig', newline="") as csvfile:
#             csvwriter = csv.writer(csvfile)
#             csvwriter.writerow(entry)
#     except:
#         pass
#     return 'Done'
