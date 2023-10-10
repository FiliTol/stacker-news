from bs4 import BeautifulSoup
import requests as requests
import re
from datetime import datetime
from scripts import item


NA = None


# Function that scrapes a 'discussion' item
def scrape_discussion():
    # Produces a string then inserted in a csv
    pass


# Function that extracts the item title from the provided page
def extract_title(page):
    # Produces a string then inserted in a csv
    try:
        title = page.find('a', class_='item_title__FH7AS text-reset me-2').get_text()
    except:
        title = NA
    return title


# Function that extracts the item banner from the provided page
def extract_banner(page):
    # Produces a string then inserted in a csv
    try:
        banner = page.find('div', class_='item_other__MjgP3')
    except:
        banner = NA
    partial_banner_data = [i.text for i in banner.find_all('span')]

    final_banner = {'sats': '',
                    'boost': '',
                    'comments': '',
                    'author': '',
                    'tag': '',
                    'timestamp': '',
                    }

    for b in partial_banner_data:
        if "boost" in b:
            pass
        if "sats" in b:
            pass
        if "@" in b:
            pass

    return  banner_data


# Function that extracts the item body from the provided page
def extract_body():
    # Produces a string then inserted in a csv
    pass


# Function that extracts the comments data
# Data include both the `total sat collected by comments` and the `comment item number`
def extract_comment_data():
    # Produces a string then inserted in a csv
    pass
