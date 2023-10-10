from bs4 import BeautifulSoup
import requests as requests
import re
from datetime import datetime


# Fixed value to be returned for missing values or request errors
NA = None


# Function that returns the webpage where the item is
def get_item_page(item_code):  # TODO this function could be bundled-in the function scraping_user.get_profile()

    # Provided a string returns a bs4.BeautifulSoup object
    try:
        url_posts = f'https://stacker.news/items/{item_code}'
        response = requests.get(url_posts)
        response.raise_for_status()
        soup = BeautifulSoup(response.text, 'html.parser')
        return soup
    except:
        return NA


# Function that returns the datetime given an html file where a timestamp is present
def get_timedate(page):

    # Provided a string and a soup, returns a datetime object
    try:
        timestamp_pattern = re.compile(r'(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z)')
        original_format = "%Y-%m-%dT%H:%M:%S.%fZ"

        timestamp_element = page.find('a', attrs={'title': timestamp_pattern})
        timestamp_match = timestamp_pattern.search(timestamp_element['title']).group(1)
        timestamp_datetime = datetime.strptime(timestamp_match, original_format)
        return timestamp_datetime
    except:
        return NA


# Job offers items are identifiable by an 'apply' button.
# Function that returns 'True' if the item is a job offer.
def detect_item_job(page):

    # Provides TRUE if the item is a Job offer
    try:
        collect_button = page.find('a',
                                   class_='btn btn-primary',
                                   target='_blank',
                                   tabindex="0").get_text()
        regex_apply = r'^mailto:[^@]+@[^@]+\s+via\s+Stacker\s+News$'
        spot_job = re.sub(regex_apply, '', collect_button)

        if spot_job.strip() == 'apply':
            return True
    except:
        return False


# Function that returns 'true' if the item is a 'link' item
def detect_item_link(page):

    # Provides TRUE if the item is a link item
    try:
        collect_link = page.find('a', class_='item_link__4cWVs', target='_blank').get_text()
        regex_link = r'\bhttps?://\S+'
        spot_link = re.sub(regex_link, '', collect_link)
        return True
    except:
        return False


# Function that returns 'true' if the item is a 'link' item
def detect_item_poll(page):

    # Provides TRUE if the item is a poll item
    try:
        collect_poll = page.find('div', class_='poll_pollBox__Z9Blt').get_text()
        return True
    except:
        return False


# Function that returns 'true' if the item is a 'link'
def detect_item_bounty(page):

    # Provides TRUE if the item is a bounty item
    try:
        collect_bounty = page.find('div', class_='px-3 py-1 d-inline-block bg-grey-medium rounded text-success').get_text()
        return True
    except:
        return False


# Function that returns true if the item is a post item, meaning that it has a title
# If an item does not have a title, then it is a comment
def detect_title(item, page):

    # Provides TRUE if the item has a title, therefore it is a post
    try:
        title = page.find('a', class_='item_title__FH7AS text-reset me-2', href=f'/items/{item}')
        return title.get_text()
    except:
        return False


# Function that returns the item classification provided the item number and the soup
def detect_item_type(i, page):

    # Provided an int, soup it returns a string
    category = NA
    try:
        if detect_title(i, page):
            category = 'post'
            if detect_item_link(page):
                category = 'link'
            elif detect_item_bounty(page):
                category = 'bounty'
            elif detect_item_poll(page):
                category = 'poll'
            elif detect_item_job(page):
                category = 'job'
            else:
                category = 'discussion'
        else:
            category = 'comment'
        return category
    except:
        return NA


# Function that scrapes a 'discussion' item
def scrape_discussion():

    # Produces a string then inserted in a csv
    pass


# Function that scrapes a 'link' item
def scrape_link():

    # Produces a string then inserted in a csv
    pass


# Function that scrapes a 'poll' item
def scrape_poll():

    # Produces a string then inserted in a csv
    pass


# Function that scrapes a 'discussion' item
def scrape_bounty():

    # Produces a string then inserted in a csv
    pass


# Function that scrapes a 'comment' item
def scrape_comment():

    # Produces a string then inserted in a csv
    pass

