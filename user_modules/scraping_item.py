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
def get_timedate(item_code):
    # Provided a string returns a datetime.datetime object
    page = get_item_page(item_code)
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
def detect_item_job(item_code):

    # Provides TRUE if the item is a Job offer
    page = get_item_page(item_code)
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
def detect_item_link(item_code):

    # Provides TRUE if the item is a link item
    page = si.get_item_page(item_code)
    try:
        collect_link = page.find('a', class_='item_link__4cWVs', target='_blank').get_text()
        regex_link = r'\bhttps?://\S+'
        spot_link = re.sub(regex_link, '', collect_link)
        return True
    except:
        return False

