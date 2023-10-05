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
