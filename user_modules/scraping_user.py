from bs4 import BeautifulSoup
import requests as requests
import re


# Fixed value to be returned for missing values or request errors
NA = None


# Function that returns the webpage where the user profile is
def get_profile(name): # TODO this function could be generalized for crawling both user pages and post pages

    try:
        # Provided a string returns a bs4.BeautifulSoup object
        url_posts = f'https://stacker.news/{name}'
        response = requests.get(url_posts)
        response.raise_for_status()
        soup = BeautifulSoup(response.text, 'html.parser')
        return soup
    except:
        return NA


# Function that returns the total amount of sats stacked by the user
def get_total_stacked(name):
    # Provided a string returns an integer
    page = get_profile(name)
    try:
        nym_tot_stacked = page.find('div', class_='mb-2 ms-0 ms-sm-1 user-header_username__bqOV1 text-success').get_text()
        regex_sats = r'[^\d]+'
        nym_tot_stacked = int(re.sub(regex_sats, '', nym_tot_stacked))
        return nym_tot_stacked
    except:
        return NA


# Function that returns the first user appearance (measured by the code of the first item created)
def get_stacking_since(name):
    # Provided a string returns a string
    page = get_profile(name)
    try:
        nym_first_item = page.find('a', class_='ms-1').get_text()[1:]
        return nym_first_item
    except:
        return NA


# Function that returns the longest cowboy-hat streak
# If a user tip 100 sats (or more) in a day it gets a cowboy hat for that day
def get_cowboy_streak(name):
    # Provided a string returns an integer
    page = get_profile(name)
    try:
        nym_ch_streak = page.find_all('small', class_='text-muted d-flex-inline')[1].text
        regex_ch = r'[^\d]+'
        nym_ch_streak = int(re.sub(regex_ch, '', nym_ch_streak))
        return nym_ch_streak
    except:
        return NA


# Function that returns the total number of Items created by the user
def get_total_items(name):
    # Provided a string returns an integer
    page = get_profile(name)
    try:
        nym_tot_items = page.find('a', class_='nav-link', href=f'/{name}/all').get_text()
        regex_nym_items = r' items\b'
        nym_tot_items = int(re.sub(regex_nym_items, '', nym_tot_items))
        return nym_tot_items
    except:
        return NA

