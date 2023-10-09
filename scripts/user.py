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
    except:
        return NA

    # Section that extracts the total amount of sats stacked by the user
    try:
        nym_tot_stacked = soup.find('div', class_='mb-2 ms-0 ms-sm-1 user-header_username__bqOV1 text-success').get_text()
        regex_sats = r'[^\d]+'
        nym_tot_stacked = int(re.sub(regex_sats, '', nym_tot_stacked))
    except:
        nym_tot_stacked = NA

    # Section that returns the first user appearance (measured by the code of the first item created)
    try:
        nym_first_item = soup.find('a', class_='ms-1').get_text()[1:]
    except:
        nym_first_item = NA

    # Section that returns the longest cowboy-hat streak
    # If a user tip 100 sats (or more) in a day it gets a cowboy hat for that day
    try:
        nym_ch_streak = soup.find_all('small', class_='text-muted d-flex-inline')[1].text
        regex_ch = r'[^\d]+'
        nym_ch_streak = int(re.sub(regex_ch, '', nym_ch_streak))
    except:
        nym_ch_streak = NA

    # Section that returns the total number of Items created by the user
    try:
        nym_tot_items = soup.find('a', class_='nav-link', href=f'/{name}/all').get_text()
        regex_nym_items = r' items\b'
        nym_tot_items = int(re.sub(regex_nym_items, '', nym_tot_items))
    except:
        nym_tot_items = NA

    return [name, nym_tot_stacked, nym_first_item, nym_ch_streak, nym_tot_items]

