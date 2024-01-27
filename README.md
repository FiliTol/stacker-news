# Stacker News Network Analysis

## Reproduce the environment

In order to reproduce the environment used for the research, the following steps are suggested.

1. Clone locally the current repo (or download the zipped folder);
2. Unzip the zipped folder in a custom path;
3. Navigate to the unzipped folder at the custom path and execute the following commands
to create a python environment, activate the environment and install the requirements.

*The '$' symbol indicates a new prompt line*

```{bash}
$ python -m venv .venv
$ source .venv/bin/activate
$ pip install -r requirements.txt
```

At this point all the necessary python packages are installed locally in the environment.
The scraping process is break down into 3 steps:
1. Setup the database folder and a new sqlite database;
2. Scrape the items of the forum;
3. Scrape the user profiles (profiles crawled are the ones of users that appeared at least once in the previous scraping process).

```{bash}
$ python python/setupDB.py         # Setup SQLite database
$ python python/scraping_items.py  # Scrape forum items
$ python python/scraping_users.py  # Scrape user profiles
```

It is suggested to execute `R` scripts using the Rstudio sotfware and open the folder in Rstudio as an Rproject (by 
opening the `stacker_news.Rproj` file).
At the execution of every `.R` script, a function will verify if the needed packages are installed: if not then it 
procedes to install them, if installed they are imported in the environment.

These steps reproduce exactly the environment and dataset used to produce this research.

## Project structure and customization

### Python code

The functions and parameters used for the webscraping activity are located in different scripts.
Scripts are freely customizable. In order to change the number of items to retrieve or the exact range, edit `python/scraping_items.py:62`.

```
python
├── comment.py
├── discussion.py
├── __init__.py
├── item.py
├── link.py
├── scraping_items.py
├── scraping_users.py
├── setupDB.py
└── user.py
```

### R code

The structure of `R` scripts is based on the paper chapters.
`overview` folder contains the `data_cleaning.R` script (that executes transformations on the data and saves RDS files); 
the `summary_tables.R` contains the code used for the initial data exploration.
The `directed` folder contains all the code used for the social network analysis. The `directed_general.R` script
contains the procedures to reproduce the general graph section. The numbered scripts are referred to the
five periods analysed to setup the final table of the paper.

```
R
├── directed
│ ├── directed_general.R
│ ├── fifth.R
│ ├── first.R
│ ├── fourth.R
│ ├── second.R
│ └── third.R
└── overview
    ├── data_cleaning.R
    └── summary_tables.R
```

### Data

Data are contained in a single sqlite database file inside the `data` folder.
The database contains four tables:

```
stacker_news.sqlite
├── comments            # All the 'comment' items
├── post                # All the 'post` items
├── user                # All the user profiles 
├── exceptions          # Exceptions and errors occured during the scraping process
```

Every script interacting with the data at its source is set to search for the database file in the `~data/` path.

> The `setupDB.py` script **completely wipes** the `stacker_news.sqlite` file. Remember to backup the `stacker_news.sqlite' file
> before running any python script.

#### RDS files

In order to simplify the data processing and analysis conducted in `R`, data used for the analysis are saved in `.RDS` form
and are avaliable in the `RDS_files` folder in the main directory of the project.

```
RDS_files
├── c_fifth_period
├── c_first_period
├── c_fourth_period
├── comments
├── c_second_period
├── c_third_period
├── p_fifth_period
├── p_first_period
├── p_fourth_period
├── posts
├── p_second_period
├── p_third_period
└── users
```

The `post`, `comments` and `users` files are copies of the respective `data.table` objects. Files starting by 'c'
correspond to `data.table` objects referring to the comments table (partitioned into periods); files starting by 'p' are
referring to the posts table (partitioned into periods).

### Images

The execution of the `R` scripts generates some plot images, used for exploratory analysis. The images will be generated inside an `images/` folder.



