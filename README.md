# BehaviorBounty

Humans, for the most part, behave and act as economic agents driven by [primordial
incentives](https://en.wikipedia.org/wiki/The_Naked_Ape) or by more sophisticated
reward schemes. Actions and behaviours carried on in Internet-based contexts (as
forums, social media, etc) are not exempted from this biological truth. This is
the reason why social media platforms and forums soon understood that the implementation
of features as *likes* or some sort of other reward scheme could improve customer
retention and interaction by orders of magnitute.

The following project has the goal to investigate the most rewarding behaviours
for users interacting in the online forum [Stacker News](https://stacker.news), an
unconventional internet-based forum where likes are replaced by *zaps*, bitcoin
microtransactions. 

More details about the project can be found in the [attached paper](paper.pdf).

## Reproduce the environment for the analysis

> **Important**: as of june 2024, the Stacker News forum implemented several new
> features and gave to the users the option to hide some information about their
> profiles. This advancements could generate some inconsistencies between the
> results reported in the paper and the current forum landscape. If you need
> to reproduce the analysis as carried on by the authors, you're suggested
> to get in touch with me. My contacts are listed in my [personal website](www.filippotol.in).

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
### R packages

It is suggested to execute `R` scripts using the Rstudio sotfware and open the folder in Rstudio as an Rproject (by 
opening the `stacker_news.Rproj` file).
At the execution of every `.R` script, a function will verify if the needed packages are installed: if not then it 
procedes to install them, if installed they are imported in the environment.

#### Alternative installation of R packages

In order to sync all the packages and `R` requirements, is also possible to use the renv tools provided by Rstudio.
Open the project file with Rstudio, navigate to the *tools* settings and open the *project options*.
There, navigate to the *environments* section end activate the setting *Use renv for this project*.

The `R` session will restart. Then, navigate to the console and type the following command:

```{R}
renv::init()
```

This command will ask for a choice in the renv management, select the option to restore the project from the lockfile.
Rstudio will then proceed to install all the R packages needed.


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



## I'm Using GitHub Under Protest

This project is currently hosted on GitHub.  This is not ideal; GitHub is a
proprietary, trade-secret system that is not Free and Open Souce Software
(FOSS). I urge you to read about the
[Give up GitHub](https://GiveUpGitHub.org) campaign from
[the Software Freedom Conservancy](https://sfconservancy.org) to understand
some of the reasons why GitHub is not a good place to host FOSS projects.

Any use of this project's code by GitHub Copilot, past or present, is done
without our permission. I do not consent to GitHub's use of this project's
code in Copilot.

![Logo of the GiveUpGitHub campaign](https://sfconservancy.org/static/img/GiveUpGitHub.png)
