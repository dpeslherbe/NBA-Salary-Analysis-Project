# NBA Salary Analysis Project

The Goal of this project is to cluster players according to contracts and performance, so that we may then use this information to either decide if a player is playing above or under contract value, as well, as predict the best contract value for a specific set of performance metrics.

### NBA Data Collection

For this project, we need a lot of data concerning NBA Players and their Salaries. However, there does not seem to be many readily available datasets with this information free for use on the web. Fortunately for us, web scraping is a solution available for this task.

Note that ESPN has NBA Salary Data for Players from 1999-2000 to 2019-2020 on their site @ http://www.espn.com/nba/salaries. We can scrape this data using the tidyverse and rvest packages in R, using SelectorGadget to find the relevant HTML nodes necessary. However, we must also make sure to use tidyverse-included package stringr to remove the unnnecessary information still in the tables (in this case, the position played).

However, this set of data seems to be incomplete (many players from the early 2000s are missing salary information on the website).

Thus, using WebScraper.io tools to scrape Javascript generated data from the hoopshype.com website @ https://hoopshype.com/salaries/players/, we get csv files (converted to excel by automation on the computer) containing player salary information going back to 1990-1991. Then, using readxl package, and some light coding, we are able to obtain a data frame (named DataSal in this case) with this information.

Then using nbastatR package and function bref_players_stats(), we scrape per game and advanced season statistics from players in the same set of years as our salary data. We then discard superflous information (links, team, season, etc...), and combine the data into DataStats.

Using left_join(), we combine both data frames according to player name, and season. However, there seem to be some missing values, when using the VIM package and the associated aggr() function to check for missing values.

We then set up the index of rows with missing values, then use unique() to get the players names concerned by the row issues. For some, the issue is a difference in name spelling between data sources; for others, the seaon statistics are missing either for a season-long injury, being waived, retirement, or not seeing the floor in a specific season (but the player still has money paid, which is why the row is empty; paid for not playing).

We then re-name all the players in DataStats with this issue, and use na.omit to remove the data for the rows in the other cases.

Then, we export our Final Data Set as CSV for ease of use in the future.
