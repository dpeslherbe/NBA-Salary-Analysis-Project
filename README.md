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

## Initial Clustering Attempt

Using the Data Set created before, we start by filtering through the players used; for the clustering and the analysis to have any value, we must be sure that the players selected correspond to certains minimum criteria. In this case, since we are looking at performance, we can disregard the player salary for now, and we make sure to filter players who have little impact on a game. For this part, the criteria chosen were a minimum of 41 Games played (so at least half the season), and a minimum of 12 Minutes per Game (at least a Quarter of Total Game Played on Average).

We notice many of the variables have significant correlation; but this is to be expected. For example, you usually want to play high minutes if you are going to score a lot of points; the negative correlation between 3-point shot rate and rebounding percentage can be explained by the fact that a 3-point shot implies distance from the rim, thus from most rebounds (unless it is a long rebound, but the goal of this project is not shot mechanics and ball physics).

To reduce the number of variables used, we use Principal Component Analysis; the first step before applying this is to normalize the data to reduce scaling effects (points per game are measured on a scale of 0 to the maximum ppg in a season, vs a scale of 0 to 1 for shooting percentage).

Note that to get a better idea of player value, we must take our statistics relative to league average (this helps mitigate the effects of changes in the way the game is and has been played throughout the years, with the changes in shot selection and game analytics becoming more common place nowadays, by classifying the best scorers or the most efficient shooters relative to league average that year, and then scaling every year together as well).

Note that we first cluster players based on diret stats only, then again with advanced metrics related to game winning impact i.e. WS, VROP, etc...

Then, to reduce the number of variables to work with, we set a variance threshold of 90%; this means we will only retain the Principal Components that explain 90% or more of the variance (in the case of the first Cluster Analysis, we have 10, and for the second, we have 2).

Typical methods such as the Elbow Method, Silhouette Method, Gap Statistic Method and NbClust Method might give us the best number of clusters to choose in this case, but here we are no searching for the best number of clusters, since having 2 clusters would likely indicate which players had a good season which year, and who did not; here, we wish to quantify a player's season in more terms than just good or bad (e.g. statistically good, but low winning impact, big winning impact with low usage, star player types, etc...).

Thus, we start by modifying the Silhouette Score function, previously developped for the Pokemon Strength Analysis, to calculate the average silhouette scores for multiple numbers of clusters. Then, instead of looking at the best silhouette score, we look at silhouette score improvement, i.e. how much more information do we get from the clustering by adding an extra cluster. For the first Cluster Analysis, we then get 8 clusters, and 5 for the second Cluster Analysis of Advanced Metrics.

Note that overall, this gives a very high number of clusters (38, since two of the combinations are empty. We do notice that the eight clusters from the first cluster analysis also separate players into roles according to their counting stats, and importance in a game (star guards/wings, star bigs, rim-running bigs, shooters, etc...), and the second cluster analysis separates players into game impact categories (two-way players, good offence/bad defense, bad offence/good defense, net-ngative on the court, etc...).

This number of combinations is very to be truly explored in depth, thus we will have to find a way to reduce the overall number of clusters, or only focus on certain combined clusters for the rest of the analysis (This is still to be determined, and no decision has been made or explored yet; please feel free to reach out for any questions, ideas, or suggestions at this time).
