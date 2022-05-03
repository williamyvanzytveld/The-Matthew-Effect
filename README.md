---
title: 'The Matthew Effect'
author: "William Y. VanZytveld"
date: "May 2022"
output: html_document
---

```{r, echo = FALSE, message = FALSE}
library(tidyverse)
library(lubridate)
library(robotstxt)
library(tidytext)
library(rvest)
library(formattable)
library(pander)
library(reactable)
library(wesanderson)

```

# Initial Premise

As a longtime avid fútbol fan, I am interested in exploring the effects of birth dates on a player's career. This question is prompted by reading Malcolm Gladwell's, *Outliers* in which he supports his relative age theory with incredible examples of how many professional athletes, particularly in Hockey, have an unusually large proportion of birthdays clustered in the beginning three months. One might even say it is statistically significant. The theory is that during adolescent years of development, children born near the cut off date (i.e. January 1) are more physically developed than their December birthday counterparts. This results in promotion to more competitive age groups and more attention in development. Malcolm labeled it, "accumulative advantage" or "The Matthew Effect" its namesake taken from Matthew 25:29 which states the adage, "the rich get richer and the poor get poorer" (Outliers, 2008). 

# Questions:

**"Does a footballer's birthday affect their chances of reaching the professional level in European Football?"**

**"Does the intensity of The Matthew Effect increase, decrease, or stay the same when examining different tiers of football?"**

The cutoff dates taken from [Sports Tours](https://www.sports-tours.co.uk/european-football-age-groups) for the top five leagues are as follows:

```{r}
cutoff_dates <- data.frame(league = c("Premier League", "La Liga", "Bundesliga", "Ligue 1", "Serie A"),
                           cutoff_date = c("September 1", "January 1", "January 1", "January 1", "January 1"))
cutoff_dates %>% as_tibble() %>% 
  formattable(align = 'l')
```

Below is data scraped from [statscrew](https://www.statscrew.com/minorhockey/roster/t-10853/y-2007)

```{r}
url <- "https://www.statscrew.com/minorhockey/roster/t-10853/y-2007"
paths_allowed(url)

gladwell <- read_html(url)
gladwell <- gladwell %>% 
  html_nodes("table") %>% 
  html_table() %>% 
  as.data.frame() %>% 
  rename(player = Player,
         position = Pos.,
         dob = Birth.Date,
         height = Height,
         weight = Weight,
         sc = S.C,
         hometown = Hometown) %>% 
  mutate(foot = str_extract(height, "^\\d+'"), 
         inch = str_extract(height, "\\d+\"$"),
         foot = as.numeric(str_remove(foot, "[^\\d]")),
         inch = as.numeric(str_remove(inch, "[^\\d]")),
         height_cm = cm(foot * 12) + cm(inch)) %>% 
  separate(3, into = c("month", "day", "year")) %>% 
  separate(1, into = c("first_name", "last_name")) %>% 
  mutate(month_num = as.character(as.integer(factor(month, levels = month.name))),
         dob = make_date(year,
                         month_num,
                         day)) %>% 
  relocate(dob, .before = month) %>% 
  filter(!is.na(dob))
gladwell <- gladwell %>%
  slice(4,34,12,13,9,31,30,19,32,25,10,14,23,6,29,1,27,11,24,25,16,5,3,15,17) %>%
  mutate(name = paste(first_name, last_name),
         birthdate = paste(month_num, day, year)) %>%
  select(name, position, sc, height, weight, birthdate, hometown) %>%
  head(3)



```

The table above should resemble very closely to the graph as published in [*Outliers*](https://docs.google.com/viewer?a=v&pid=sites&srcid=ZGVmYXVsdGRvbWFpbnxhaHNicml0bmljcmVhbWVyfGd4OjJhY2JkNTEwMjkwM2RjMzM) on page 20 and 21.


Much research has already been done on the relative age effect, particulary prominent in the lower age groups. Helsen, Winckel, and Williams conducted a thorough analysis of 2,175 fútbol players playing in international club competitions strictly in Europe. Their results showed an "over-representation of players born in the first quarter of the selection year (from January to March) for all the national youth selections at the under-15 (U-15), U-16, U-17 and U-18 age categories" (Helsen, Winckel, and Williams, 2005). The purpose of this report is to mirror their analysis and substituting an older demographic of players as the subjects of the study. Very similar to the structure of Helsen, Winckel, and Williams' study, the first part of this report will simply view the distribution of players born in each month. The second part will explore in more detail characteristics such as as goals and assists contribution, FIFA score, and their market value in the 2020-2021 season and observe any patterns that may have favored birthdays closer to the cutoff date. Helsen, Winckel, and Williams concluded in their analysis on youth players that there was a statistically significant proportion of footballers born in the first three months than the last three but once the players reached the professional stage, other factors besides their birthdate were better indicators of their market value. I hypothesize the same will be true for the professional footballers I will be examining. I should note that differences in the results from these two studies should not be over analyzed as their are many confounding variables. Our datasets are from different time periods where the philosophies at the youth academy level might have changed and the solutions that Helsen, Winckel, and Williams proposed to reduce the relative age effect might have already been implemented. Coincidentally, some players might overlap between our studies. A 12-year old in 2005 would be 28 years old today. An 18-year old (the max age observed in Helsen, Winckel, and Williams' study) would be 34 years old.

# Data Collection

I intend to find data using CSV files provided by kaggle, FIFA World Cup, and datahub. Depending on the availability of data and ease of accessing a complete list of players, I intend to be as inclusive as possible within the top 5 leagues in Europe. These are the Premier League (ENG), La Liga (ESP), Bundesliga (GER), Serie A (ITA), and Ligue One (FRA). Each player will then be treated as an individual case and for the purposes of tidy data, merit its own row. Other variables to explore are their goals to game ratio, their discipline record, market worth over time, nationality, and the difference between club and country dynamics. To explore the last variable, I will isolate a data set of players who competed in the World Cup for the nation they represent. This will include many more players who haven't reached the top 5 leagues but have still competed for their country. Since the FIFA data set does not record data on player's goals/assists contribution or other more commonly analyzed statistics of talent, I intend to join the data with another data set that lists players alongside statistics relating to their match performances. I intend to employ web scraping skills when permitted by the website from a variety of football statistic websites. I found the FIFA data set to be an already comprehensive in its detail of their player ratings and the scale of players it included. However I found it incomplete in that it excluded data points that measure a player's success at the games such as goals, assists, chances created, duels won, and their disciplinary record. The data in its csv form that I found it was already in tidy format with an individual player representing a single case with a multitude of variables assigned to individal columns that described each player. The package that I found included a list of data sets for a variety of years with perfect column alignment which is conducive to a `bind_rows()` function if so desired. Since we have introduced time with this option, we could potentially explore a player's FIFA score over time. 

I will intend to use R code to create tidy tables, apply statistical reasoning, and communicate my results effectively with both textual analysis and visualizations. 

## List of Data tables: 


### Wikipedia Country Codes: 

CSV file is taken from Wikiedia and contains the country codes for each country. After renaming the columns to `alphacode3` and `en_short_name`, I selected said columns and assigned the table to `country_codes`to be later joined with other datasets in this report.
```{r}
#Loading and Cleaning data set
country_codes <- read_csv("data/country_codes.csv")

#Cleaning dataset
country_codes <- country_codes %>% 
  rename(alphacode3 = `Alpha-3 code`,
         en_short_name = `English short name lower case`) %>% 
  select(en_short_name, alphacode3)
```

### Footbal Data Set #1: FIFA 21 Players

 I have also intentionally selected only variables that we are primarily interested in our first round of exploration with birth dates. Consequently the majority of the columns (i.e. `weak_foot`, `skill_moves`, etc..) have been temporarily set aside. 


Before we dive into the data, I intend to "clean" the data and ensure the proper classes of vectors for my variables for future calculations. 

The following lists specific actions:

1. create a new variable that calclates the day of the year taken from dob. For example, January 1 will be day 1 and December 31 will be Day 365. For plotting purposes, I will be creating variables, taking information from `dob` to make a variable for `year`, `month`, and `day`, all numeric. I also created a variable that calculates the day of the year using the information from `dob`as well. 

2. I created two variables titled `quarter` and `quarter_num` which lists the quarter of the year that the player was born. The levels for this variable are "Jan-Mar", "Apri-Jun", "Jul-Sep", and "Oct-Dec". The levels for `quarter_num` are 1, 2, 3, and 4. 

3. I created a ranking column based on the player's FIFA's score and relocated the newly made `year` through `quarter_num` variables (see step 1) to be placed after `long_name`.  

4. I select only the columns I have deemed to be of interest in this report. Since the original players_21.csv file contained up to 109 variables, I have decided to exclude the majority of the variables that relate to the FIFA score, instead opting to use `overall` as an indicator of the player's talent. One last step in selecting certain columns was renaming `short_name` to `name`, `club_name` to `club`, `league_name` to `league`, `league_rank` to `league_tier_number`, and `overall` to `fifa_score`.

```{r}
fifa_cleaning <- function(csv_file){
  name <- read_csv(csv_file)
  name <- name %>% 
    mutate(year = year(ymd(dob)),
         month_num = month(ymd(dob)),
         day = day(ymd(dob)),
         month = factor(month.abb[month_num],levels=month.abb),
         day_of_year = yday(dob),
         quarter = ifelse(month_num %in% 1:3, "Jan-Mar",
                               ifelse(month_num %in% 4:6, "Apr-Jun",
                                      ifelse(month_num %in% 7:9, "July-Sep",
                                             "Oct-Dec"))),
         quarter_num = ifelse(month_num %in% 1:3, 1,
                               ifelse(month_num %in% 4:6, 2,
                                      ifelse(month_num %in% 7:9, 3,
                                             4))),
         fifa_rank = 1:nrow(name)) %>%
    relocate(`year`: `quarter_num`, .after = `long_name`) %>% 
    select(fifa_rank, sofifa_id,
         jersey = team_jersey_number,
         `long_name`: `nationality`,
         club = club_name,
         league = league_name,
         league_tier_number = league_rank,
         fifa_score = overall,
         value_eur,
         wage_eur,
         joined)
  name
}

fifa_players_15 <- fifa_cleaning("data/players_15.csv")
fifa_players_16 <- fifa_cleaning("data/players_16.csv")
fifa_players_17 <- fifa_cleaning("data/players_17.csv")
fifa_players_18 <- fifa_cleaning("data/players_18.csv")
fifa_players_19 <- fifa_cleaning("data/players_19.csv")
fifa_players_20 <- fifa_cleaning("data/players_20.csv")
fifa_players_21 <- fifa_cleaning("data/players_21.csv")

all_fifa_players <- fifa_players_15 %>% 
  bind_rows(fifa_players_16, 
            fifa_players_17, 
            fifa_players_18, 
            fifa_players_19,
            fifa_players_20,
            fifa_players_21)
all_distinct_fifa_players <- fifa_players_15 %>% 
  bind_rows(fifa_players_16, 
            fifa_players_17, 
            fifa_players_18, 
            fifa_players_19,
            fifa_players_20,
            fifa_players_21) %>% 
  distinct(long_name, .keep_all = TRUE) %>% 
  select(fifa_rank:quarter_num,
         nationality:fifa_score)
all_fifa_players %>% 
    reactable(filterable = TRUE,
            searchable = TRUE,
            minRows = 5)
```

For future use, I have saved individual datasets using our recently cleaned data set saved to `fifa_players_21`.



The `league_filter` function requires two arguments: the  name of the league and the name of the country we want to filter for. To view a list of the exact name of both the league and club as listed in the `fifa_players_21` data set, run the following. It is currently set to return all distinct club and league names in the first league tier.

```{r}
tier_filter <- function(tier_number, country){
  fifa_players_21 %>% 
    filter(league_tier_number == tier_number)
}

tier_filter(4) %>% 
  distinct(league) %>% 
  head(6) %>% 
  formattable(align = 'l')
  
tier_filter(2) %>% 
  distinct(nationality) %>% 
  head(3) %>% 
  formattable(align = 'l')
```

In the spirit of reproducibility, I made special note to filter for players that were born in the same country as the league in which they compete. For example, a player born in Sevilla (Spain) and plays for Real Madrid (Spain) would be kept whereas a player born in Marseille (France) and plays for Liverpool (England) would be dropped. This in theory will accentuate "The Matthew Effect" as we are now focusing our data set on players raised in club youth academies. I hypothesize that"The Matthew Effect" to be even more visible the lower tier club we travel. For example, the Barcelona B team in La Liga Segunda División is the feeder club into Barcelona FC but is comprised of many teenagers recently graduated from the Barcelona youth academy and transitioning into the senior team.

```{r}
#saving new data sets according to Year, League, and Country
#filter function
league_filter <- function(dataset, league, country){
  league_filtered = dataset %>% 
    filter(league == league,
           nationality == country)
  league_filtered
}

league_filter(fifa_players_21, "English Premier League", "England")

#tier 1
premier <- league_filter(fifa_players_21, "English Premier League", "England")
laliga <- league_filter(fifa_players_21, "Spain Primera Division", "Spain")
ligue1 <- league_filter(fifa_players_21, "French Ligue 1", "France")
bundesliga <- league_filter(fifa_players_21, "German 1. Bundesliga", "Germany")
seriea <- league_filter(fifa_players_21, "Italian Serie A", "Italy")

#tier2
laliga2 <- league_filter(fifa_players_21, "Spanish Segunda División", "Spain") 

#tier3
german3bundesliga <- league_filter(fifa_players_21, "German 3. BUndesliga", "Germany")

#tier4
englishleague2 <- league_filter(fifa_players_21, "English League Championship", "England")
premier %>% 
    reactable(filterable = TRUE,
            searchable = TRUE,
            minRows = 5)
```

### Football Dataset #2: Current Registered Players

After having confirmed permission from the website, I scraped the only table from [fbref.com](https://fbref.com/en/comps/Big5/stats/players/Big-5-European-Leagues-Stats#stats_standard). Regarding Data Cleansing, I performed the following in order:

1.) shifted the first row up to be the header names,

2.) removed irrelevant rows to the study by filtering out the rows with "Player",

3.) separated the `Comp` and `Age` variables.`Comp` had carried over flag information from the website. Separating `Age` will be mutated later to obtain a list of the players' age in total number of days rather than years and days.

4.) for the five leagues that I am exploring in particular, I adjusted their names to match exactly how the `fifa_players_21` data set had listed the leagues,

5.) accommodated for players who were born in the UK but are listed with their more exact nationality (i.e. Welsh, Irish),

6.) factored the `year` and `day` to be numeric,

7.) calculated a new list that describes a player's age in total number of days,

8.) the player's birth date saved as `dob` and formatted as "0000-00-00", is calculated by subtracting the number of days from today,

9.) I created four additional variables using the newly made `dob`. They are `day` (numeric), `year` (numeric), `month_num` (numeric), `month`(character), and `day_of_year` (numeric),

10.) I joined the table with Wikipedia country_codes data set on `nationality` and `alphacode3` variables.

```{r}
#Loading data set
url <- "https://fbref.com/en/comps/Big5/stats/players/Big-5-European-Leagues-Stats#stats_standard"
paths_allowed(url)
fbref <- read_html(url)
fbref <- fbref %>% 
  html_nodes("table") %>% 
  html_table() %>% 
  as.data.frame()

#cleaning and tidying data set
cur_players <- fbref                         # Duplicate data frame
colnames(cur_players) <- fbref[1, ]          # Convert first row to header
cur_players <- cur_players[-1]
cur_players <- cur_players[-1,]

current_players <- cur_players %>% 
  filter(Player != "Player") %>% 
  separate(Nation, into = c("flag.n", "nationality"), sep = " ") %>% 
  separate(Comp, into = c("flag.c", "league", "league2"), sep = " ") %>% 
  separate(Age, into = c("age.year", "age.days")) %>% 
  mutate(league = case_when(league == "Premier" ~ "English Premier League",
                            league == "Ligue" ~ "French Ligue 1",
                            league == "Serie" ~ "Italian Serie A",
                            league == "Bundesliga" ~ "German 1. Bundesliga",
                            league == "La" & league2 == "Liga" ~ "Spain Primera Division"),
         nationality = ifelse(nationality %in% c("ENG", 
                                                 "SCO",
                                                 "WAL",
                                                 "NIR"), "GBR", as.character(nationality)),
         age.year = as.numeric(as.character(age.year)),
         age.days = as.numeric(as.character(age.days)),
         today = today(),
         current_year = year(ymd(today)),
         year_born = current_year - age.year,
         born_leapyear = leap_year(year_born),
         total_age_days = age.days + 365.25 * age.year,
         dob = as.Date(-total_age_days, origin = today()),
         year = year(ymd(dob)),
         month_num = month(ymd(dob)),
         day = day(ymd(dob)),
         month = factor(month.abb[month_num],levels=month.abb),
         day_of_year = ifelse(born_leapyear == FALSE, yday(dob), yday(dob) + 1)) %>% 
  left_join(country_codes, by = c("nationality" = "alphacode3")) %>%
  select(player = Player,
       club = Squad,
       league = league,
       dob:day_of_year,
       starts = Starts,
       minutes_played = Min)
current_players %>% 
    reactable(filterable = TRUE,
            searchable = TRUE,
            minRows = 5)
```

### Football Data Set #3: World Cup Squads

Data Cleaning:

1.) parsed `b.month`,`b.day`, and `b.year` from the `Date Of Birth` variable. Calculated the `day_of_year` using `Date Of Birth` variable as well.

2.) switched the text of the team (country) names to be all capital letters. "Egypt" is now "EGYPT".

3.) created a new variable `month` using `b.month` to obtain the month abbreviation of each players' birth month.

```{r}
#Cleaning World Cup Squads
worldcupsquads <- read_csv("data/2018 FIFA World Cup Squads.csv")
worldcupsquads <- worldcupsquads %>% 
 mutate(b.month = month(ymd(`Date Of Birth`)),
        b.day = day(ymd(`Date Of Birth`)),
        b.year = year(ymd(`Date Of Birth`)),
        day_of_year = yday(`Date Of Birth`),
        `Team` = toupper(`Team`),
         month = month.abb[as.numeric(b.month)])

worldcupsquads %>% 
  reactable(filterable = TRUE,
            searchable = TRUE,
            minRows = 5)

```

Let us state our expectation of how the "Matthew Effect" will affect the distribution of player birthdays in professional football. This accumulative effect  would predict a higher proportion of professional footballers being born in the earlier parts of the year and in England's case, closer to September 1. So, what does the data reveal? Explicitly stated, will we see more players born closer to the cutoff date or will we see a flatter line where players are evenly distributed across each months?

# Bar Graphs
### First Tier Leagues

Our first round of inquiry will produce frequency tables of football birthdays by quarter. We will first examine our entire data set `all_distinct_players` of all unique players to have a registered FIFA profile from 2017 to the end of the 2020-2021 season. We will then be narrowing our research by examining the Premier League and La Liga.


```{r}
bar_plot <- function(dataset, subtitle, move_y_down){
  colors <- wes_palette("GrandBudapest1",4)
  dataset %>% 
  group_by(quarter) %>% 
  mutate(n_quarter = n()) %>% 
  arrange(n_quarter) %>% 
  ggplot() +
  aes(x = reorder(quarter, quarter_num),
      fill = quarter) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Fútbol Player Birthdays across the Calendar Year",
       subtitle = subtitle,
       x = "Quarter",
       y = "Number of Players",
       caption = "Source: sofifa") +
  geom_text(aes(label = ..count..),
            stat = "count",
            nudge_x = 0,
            nudge_y = move_y_down,
            colour = "white",
            size = 3) +
    scale_fill_manual(values = colors)
}

bar_plot(all_distinct_fifa_players, "All FIFA Players from 2015 to 2021", -800)
bar_plot(premier, "Premier League Players", -20)
bar_plot(laliga, "La Liga Players", -20)
```

From the second graph above, we see that 444 Premier League Players were born in the first quarter (Jan-Mar) wheras 494 were born in the last quarter (Oct-Dec).

In the third graph, we see that the La Liga has produced different results than the Premier League, the hypothesis being a different cutoff date, January 1 for the former and September 1 for the latter. We see from our results that 372 La Liga Spanish players were born in the first quarter whereas 185 were born in the last quarter.

In our second round of inquiry, we will be creating a frequency table of player birthdays by month and subsequently creating a bar chart.

Functions:
```{r}
month_frequency_table <- function(dataset){
  dataset <- dataset %>% 
    group_by(month) %>% 
    count()
  dataset <- dataset %>% 
    reactable(defaultSorted = c("month", "n"),
            filterable = TRUE,
            searchable = TRUE,
            minRows = 6)
  dataset
}

month_bar_graph <- function(dataset, subtitle){
  colors2 <- c("seagreen", "seagreen", "seagreen", "thistle3", "thistle3", "thistle3", "turquoise4", "turquoise4", "turquoise4", "orangered3", "orangered3", "orangered3")
  dataset %>% 
  group_by(month) %>% 
  mutate(n_month = n()) %>% 
  arrange(n_month) %>% 
  ggplot() +
  aes(x = reorder(month, month_num),
      fill = month) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of Fútbol Player Birthdays across the Calendar Year",
       subtitle = subtitle,
       x = "Birth Month", 
       y = "Number of Players",
       caption = "Source: sofifa") +
  geom_text(aes(label = ..count..), 
            stat = "count", 
            nudge_x = 0, 
            nudge_y = -5, 
            colour = "white", 
            size = 3) +
    scale_fill_manual(values = colors2)
}

month_frequency_table(fifa_players_21)
month_frequency_table(premier)
month_frequency_table(laliga)
month_bar_graph(premier, "Premier League Players")
month_bar_graph(laliga, "La Liga Players")
```

### Lower Divisions

We repeat the process from above with lower divisions. I have chosen La Liga Segunda División, German 3. Bundesliga, and English Premier League Two as the representatives for the 2nd, 3rd, and 4th tiers respectively. My hypothesis is that . My reasoning for this is that lower league clubs, with significantly less money to spend on talent across the globe will turn to youth academies and homegrown talent. In the case of many clubs, their youth academy of U-18 *is* their representing club in the lower leagues. In other words, I am predicting that money will mitigate The Matthew Effect the farther down the league tier you travel.

```{r}
month_bar_graph(laliga2, "Spanish Segunda Division Players")
month_bar_graph(german3bundesliga, "Bundesliga 3 Division Players")
month_bar_graph(englishleague2, "English League 2 Players")
```

### World Cup data set

```{r}
worldcupsquads %>% 
  group_by(month) %>% 
  mutate(n_month = n()) %>% 
  arrange(n_month) %>% 
  ggplot() +
  aes(x = reorder(month, b.month),
      fill = month) +
  geom_bar() +
  theme(legend.position = "none") +
  theme_minimal() +
  labs(title = "2018 World Cup Squad Birth Months",
       x = "Birth Month", 
       y = "Number of Players",
       caption = "Source: kaggle") +
  geom_text(aes(label = ..count..), 
            stat = "count", 
            nudge_x = 0, 
            nudge_y = -1.6, 
            colour = "white", 
            size = 3) +
  scale_fill_manual(values = c("wheat3", "wheat4", "peachpuff4", "wheat3", "wheat4", "peachpuff4", "wheat3", "wheat4", "peachpuff4", "wheat3", "wheat4", "peachpuff4")) +
  theme(legend.position = "none")
```

# Regressions

### Scatter Plots
```{r}
fifa_reg_function <- function(dataset){
  fifa_reg <- dataset %>% 
  mutate(league.2 = ifelse(league == "Spain Primera Division", "La Liga", "all others"),
         is_premier = ifelse(str_detect(league, "English"), "Premier", "All Other Leagues")) %>% 
  filter(day_of_year != 60) %>% 
  group_by(day_of_year) %>%  
  summarize(count_per_day = n(),
            league,
            club,
            is_premier) 
}

fifa_reg_scatter_plot <- function(dataset, subtitle){
  fifa_reg <- fifa_reg_function(dataset)
  fifa_reg %>% 
  select(day_of_year,
         count_per_day) %>% 
  distinct(day_of_year, .keep_all = TRUE) %>% 
  ggplot() +
  aes(x = day_of_year,
      y = count_per_day) +
  geom_point(color = "#CC79A7",
             alpha = .5) +
  geom_smooth(color = "black") +
  labs(title = "Number of Players born each day of the year",
       subtitle = subtitle,
       x = "Day of the Year (i.e. January 1 = 1)",
       y = "Number of footballer birthdays",
       caption = "Source: FIFA",
       color = "Premier v. All Other Leagues") +
  theme_minimal()
}
fifa_reg_scatter_plot(all_distinct_fifa_players, "from FIFA 2021 Dataset of 41,975 players")
fifa_reg_scatter_plot(bundesliga, "from FIFA 2021 Bundesliga Dataset of 1,189 players")
fifa_reg_scatter_plot(current_players,"from Current Players Dataset of 2,853 players")
```

### Regression Analysis using a fitted model
```{r}
regression <- function(dataset){
  table <- fifa_reg_function(dataset)
  regression <- lm(count_per_day ~ day_of_year, data = table)
  regression %>% 
  summary() %>% 
  pander()
}
regression(all_distinct_fifa_players)
regression(current_players)
regression(fifa_players_21)
regression(fifa_players_15)
```

Our regression analysis fitted a model using `day_of_year` as a predictor for the `count_per_day`. As expected from our previous bar plots, we obtained a negative slope of "-.086." This model would suggest that after taking a sample of all registered FIFA players at the end of the 2020-2021 season (dataset is `fifa_players_21`), it is expected that there be .086 fewer players born on each day of the year as you travel forward on the calendar timeline starting with January 1. The y-intercept has little significance in our analysis in that it is impossible to be born on the 0th day of the year. The best interpretation of it then is that January 1, as predicted by the model, had marginally fewer than 60.23 players born on that date. The same analsysis can be repeated for whatever data set you choose.

# Conclusions

Our results seem to suggest that The Matthew Effect is at play . What I find telling from the results is the distinction between the Premier League and all other leagues. We would have reason to believe The Matthew Effect is influencing our results if all players were born, but it is even more compelling that upon switching the cutoff date, we can see the higher proportion of players born closer to the cutoff date rather than January 1. In other words, holding other variables constant and changing just one variable, namely the cutoff date, we have shown how the cutoff date has influenced the distribution of footballer birthdays across the calendar year. We have worked to dilute the effects of confounding variables in this way. Thus far, we have arrived at superficial evidence that the relative age theory is more prominent in the lower leagues by comparing the distribution between La Liga Primera Divisoin and La Liga Segunda Division. 

This study is vastly incomplete and scrapes the tip of the iceberg of what we could explore with The Matthew Effect. Further analysis could include how well of predictor a footballer's birth date is to their future career beyond just making it to the professional level. This could be measured by their market value (`value_euro`), FIFA score  over time, minutes played (`fifa_score`), or their career statistics (goals and assists). As Helsen, Winckel, and Williams have already explored, it appears that the correlation between birth dates and these measures of success are much weaker than simply making it to professional football.

# Sources:

### Links:

[kaggle](https://www.kaggle.com/datasets/stefanoleone992/fifa-20-complete-player-dataset?select=players_20.csv)

[FIFA World Cup](https://github.com/pratapvardhan/FIFAWorldCup/blob/master/squads.csv)

[datahub](https://datahub.io/collections/football)

[Sports Tours](https://www.sports-tours.co.uk/european-football-age-groups)

[Helsen, Winckel, Williams](https://www.researchgate.net/publication/7570079_The_relative_age_effect_in_youth_soccer_across_Europe)

###  Citations: 

Gladwell, Malcolm. *Outliers*. Little, Brown and Company, 2018.

Helsen, Werner & Winckel, Jan & Williams, Andrew. (2005). The relative age effect in youth soccer across Europe. Journal of sports sciences. 23. 629-36. 10.1080/02640410400021310.
