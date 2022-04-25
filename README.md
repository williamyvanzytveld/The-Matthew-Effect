# The-Matthew-Effect
Exploring "The Matthew Effect" in European Football.
---
title: 'Project 3 : The Matthew Effect'
author: "William Y. VanZytveld"
date: "4/26/2022"
output: html_document
---
```{r, echo = FALSE}
library(tidyverse)
library(lubridate)
library(robotstxt)
library(tidytext)
library(rvest)
library(htmlTable)
library(rio)
library(formattable)
library(openintro)
library(pander)
```


#### Initial Premise

As a longtime avid fútbol fan, I am interested in exploring the effects of birth dates on a player's career. This question is prompted by reading Malcolm Gladwell's, *Outliers* in which he supports his relative age theory with incredible examples of how many professional athletes, particularly in Hockey, have an unusually large proportion of birthdays clustered in the beginning three months. One might even say it is statistically significant. The theory is that during adolescent years of development, children born near the cut off date (i.e. January 1) are more physically developed than their December birthday counterparts. This results in promotion to more competitive age groups and more attention in development. Malcolm labeled it, "accumulative advantage" or "The Matthew Effect" its namesake taken from Matthew 25:29 which states the adage, "the rich get richer and the poor get poorer" (Outliers, 2008). 

The cutoff dates taken from [Sports Tours](https://www.sports-tours.co.uk/european-football-age-groups) for the top five leagues are as follows:

```{r}
cutoff_dates <- data.frame(league = c("Premier League", "La Liga", "Bundesliga", "Ligue 1", "Serie A"),
                           cutoff_date = c("September 1", "January 1", "January 1", "January 1", "January 1"))
cutoff_dates %>% as.tibble() %>% 
  formattable(align = 'l')
```

Below is data scraped from [statscrew](https://www.statscrew.com/minorhockey/roster/t-10853/y-2007)

```{r}
url <- "https://www.statscrew.com/minorhockey/roster/t-10853/y-2007"
paths_allowed(url)

library("httr")
table <- tempfile(fileext = ".html")
GET(url = url, user_agent("Mozilla/5.0"), write_disk(table))

library("XML")
df <- readHTMLTable(table)
df <- as.data.frame(df)
gladwell <- df %>% 
  rename(player = NULL.Player,
         position = NULL.Pos.,
         dob = NULL.Birth.Date,
         height = NULL.Height,
         weight = NULL.Weight,
         sc = NULL.S.C,
         hometown = NULL.Hometown) %>% 
  mutate(foot = str_extract(height, "^\\d+'"), 
         inch = str_extract(height, "\\d+\"$"),
         foot = as.numeric(str_remove(foot, "[^\\d]")),
         inch = as.numeric(str_remove(inch, "[^\\d]")),
         height_cm = cm(foot * 12) + cm(inch)) %>% 
  separate(3, into = c("month", "day", "year")) %>% 
  separate(1, into = c("first_name", "last_name")) %>% 
  mutate(day = as.integer(factor(day)),
         year = as.integer(factor(year)),
         month_num = as.integer(factor(month, levels = month.name)),
         dob = make_date(month_num,
                         day,
                         year)) %>% 
  relocate(dob, .before = month) %>% 
  filter(!is.na(dob))

gladwell %>% 
  head(5)
```


```{r}
url <- "https://www.statscrew.com/minorhockey/roster/t-10853/y-2006"
gladwell <- read_html(url)
gladwell <- gladwell %>% 
  html_nodes("table") %>% 
  html_table() %>%
  as.data.frame() %>% 
  rename(player = Player,
         position = Pos.,
         birth.date = Birth.Date,
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
  mutate(day = as.integer(as.character(day)),
         year = as.integer(as.character(year)),
         month_num = as.integer(factor(month, levels = month.name)),
         dob = make_date(year,
                         month_num,
                         day),
         month_abb = month.abb[as.integer(month_num)]) %>% 
  relocate(dob, .before = month)
gladwell %>% 
  slice(4,34,12,13,9,31,30,19,32,25,10,14,23,6,29,1,27,11,24,25,16,5,3,15,17) %>% 
  mutate(name = paste(first_name, last_name),
         birthdate = paste(month_abb, day, year)) %>% 
  select(name, position, sc, height, weight, birthdate, hometown)
```

The table above should resemble very closely to the graph as published in [*Outliers*](https://docs.google.com/viewer?a=v&pid=sites&srcid=ZGVmYXVsdGRvbWFpbnxhaHNicml0bmljcmVhbWVyfGd4OjJhY2JkNTEwMjkwM2RjMzM) on page 20 and 21.


Much research has already been done on the relative age effect, particulary prominent in the lower age groups. Helsen, Winckel, and Williams conducted a thorough analysis of 2,175 fútbol players playing in international club competitions strictly in Europe. Their results showed an "over-representation of players born in the first quarter of the selection year (from January to March) for all the national youth selections at the under-15 (U-15), U-16, U-17 and U-18 age categories" (Helsen, Winckel, and Williams, 2005). The purpose of this report is to mirror their analysis and substituting an older demographic of players as the subjects of the study. Very similar to the structure of Helsen, Winckel, and Williams' study, the first part of this report will simply view the distribution of players born in each month. The second part will explore in more detail characteristics such as as goals and assists contribution, FIFA score, and their market value in the 2020-2021 season and observe any patterns that may have favored birthdays closer to the cutoff date. Helsen, Winckel, and Williams concluded in their analysis on youth players that there was a statistically significant proportion of footballers born in the first three months than the last three but once the players reached the professional stage, other factors besides their birthdate were better indicators of their market value. I hypothesize the same will be true for the professional footballers I will be examining. I should note that differences in the results from these two studies should not be over analyzed as their are many confounding variables. Our datasets are from different time periods where the philosophies at the youth academy level might have changed and the solutions that Helsen, Winckel, and Williams proposed to reduce the relative age effect might have already been implemented. Coincidentally, some players might overlap between our studies. A 12-year old in 2005 would be 28 years old today. An 18-year old (the max age observed in Helsen, Winckel, and Williams' study) would be 34 years old.

#### Data Collection

I intend to find data using CSV files provided by kaggle, FIFA World Cup, and datahub. Depending on the availability of data and ease of accessing a complete list of players, I intend to be as inclusive as possible within the top 5 leagues in Europe. These are the Premier League (ENG), La Liga (ESP), Bundesliga (GER), Serie A (ITA), and Ligue One (FRA). Each player will then be treated as an individual case and for the purposes of tidy data, merit its own row. Other variables to explore are their goals to game ratio, their discipline record, market worth over time, nationality, and the difference between club and country dynamics. To explore the last variable, I will isolate a data set of players who competed in the World Cup for the nation they represent. This will include many more players who haven't reached the top 5 leagues but have still competed for their country. Since the FIFA data set does not record data on player's goals/assists contribution or other more commonly analyzed statistics of talent, I intend to join the data with another data set that lists players alongside statistics relating to their match performances.

Since the inception of the project and initial data collection (Project 1), I have been able to acquire additional data sets that I expect to be helpful. I intend to employ web scraping skills when permitted by the website from a variety of football statistic websites. I found the FIFA data set to be an already comprehensive in its detail of their player ratings and the scale of players it included. However I found it incomplete in that it excluded data points that measure a player's success at the games such as goals, assists, chances created, duels won, and their disciplinary record. The data in its csv form that I found it was already in tidy format with an individual player representing a single case with a multitude of variables assigned to individal columns that described each player. The package that I found included a list of data sets for a variety of years with perfect column alignment which is conducive to a `bind_rows()` function if so desired. Since we have introduced time with this option, we could potentially explore a player's FIFA score over time. 

I will continue to use R code to create tidy tables, apply statistical reasoning, and communicate my results effectively with both textual analysis and visualizations. 

### List of Data tables: 

After loading the `players_21.csv` file, I have created separate tables for each league. In the spirit of reproducibility, I made special note to filter for players that were born in the same country as the league in which they compete. For example, a player born in Sevilla (Spain) and plays for Real Madrid (Spain) would be kept whereas a player born in Marseille (France) and plays for Liverpool (England) would be dropped. This in theory will accentuate "The Matthew Effect" as we are now focusing our data set on players raised in club youth academies. I hypothesize that"The Matthew Effect" to be even more visible the lower tier club we travel. For example, the Barcelona B team in La Liga Segunda División is the feeder club into Barcelona FC but is comprised of many teenagers recently graduated from the Barcelona youth academy and transitioning into the senior team. I have also intentionally selected only variables that we are primarily interested in our first round of exploration with birth dates. Consequently the majority of the columns (i.e. `weak_foot`, `skill_moves`, etc..) have been temporarily set aside. 


Before we dive into the data, I intend to "clean" the data and ensure the proper classes of vectors for my variables for future calculations. 

The following lists specific actions:

1. create a new variable that calclates the day of the year taken from dob. For example, January 1 will be day 1 and December 31 will be Day 365.

2. I have separated the `dob` column into three separate columns: b.year, b.month, and b.day. I proceed to make year numeric, translate the month column into abbreviations (i.e. Jan), and make year numeric.

3. I combine my newly made columns back into the original `dob` column and factor it as a date.

4. I select only the columns I have deemed to be of interest in this report. Since the original players_21.csv file contained up to 109 variables, I have decided to exclude the majority of the variables that relate to the FIFA score, instead opting to use `overall` as an indicator of the player's talent. One last step in selecting certain columns was renaming `short_name` to `name`, `club_name` to `club`, `league_name` to `league`, `league_rank` to `league_tier_number`, and `overall` to `fifa_score`.


Let us state our expectation of how the "Matthew Effect" will affect the distribution of player birthdays in professional football. This accumulative effect  would predict a higher proportion of professional footballers being born in the earlier parts of the year and in England's case, closer to September 1. So, what does the data reveal? Explicitly, will we see more players born closer to the cutoff date or will we see a flatter line where players are evenly distributed across each months?

```{r}
fifa_players_21 <- read_csv("data/players_21.csv") 
fifa_players_21 <- fifa_players_21 %>% 
  separate(`dob`, into = c("b.year", "b.month", "b.day"), sep = "-") %>% 
  mutate(month_num = as.numeric(as.character(b.month)),
         year = as.numeric(b.year),
         month = month.abb[as.numeric(b.month)],
         day = as.numeric(b.day),
         dob = make_date(year, 
                         month_num, 
                         day),
         day_of_year = yday(dob),
         fifa_rank = 1:nrow(fifa_players_21)) %>% 
  select(fifa_rank, sofifa_id,
         jersey = team_jersey_number,
         long_name,
         age,
         dob,
         year,
         month,
         day,
         month_num,
         day_of_year,
         height_cm,
         weight_kg,
         nationality,
         club = club_name,
         league = league_name,
         league_tier_number = league_rank,
         fifa_score = overall,
         value_eur,
         wage_eur,
         joined)

players_test <- fifa_players_21 %>% 
  slice(1:100)
view(players_test)
```

For future use, I have saved individual datasets using our recently cleaned data set saved to `fifa_players_21`. 

```{r}
premier <- fifa_players_21 %>% 
  filter(league == "English Premier League",
         nationality == "England")

laliga <- fifa_players_21 %>% 
  filter(league == "Spain Primera Division",
         nationality == "Spain")

ligue1 <- fifa_players_21 %>% 
  filter(league == "French Ligue 1",
         nationality == "France")

bundesliga <- fifa_players_21 %>% 
  filter(league == "German 1. Bundesliga",
         nationality == "Germany")

seriea <- fifa_players_21 %>% 
  filter(league == "Italian Serie A",
         nationality == "Italy")


filter_league <- function(league_name, country){
  filtered_fifa <- fifa_players_21 %>% 
    filter(league == "league_name",
           nationality == "country")
  return(filtered_fifa)
}

premier %>% 
  head(5)
laliga %>% 
  head(5)
ligue1 %>% 
  head(5)
bundesliga %>% 
  head(5)
seriea %>% 
  head(5)


worldcupsquads <- read_csv("data/2018 FIFA World Cup Squads.csv")
view(worldcupsquads)
worldcupsquads %>% 
  rename(team = `Team`,
         group = `Group`,
         squad_number = `Squad Number`,
         field_position = 4, 
         name = 5,
         dob = 6,
         age = 7,
         caps = 8,
         goals = 9,
         club = 10,
         player_count = 11) %>% 
  select(team, name, field_position, dob, age, goals) %>% 
  filter(team == "Argentina") %>% 
  arrange(desc(goals)) %>% 
  head(5)
```

###### Dataset #2:

```{r}
url <- "https://fbref.com/en/comps/Big5/stats/players/Big-5-European-Leagues-Stats#stats_standard"
paths_allowed(url)
fbref <- read_html(url) 
fbref <- fbref %>% 
  html_nodes("table") %>% 
  html_table() %>% 
  as.data.frame()

cur_players <- fbref                         # Duplicate data frame
colnames(cur_players) <- fbref[1, ]          # Convert first row to header
cur_players <- cur_players[-1]
cur_players <- cur_players[-1,]
```

##### Loading Wikipedia Country Codes: 

```{r}
country_codes <- read_csv("data/country_codes.csv") %>% 
  rename(alphacode3 = 3) %>% 
  select(1,3)

```


```{r}
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
         total_age_days = age.days + 365.25 * age.year,
         dob = as.Date(-total_age_days, origin = today())) %>% 
  separate(dob, into = c("b.year", "b.month", "b.day"), sep = "-") %>% 
  mutate(month_num = as.numeric(as.character(b.month)),
         year = as.numeric(b.year),
         month = month.abb[as.numeric(b.month)],
         day = as.numeric(b.day),
         dob = make_date(year, 
                         month_num, 
                         day),
         day_of_year = yday(dob)) %>% 
    left_join(country_codes, by = c("nationality" = "alphacode3"))
  # select(player = Player,
  #        club = Squad,
  #        league = league,
  #        dob,
  #        starts = Starts,
  #        minutes_played = Min)
```


Our first round of inquiry begins with creating a frequency table of player birthdays by month and subsequently creating a bar chart.

```{r}
premier %>% 
  group_by(month) %>% 
  mutate(n_month = n()) %>% 
  arrange(n_month) %>% 
  ggplot() +
  aes(x = reorder(month, month_num),
      fill = month) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Premier League Players Birth Months",
       x = "Birth Month", 
       y = "Number of Players",
       caption = "Source: sofifa") +
  geom_text(aes(label = ..count..), 
            stat = "count", 
            nudge_x = 0, 
            nudge_y = -1.6, 
            colour = "white", 
            size = 3)
```


```{r}
laliga %>% 
  group_by(month) %>% 
  mutate(n_month = n()) %>% 
  arrange(n_month) %>% 
  ggplot() +
  aes(x = reorder(month, month_num),
      fill = month) +
  geom_bar() +
  theme(legend.position = "none") +
  theme_minimal() +
  labs(title = "La Liga Players Birth Months",
       x = "Birth Month", 
       y = "Number of Players",
       caption = "Source: sofifa") +
  geom_text(aes(label = ..count..), 
            stat = "count", 
            nudge_x = 0, 
            nudge_y = -1.6, 
            colour = "white", 
            size = 3)


ligue1 %>% 
  group_by(month) %>% 
  mutate(n_month = n()) %>% 
  arrange(n_month) %>% 
  ggplot() +
  aes(x = reorder(month, month_num),
      fill = month) +
  geom_bar() +
  theme(legend.position = "none") +
  theme_minimal() +
  labs(title = "Ligue 1 Players Birth Months",
       x = "Birth Month", 
       y = "Number of Players",
       caption = "Source: sofifa") +
  geom_text(aes(label = ..count..), 
            stat = "count", 
            nudge_x = 0, 
            nudge_y = -1.6, 
            colour = "white", 
            size = 3)

bundesliga %>% 
  group_by(month) %>% 
  mutate(n_month = n()) %>% 
  arrange(n_month) %>% 
  ggplot() +
  aes(x = reorder(month, month_num),
      fill = month) +
  geom_bar() +
  theme(legend.position = "none") +
  theme_minimal() +
  labs(title = "Bundesliga Players Birth Months",
       x = "Birth Month", 
       y = "Number of Players",
       caption = "Source: sofifa") +
  geom_text(aes(label = ..count..), 
            stat = "count", 
            nudge_x = 0, 
            nudge_y = -1.6, 
            colour = "white", 
            size = 3)

seriea %>% 
  group_by(month) %>% 
  mutate(n_month = n()) %>% 
  arrange(n_month) %>% 
  ggplot() +
  aes(x = reorder(month, month_num),
      fill = month) +
  geom_bar() +
  theme(legend.position = "none") +
  theme_minimal() +
  labs(title = "Serie A Players Birth Months",
       x = "Birth Month", 
       y = "Number of Players",
       caption = "Source: sofifa") +
  geom_text(aes(label = ..count..), 
            stat = "count", 
            nudge_x = 0, 
            nudge_y = -1.6, 
            colour = "white", 
            size = 3)
```




### Lower Divisions

We repeat the process from above with lower divisions. I have chosen La Liga Segunda División, German 3. Bundesliga, and English Premier League Two as the representatives for the 2nd, 3rd, and 4th tiers respectively.

```{r}
laliga2 <- fifa_players_21 %>% 
  filter(league == "Spanish Segunda División")
laliga2 %>% 
  group_by(month) %>% 
  mutate(n_month = n()) %>% 
  arrange(n_month) %>% 
  ggplot() +
  aes(x = reorder(month, month_num),
      fill = month) +
  geom_bar() +
  theme(legend.position = "none") +
  theme_minimal() +
  labs(title = "La Liga Segunda División Players Birth Months",
       x = "Birth Month", 
       y = "Number of Players",
       caption = "Source: sofifa") +
  geom_text(aes(label = ..count..), 
            stat = "count", 
            nudge_x = 0, 
            nudge_y = -1.6, 
            colour = "white", 
            size = 3)
  

englishleague2 <- fifa_players_21 %>% 
  filter(league == "English League Two")
englishleague2 %>% 
  group_by(month) %>% 
  mutate(n_month = n()) %>% 
  arrange(n_month) %>% 
  ggplot() +
  aes(x = reorder(month, month_num),
      fill = month) +
  geom_bar() +
  theme(legend.position = "none") +
  theme_minimal() +
  labs(title = "English League Two Players Birth Months",
       x = "Birth Month", 
       y = "Number of Players",
       caption = "Source: sofifa") +
  geom_text(aes(label = ..count..), 
            stat = "count", 
            nudge_x = 0, 
            nudge_y = -1.6, 
            colour = "white", 
            size = 3)

german3bundesliga <- fifa_players_21 %>% 
  filter(league == "German 3. Bundesliga")
german3bundesliga %>% 
  group_by(month) %>% 
  mutate(n_month = n()) %>% 
  arrange(n_month) %>% 
  ggplot() +
  aes(x = reorder(month, month_num),
      fill = month) +
  geom_bar() +
  theme(legend.position = "none") +
  theme_minimal() +
  labs(title = "German 3 Bundesliga",
       x = "Birth Month", 
       y = "Number of Players",
       caption = "Source: sofifa") +
  geom_text(aes(label = ..count..), 
            stat = "count", 
            nudge_x = 0, 
            nudge_y = -1.6, 
            colour = "white", 
            size = 3)
```










##### Sources:

[kaggle](https://www.kaggle.com/datasets/stefanoleone992/fifa-20-complete-player-dataset?select=players_20.csv)

[FIFA World Cup](https://github.com/pratapvardhan/FIFAWorldCup/blob/master/squads.csv)

[datahub](https://datahub.io/collections/football)

[Sports Tours](https://www.sports-tours.co.uk/european-football-age-groups)

[Helsen, Winckel, Williams](https://www.researchgate.net/publication/7570079_The_relative_age_effect_in_youth_soccer_across_Europe)


##### Citations: 

Gladwell, Malcolm. *Outliers*. Little, Brown and Company, 2018.


Helsen, Werner & Winckel, Jan & Williams, Andrew. (2005). The relative age effect in youth soccer across Europe. Journal of sports sciences. 23. 629-36. 10.1080/02640410400021310. 


```{r}
fifa_reg <- fifa_players_21 %>%
  filter(day_of_year != 60) %>% 
  group_by(day_of_year) %>% 
  summarize(count_per_day = n(),
            league,
            club)
fifa_reg %>% 
  ggplot() +
  aes(x = day_of_year,
      y = count_per_day) +
  geom_point() +
  labs(title = "Number of Players born each day of the year",
       subtitle = "from FIFA 2021 Dataset of 18,944 players ",
       x = "Day of the Year (i.e. January 1 = 1)",
       y = "Number of footballer birthdays")
```



Regressions

```{r}
fifa_regression <- lm(count_per_day ~ day_of_year, data = fifa_reg)
fifa_regression %>% 
  pander()

fifa_regression %>% 
  summary()

fifa_reg %>% 
  ggplot() +
  aes(x = day_of_year,
      y = count_per_day) +
  geom_point()


fifa_month_reg <- fifa_players_21 %>% 
  group_by(month_num) %>% 
  summarize(count_per_month = n())
fifa_month_reg

fifa_regression_month <- lm(count_per_month ~ month_num, data = fifa_month_reg)
fifa_regression_month
```

