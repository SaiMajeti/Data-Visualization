library(dslabs)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(tidyselect)
library(ggstance)
library(lubridate)
library(kableExtra)

#read dwguide.csv
dwguide <- read.csv2("dwguide.csv", header = TRUE, sep = ",")

#read imdb_details.csv
imdb <- read.csv2("imdb_details.csv", header = TRUE, sep = ",")

#read all-detailsepisodes
doctornum <- read.csv2("all-detailsepisodes.csv", header = TRUE, sep = ",")

#a brief look into the datasets
glimpse(dwguide)
glimpse(imdb)
glimpse(doctornum)

#select required columns from each dataset
dwsmall <- select(dwguide, episodenbr:chart) %>%
  filter(episodenbr > 696) %>%
  mutate_each(funs(toupper)) #all in upper case letters

ratings <- select(imdb, number, title, rating, nbr_votes, season ) %>%
  mutate_each(funs(toupper)) #all in upper case letters

doctornum <- doctornum %>%
  mutate_each(funs(toupper)) %>%
  arrange(doctorid) %>%
  filter(doctorid == 9| doctorid == 10| doctorid == 11 | doctorid == 12 | doctorid ==13)


#new small data files brief
glimpse(dwsmall)
glimpse(ratings)

#join first two datasets
drwho <- left_join(dwsmall, ratings, by = "title") %>%
  arrange(( episodenbr))

#join drwho with dr. id -- final table we are going to use which contains all req. details now
drid <- left_join(drwho, doctornum, by = "title" ) %>%
  mutate(episode = paste(season, number, sep = '-')) %>%
  select(episode, season, number, everything())

#change classes of numerical columns 
drid$season <- as.numeric(as.character(drid$season))
drid$rating <- as.numeric(as.character(drid$rating))
drid$number <- as.numeric(as.character(drid$number))
drid$AI <- as.numeric(as.character(drid$AI))
drid$chart <- as.numeric(as.character(drid$chart))
drid$nbr_votes <- as.numeric(as.character(drid$nbr_votes))
drid$views <- as.numeric(sapply(strsplit(drid$views, "M"), "[[", 1)) #remove 'M' and 
names(drid)[names(drid) == 'views'] <- 'viewsinM'

#number of NAs for each column
sampletable <- drwho %>%
  summarise_all(funs(sum(is.na(.))))%>%
  filter_all(any_vars(. > 1)) %>%
  kable(lable = "No. of missing values is each column")

sampletable

#missingvalues to look at the difference in the number of observations
missing <- filter(drwho, is.na(number))
missing$title


#summary of ratings column
summary(select(drid, rating))

# let's see the distribution of episode ratings to confirm the above
ratingsdist <- drid %>% 
  ggplot(aes(y = "", x = rating)) + 
  geom_boxploth()

ratingsdist

#avg. rating for each season
seasonrating <- drid %>%
  select(season, rating) %>%
  filter(!is.na(rating)) %>%
  group_by(season) %>%
  summarize(avg_rating = mean(rating)) %>%
  mutate_each(funs(round(., 2)), avg_rating) #round to 2 decimals

kable(seasonrating)

# average ratings plot - to see season with highest avg. rating
ratingplot <- ggplot(seasonrating, aes(season, avg_rating)) +
  geom_path(colour = "red", size = 1) +
  scale_x_continuous(limits = c(1, 11), breaks = seq(1, 11, 1)) +
  scale_y_continuous(limits = c(1,10), breaks = seq(1, 10, 1))+
  geom_hline(aes(yintercept = max(avg_rating)) , linetype = "dashed", col = "blue") +
  geom_text(aes(y = avg_rating[4]+ 0.5, x= season[4], label = max(avg_rating)))

ratingplot


#first, let me see the overall plot of the each episode ratings
episoderating <- drid %>%
  ggplot(aes(episode, rating)) +
  geom_point() + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
episoderating 

lowrated <- drid %>%
  filter(rating < 7) %>% #less than the first quartile
  ggplot(aes(episode, rating)) + 
  geom_point(colour = "red")

lowrated

plot <- drid %>%
  filter(!is.na(doctorid), !is.na(rating)) %>%
  ggplot(aes(colour = doctorid)) +
  geom_boxplot(aes(doctorid, rating)) 
plot

doctorrating <- drid %>%
  filter(!is.na(rating), !is.na(doctorid), !is.na(episodenbr), !is.na(viewsinM), !is.na(nbr_votes)) %>%
  group_by(doctorid) %>%
  summarise(avgrat = mean(rating),
            numepi = n(),
            avgviews = mean(viewsinM),
            views_md = median(viewsinM),
            views_mx = max(viewsinM),
            avgvotes = mean(nbr_votes),
            votes_md = median(nbr_votes),
            votes_mx = max(nbr_votes),
  ) %>%
  mutate_each(funs(round(., 2)), avgrat, avgviews, views_md)%>%
  mutate_each(funs(round(.,0)), avgvotes, votes_md, votes_mx) %>%
  arrange(desc(avgrat))

kable(doctorrating)


#no.of episodes aired vs. day of the week and avg. rating

day <- drid %>%
  filter(!is.na(weekday),!is.na(episodenbr),!is.na(rating))%>%
  group_by(weekday)%>%
  summarise(numepisodes = n(),
            avgrating = mean(rating),
            maxrating = max(rating)) %>%
  arrange(desc(numepisodes))

kable(day)

drid1 <- drid %>%  #get month of broadcast
  mutate(broadcastmonth = month(dmy(drid$broadcastdate), label = TRUE)) %>% 
  filter(!is.na(doctorid)) %>%
  group_by(broadcastmonth) %>%
  ggplot() +
  geom_point(aes(number, rating, color = doctorid)) +
  facet_wrap(~broadcastmonth)

drid1

#facet rating, AI for each seasons-episode-doctor; 

#Question:1
facet <- drid %>%
  filter(!is.na(season), !is.na(number), !is.na(rating), !is.na(doctorid)) %>%
  ggplot(aes(number, rating, color = doctorid, alpha = AI), na.rm = TRUE) + 
  geom_jitter() +
  facet_wrap(~season, shrink = FALSE) 
facet

#2nd facet
facet2 <- drid %>%
  filter(!is.na(season), !is.na(number), !is.na(viewsinM), !is.na(doctorid)) %>%
  ggplot(aes(number, viewsinM, color = doctorid, size = nbr_votes), na.rm = TRUE) + 
  geom_point() +
  facet_wrap(~season, shrink = TRUE, ncol = 4)

facet2

# seasons and chart rankings
chartrankings <- drid %>%
  select(season, chart) %>%
  filter(!is.na(chart), !is.na(season)) %>%
  group_by(season) %>%
  summarise(topchart = min(chart)) %>%
  arrange(topchart)
kable(chartrankings)

chart11 <- drid %>%
  filter(chart == 1 & season == 11)

kable(chart11)