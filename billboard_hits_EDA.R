
#### EDA of billboard hit songs from the year 2000 ####

# R Dakin
# 2024-03-21
# BIOL 5404

library(tidyverse)

#### Get the data ####

help(billboard)

# The billboard data comes with the tidyverse package
# It is in wide format initially, and gives billboard chart rankings for charting songs from the year 2000

head(billboard)

# Set up a tidy dataframe, one row per song-rank, and remove the NAs:
dat <- billboard %>% pivot_longer(cols = 4:79, names_to = 'week', values_to = 'rank') %>% 
  mutate(year_entered = year(date.entered)) %>% 
  mutate(week_num = as.numeric(substr(week, 3, nchar(week)))) %>% 
  mutate(date_rank = date.entered + (7 * (week_num - 1))) %>% 
  filter(!is.na(rank))

dat # 5307 song-ranks
length(unique(dat$track)) # 316 songs (assuming all uniquely named... let's check)
length(unique(dat$artist)) # by 228 artists

dat %>% 
  select(artist, track) %>% 
  unique() %>% 
  filter(duplicated(track))

dat %>% 
  select(artist, track) %>% 
  unique() %>% 
  filter(track == 'Where I Wanna Be') # somehow, we have two charting songs w/ same name 

# So, we have 317 songs by 228 artists

#### Checking the data (hygiene) ####

dat %>% select(artist, track, year_entered) %>% 
  unique() %>% 
  pull(year_entered) %>% table() # some of the songs entered the ranks in the year 1999

# Check the rank values to make sure they're as expected:
dat %>% ggplot() + geom_histogram(aes(x = rank))
dat %>% pull(rank) %>% range() # as expected, ranks range from 1-100
dat %>% mutate(check = rank %in% 1:100) %>% pull(check) %>% table() # they're all integer values, good.

# We calculated the date of each song-rank value based on the song's entry date, + a number of weeks
# Let's check that the specfic dates of each rank correspond to a set of matching weekly values, as expected (i.e., we should have a limited set of dates that correspond to a weekly release of the billboard list):
dat %>% ggplot() + geom_bar(aes(x = date_rank), width = 0.7) + labs(y = 'Count', x = 'Date')
# Confirmed, we have 97 unique (discrete) date-values
release_dates <- dat %>% 
  filter(!duplicated(date_rank)) %>% 
  select(date_rank) %>%
  arrange(date_rank) %>% 
  mutate(interval = date_rank - lag(date_rank))

release_dates %>% 
  pull(interval) %>% table() # and all of them are 7 days apart, as expected

# We can check what day of the week those charts were released, too!
release_dates %>% mutate(d_of_week = wday(date_rank, label = T)) %>% group_by(d_of_week) %>% tally()
# billboard charts in 2000 were always on a Saturday

# For every release date in 2000, do we have all of the songs? 
# We don't expect to have complete info for release dates in 1999 ofc
release_dates_2000 <- release_dates %>% filter(year(date_rank) == 2000) %>% pull(date_rank)
release_dates_2000

week_tally <- dat %>% 
  filter(date_rank %in% release_dates_2000) %>% 
  group_by(date_rank) %>% 
  summarize(n_hits = n(), check_no_dup_ranks = length(unique(rank)) == n(), n_ties = sum(duplicated(rank)))
week_tally
# We don't! Let's check whether there are duplicated ranks for a given date, too... note, duplicated ranks could be ties in billboard's methodology

dat %>% filter(date_rank == '2000-01-01') %>% 
  select(track, rank) %>% 
  arrange(rank) %>% 
  mutate(is_tie = duplicated(rank)) %>% 
  print(n = Inf) # an example of a week w/ duplicated ranks. I assume they're ties.

#### 1. Song-level dynamics ####

songs <- dat %>% 
  group_by(artist, track) %>% # note, there are 2 hits w/ same track name
  summarize(n_weeks = n(), peak_rank = min(rank), med_rank = median(rank)) %>% 
  arrange(desc(n_weeks)) # take a look at longest-running hits

songs

dat %>% group_by(artist, track) %>% 
  summarize(peak = min(rank), tenure = max(week_num)) %>% 
  ggplot() + theme_bw() +
  geom_point(aes(y = peak, x = tenure)) +
  scale_y_continuous(trans = 'reverse') + # put top-ranked songs at top of plot
  labs(y = 'Peak rank on Billboard chart', x = 'Tenure on Billboard chart (# weeks)')

# Here we see an important feature of the data: why do so many songs have a tenure of 20 weeks?
songs %>% ggplot() + 
  geom_histogram(aes(x = n_weeks)) + 
  theme_bw() + 
  labs(x = 'Tenure (number of weeks on the top 100 chart)', y = 'Count')

dat %>% group_by(artist, track) %>% 
  summarize(peak = min(rank), tenure = max(week_num)) %>% 
  filter(tenure == 20)
dat %>% filter(artist == 'Aaliyah' & track == "I Don't Wanna") %>% print(n = Inf)
dat %>% filter(artist == 'Avant' & track == "Separated") %>% print(n = Inf)

# Doing some investigations...
# https://waxy.org/2008/05/the_whitburn_project_onehit_wonders_and_pop_longevity/
# From that site: "See the heavy dropoff on the 20th week starting in 1991? In an attempt to increase diversity and promote newer artists and songs, Billboard changed their methodology, removing tracks that had been on the Hot 100 for twenty consecutive weeks and slipped below the 50th position. These songs, called “recurrents,” were then moved to their own chart in 1991, the Hot 100 Recurrent."

songs %>% ggplot() + theme_bw() +
  geom_point(aes(y = peak_rank, x = med_rank)) + 
  scale_y_continuous(trans = 'reverse') +
  scale_x_continuous(trans = 'reverse') +
  labs(x = 'Median rank achieved', y = 'Peak rank achieved') # as we'd expect
# We have some outliers that reached a relatively high peak rank yet have a lower-than expected median rank; perhaps these were 'flash-in-the-pan', worth checking out at later time.


# Let's visualize the life and death of some of the longest-running songs:
long_tenure <- songs[1:15,]

dat %>% filter(artist %in% long_tenure$artist & track %in% long_tenure$track) %>% 
  mutate(artist_track = paste(artist, track)) %>% 
  ggplot() + theme_bw() +
  geom_point(aes(x = date_rank, y = rank, group = artist_track, color = artist_track), show.legend = F) +
  geom_line(aes(x = date_rank, y = rank, group = artist_track, color = artist_track), show.legend = F) + 
  scale_y_continuous(breaks = c(1, 25, 50, 75, 100), trans = 'reverse')

# Nearly all of the long-running songs reach #1 on the charts
# We see a pretty consistent pattern of rise and fall for these long-running songs
# We can also see that there are a few hits that get a "second life" (they rise and plateau or fall, then rise dramatically again to #1). Why might that be?
# We also see the effects of the recurrents here, because we see how common it is for long-running songs to get killed off the main chart with a final rank of 50. We would not expect that to occur by chance: it's another sign of the recurrent rule.


#### 2. Multi-hit artists ####

multi_artists <- dat %>% 
  group_by(artist, track) %>% 
  summarize(n_weeks = n()) %>% 
  group_by(artist) %>% 
  summarize(n_songs = n()) %>% 
  arrange(desc(n_songs)) %>% 
  filter(n_songs >= 3)

dat %>% 
  filter(artist %in% multi_artists$artist) %>% 
  group_by(artist, track) %>% # by artist-song
  summarize(peak = min(rank), tenure = max(week_num), weeks_t10 = sum(rank <= 10)) %>% 
  summarize(p1 = min(peak), mid = median(peak), p2 = max(peak)) %>% # by artist
  ggplot(aes(reorder(artist, mid))) + theme_bw() +
  geom_segment(aes(xend = reorder(artist, mid), y = p1, yend = p2)) +
  geom_point(aes(x = reorder(artist, mid), y = p1), color = 'orange') +
  geom_point(aes(x = reorder(artist, mid), y = p2), color = 'lightblue') +
  labs(y = 'Peak Billboard rank', x = '') +
  scale_y_continuous(trans = 'reverse', breaks = c(1, 25, 50, 75, 100)) +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.3))




#### 3. Seasonal changes in chart behaviour ####

# What about the phenology of change... when in the year does the most turnover happen in top songs?
# When do songs generally enter and leave? 
# How can we quantify change? counts, sums. n enter. n leave. n enter + n leave BUT sampling phenology? 
# Maybe also the proportion shared from prior weeks to current. and mean of abs value of churn.

seasonal_data <- data.frame(week = release_dates_2000, n_songs_list = NA, n_fresh = NA, mag_rank_change = NA)
for(i in 2:nrow(seasonal_data)){
  tmp <- dat %>% 
    filter(date_rank == seasonal_data$week[i]) %>% 
    mutate(AT = paste(artist, track))
  tmp_prev <- dat %>% 
    filter(date_rank == seasonal_data$week[i - 1]) %>% 
    mutate(AT = paste(artist, track)) %>% 
    rename('rank_prev' = 'rank')
  tmp_all_prev <- dat %>% 
    filter(date_rank < seasonal_data$week[i]) %>% 
    mutate(AT = paste(artist, track))
  seasonal_data$n_songs_list[i] <- nrow(tmp)
  seasonal_data$n_fresh[i] <- sum(!(tmp$AT %in% tmp_all_prev$AT))
  
  tmp_change <- tmp[,c(5, 9)] %>% 
    left_join(tmp_prev[,c(5, 9)]) %>% 
    mutate(change = rank - rank_prev)
  seasonal_data$mag_rank_change[i] <- mean(abs(tmp_change$change), na.rm = T)
  print(i)
}
seasonal_data

seasonal_data %>% 
  mutate(p_fresh = n_fresh / n_songs_list) %>% 
  ggplot() + theme_bw() +
  geom_point(aes(x = week, y = p_fresh)) +
  geom_smooth(aes(x = week, y = p_fresh), se = F) +
  labs(x = 'Week', y = 'Proportion of songs that are new') # surprising and worth further investigation if we had data from multiple years to see if this is a repeated pattern, or a feature of sampling. the fact that it doesn't appear to be cyclical makes me suspect this is a sampling thing (given that we don't have info on all ranked songs for a given week)

seasonal_data %>% 
  mutate(p_fresh = n_fresh / n_songs_list) %>% 
  ggplot() + theme_bw() +
  geom_point(aes(x = week, y = mag_rank_change)) +
  geom_smooth(aes(x = week, y = mag_rank_change), se = F) +
  labs(x = 'Week', y = 'Average size of rank change (across ranked songs)')

seasonal_data %>% 
  mutate(p_fresh = n_fresh / n_songs_list) %>% 
  ggplot() + theme_bw() +
  geom_point(aes(x = p_fresh, y = mag_rank_change)) +
  labs(y = 'Average size of rank change (across ranked songs)', x = 'Proportion of songs that are new')


# Next steps: what kind of data would we seek next to probe this more?




