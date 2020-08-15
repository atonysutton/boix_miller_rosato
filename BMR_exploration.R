###########
#
# Explore and Chart Data About Democracies
#
# Code by Tony Sutton
#  July 2020
#
###########

# Load packages
library(tidyverse)
library(gganimate)

# Load data
# Data credited to Carles Boix, Michael Miller, Sebastian Rosato, 2018
#  "Boix-Miller-Rosato Dichotomous Coding of Democracy, 1800-2015"
#  Harvard Dataverse V3, https://doi.org/10.7910/DVN/FJLMKT

bmr <- read_csv("BMR_democracy.csv")

# Shape data ----
## manually add 11 years to all duraitons for US, adjusting for 1789 democratization in a dataset starting at 1800
bmr %>% filter(country !='UNITED STATES OF AMERICA') %>% 
  summarize(safety_check = sum(democracy_duration, na.rm = TRUE))

bmr <- bmr %>% mutate(democracy_duration = if_else(country == 'UNITED STATES OF AMERICA',
                                                   as.integer(democracy_duration + 11),
                                                   as.integer(democracy_duration)))

bmr %>% filter(country !='UNITED STATES OF AMERICA') %>% 
  summarize(safety_check = sum(democracy_duration, na.rm = TRUE))
bmr %>% filter(country !='UNITED STATES OF AMERICA') %>% 
  summarize(lowest_duration = min(democracy_duration, na.rm = TRUE))

## check basic contours
count(bmr, democracy_trans)

max_year <- max(bmr$year)
bmr %>% filter(year == max_year) %>% count(democracy)
      
##add a marker for the last year of a democratic spell
reversals <- bmr %>% 
  filter(democracy_trans == -1) %>% 
  select(country, year) %>% 
  mutate(country_year = paste(country, year, sep="_"))

bmr <- bmr %>% mutate(next_year = year + 1, next_country_year = paste(country, next_year, sep = "_"))

bmr <- bmr %>% mutate(fin_de_siecle = if_else(next_country_year %in% reversals$country_year, 1, 0))
sum(bmr$fin_de_siecle)
sum(bmr$fin_de_siecle) == sum(bmr$democracy_trans == -1, na.rm=TRUE)
      
## create dataframe with one row per domestic spell ----
failed_democracies <- bmr %>% filter(fin_de_siecle == 1) %>% select(country, year, democracy_duration) %>% mutate(outcome = 'Eventually Failed Democracies')
current_democracies <- bmr %>% filter(year == max_year, democracy == 1) %>% select(country, year, democracy_duration) %>% mutate(outcome = ' Still Existing Democracies')
      
spells <- rbind(failed_democracies, current_democracies)
      
# Visualize ----
##chart durations as histograms, split by outcomes
ggplot(data = spells, aes(x = democracy_duration))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~outcome, nrow=2)+
  theme_minimal()+
  labs(title = 'Years Survived by Each Democracy',
       subtitle = '  for every democratic regime since 1789',
       x = 'Years',
       y = 'Number of Democracies')+
  theme(plot.title = element_text(size = 19, face = 'bold'),
        plot.subtitle = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing = unit(2, 'lines'))

ggsave(filename = "democracy_duration_histogram.jpg",
       width = 10,
       height = 6,
       units = 'in')
      
##animate chart of number and percentage of democracies each year
dem_color = 'cornflowerblue'
  
p <- ggplot((data = bmr %>%
               filter(!is.na(democracy) & year %% 5 == 0) %>%
               group_by(year) %>%
               mutate(pct_democratic = 100 * (sum(democracy) / n())) %>% ungroup),
            aes(x = democracy))+
  geom_bar()+
  scale_x_continuous(breaks = 0:1, labels = c('Autocracies', 'Democracies'))+
  transition_states(year)+
  ease_aes('cubic-in-out')+
  labs(title = 'The Spread of Democracy',
       subtitle = "  Year: {closest_state}",
       x = '',
       y = 'Number of Countries')+
  theme_minimal()+
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.text.y.right = element_text(color = dem_color),
        axis.title.y.right = element_text(color = dem_color),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

p2 <- p +
  geom_point(aes(x = 1.55, y = pct_democratic), shape = 23, size = 7, stroke = 2, fill = dem_color)+
  geom_vline(xintercept = 1.55, linetype = 'dashed', color = dem_color)+
  scale_y_continuous(sec.axis = sec_axis(~., name = 'Percent Democratic'))+
  annotate("text", x = 1, y = 110, color = 'gray60', fontface = 'italic', size = 4, label = 'Data from Boix,    \n Miller, and Rosato')

animate(p2, fps = 3, end_pause = 9)

anim_save('democracy_spread.gif')

##compare onset of male- and full-suffrage democracy
ggplot(data = bmr %>% group_by(year) %>%
         summarize(male_suffrage = sum(democracy, na.rm = TRUE),
                   full_suffrage = sum(democracy_femalesuffrage, na.rm = TRUE)),
       aes(x = year))+
  geom_line(aes(y = male_suffrage), size = 1.3, alpha = 0.5)+
  geom_line(aes(y = full_suffrage), size = 1.3)+
  theme_minimal()


