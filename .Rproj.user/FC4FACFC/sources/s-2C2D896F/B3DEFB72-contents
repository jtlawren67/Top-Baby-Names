library(tidyverse)
library(gganimate)
library(scales)

# Data Comes from https://www.ssa.gov/oact/babynames/names.zip

##Read In Data Files
all_data <- map_dfr(dir('.data'), function(x){
  read_csv(paste0('.data/', x), 
           col_names = c('name', 'gender', 'count'),
           col_types = list('c', 'c', 'i')) %>%
    mutate(year = x %>% str_extract('\\d+') %>% as.integer)
})

##Create Column For Cumulative Counts
all_data_cumulative <- all_data %>%
  group_by(gender, name) %>%
  arrange(year) %>% 
  mutate(cuml_count = cumsum(count)) %>%

#Get Top 10 Per Year
top_10_names <- all_data_cumulative %>% 
  group_by(gender, year) %>%
  top_n(10, wt = count) %>% 
  mutate(rank = rank(count))


##Create Animated Racing Histogram Plot
animated <- top_10_names %>% 
  filter(gender == "F") %>%
  ggplot(aes(x = rank, y = count/2, group = name)) + 
    geom_col(fill = "#FFC0CB") + 
    geom_text(aes(label = count %>% comma(accuracy = 1), hjust = 0)) + 
    geom_text(aes(label = name), y = 0, vjust = .2, hjust = 1) +
    labs(x = "Girl's Name", y = "# of Babies",
         title = "Top 10 Girls Baby Names (1880-2018)",
         subtitle = '{round(frame_time,0)}',
         caption = 'Source: Social Security Administration') + 
    coord_flip(clip = 'off') + 
    cowplot::theme_cowplot() + 
    theme(
      axis.title = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks = element_blank(),
      axis.text.y = element_blank() ,
      axis.line=element_blank(),
      plot.title.position = "plot"
    ) + 
    transition_time(year) + 
    ease_aes('cubic-in-out') 

animate(animated, fps = 10, duration = 50, width = 1000, height = 600,
        renderer=gifski_renderer("baby_girls.gif"))

