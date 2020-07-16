# Tidy-tuesday 14.07.2020
# Faruk Keskin
# twitter @keskinfar


# Get data
library(tidytuesdayR)
tidytuesdayR::tt_load("2019", week = 3)
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

library(tidyverse)
library(ggdark)

# Prepare data
df_ast <- astronauts %>% mutate(days_mission = hours_mission/24) %>% 
                         mutate(era = cut(astronauts$year_of_mission, 
                                breaks=c(1960,1969,1979,1989,1999,2009,2019), 
                                labels=c("1960s","1970s","1980s","1990s","2000s","2010s")))

df_ast %>% 
  ggplot()+
  geom_point(aes(x = days_mission , 
                 y = eva_hrs_mission,
# Bigger scale for better view                
                 size=(df_ast$mission_number), 
             color=sex),
             alpha = 1/3, 
             shape=19) +
# Bigger scale for better view
            scale_size(breaks = c(1:7), 
                       range = c(1,10)) +
# 6 eras
            facet_wrap(~ era, ncol=6, ) +
# titles  
            labs(title = "Time spent in space ",
                 x = "Days in space",
                 y = "EVA Hours",
                 color = "",
                 size = "Mission number",
                 caption = "@keskinfar") +
# Legend tweaks
            guides(
                  color = guide_legend(override.aes = list(size = 5)
                                       ),
                  size = guide_legend(label.position = "bottom",
                                      direction = "horizontal",
                                      title.position = "top",
                                      nrow = 1
                                      )
                   ) +
            dark_theme_gray() + 
# Minor aesthetic tweaks
            scale_color_manual(breaks = c("female", "male"),
                               values = c("khaki","steelblue1"),
                               labels = c("Female", "Male")) +
            scale_x_continuous(breaks=seq(0,400,100), labels = c("","","200","","400"))+
            theme(
                  title = element_text(color ="grey90"),
                  plot.title = element_text(family = "sans", size = 40, face="bold"),
                  plot.background = element_rect(fill = "grey10"),
                  strip.text = element_text(size=25),
                  strip.background = element_blank(),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(color = "grey30", size = 0.2),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.minor = element_line(color = "grey10", size = 0.2),
                  axis.title = element_text(size = 30),
                  axis.text = element_text(size = 16, color ="grey90"),
                  axis.ticks = element_blank(),
                  legend.background = element_blank(),
                  legend.text = element_text(size = 24),
                  legend.title = element_text(size = 30),
                  legend.key = element_blank(),
                  legend.box = "vertical",
                  legend.position = "right"
                  )
ggsave(ggplot2::last_plot(), filename = "tt_astronauts.png",
       width = 35, height = 25, units = "cm")
