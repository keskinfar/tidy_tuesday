# Tidy-tuesday 14.07.2020
# Faruk Keskin
# twitter @keskinfar

library(tidyverse)
library(ggdark)
library(tidytuesdayR)

# Get data
tt_data <- tt_load(2020, week = 29)

# Prepare data
df_ast <- tt_data$astronauts %>% mutate(days_mission = hours_mission/24) %>% 
                         mutate(era = cut(year_of_mission, 
                                breaks=c(1960,1969,1979,1989,1999,2009,2019), 
                                labels=c("1960s","1970s","1980s","1990s","2000s","2010s")))
#Prepare theme
theme_ast <- function() {
  dark_theme_gray() %+replace%
    theme(
      title = element_text(color ="grey90"),
      plot.title = element_text(family = "sans", size = 40, face="bold", hjust = 0, vjust = 1),
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
}

#Plot  ####
df_ast %>% 
  ggplot()+
  geom_point(aes(x = days_mission, y = eva_hrs_mission,
                 size = mission_number, color = sex),
             alpha = 1/3, shape=19) +
  # 6 eras
  facet_wrap(~era, ncol=6) +
  # Bigger scale for better view
  scale_size(breaks = c(1:7), range = c(1,10))+
  scale_color_manual(breaks = c("female", "male"),
                     values = c("khaki","steelblue1"),
                     labels = c("Female", "Male")) +
  scale_x_continuous(breaks=seq(0,400,100), labels = c("","","200","","400")) +
  # Titles  
  labs(title = "Time spent in space",
       x = "Days in space",
       y = "EVA Hours",
       color = "",
       size = "Mission number",
       caption = "Data: Mariya Stavnichuk and Tatsuya Corlett,  Visual: Faruk Keskin") +
  # Legend tweaks
  guides(color = guide_legend(override.aes = list(size = 5)),
         size = guide_legend(label.position = "bottom",
                             direction = "horizontal",
                             title.position = "top",
                             nrow = 1))+
  theme_ast()

# Save plot  ####
ggsave(file.path("astronauts", paste0("astronauts-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       dpi = 400, width = 35, height = 25, units ="cm")
