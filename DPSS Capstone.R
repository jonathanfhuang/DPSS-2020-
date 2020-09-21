library(tidyverse)
library(devtools)
library(urbnmapr) #devtools::install_github("UrbanInstitute/urbnmapr")
library(gganimate)
library(RColorBrewer)
library(gifski)
library(plotly)
library(crosstalk)
library(extrafont)
font_import()

####Tidy Data for Mask Mandate Map####
DPSS_Mask_Mandate <- read_csv("DPSS - Mask Mandate Data.csv")

DPSS_Mask_Mandate <- DPSS_Mask_Mandate %>%
  mutate(date_mask_policy_start = as.Date(mask_policy_start, 
                                          format = "%m/%d/%y")) %>%
  mutate(date_diff = Sys.Date() - date_mask_policy_start)

DPSS_Mask_Mandate <- DPSS_Mask_Mandate %>%
  mutate(group = case_when(
    between(date_diff, 0, 49) ~ "0-49 Days",
    between(date_diff, 50, 99) ~ "50-99 Days",
    between(date_diff, 100, 150) ~ "100-150 Days",
    TRUE ~ "No Mask Mandate Implemented"
  ))

DPSS_Mask_Mandate$state_fips[28] <- "31"

DPSS_Mask_Mandate$group <- factor(DPSS_Mask_Mandate$group, 
                                  levels = c("0-49 Days", 
                                             "50-99 Days", 
                                             "100-150 Days", 
                                             "No Mask Mandate Implemented"))

####Mask Mandate Map####
spatial_data <- left_join(get_urbn_map(map = "states", sf = TRUE),
                          DPSS_Mask_Mandate,
                          by = "state_fips")

spatial_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = group),
          color = "#ffffff", size = 0.5) +
  labs(fill = "Categorical variable") +
  geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE), 
               aes(label = state_abbv), 
               size = 3) +
  labs(fill = "Number of Days Your State \nHas Had a Mask Mandate",
       title = "Does Your State Have a Mask Mandate?") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5),
        text=element_text(family="Georgia", size=20),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 8)) +
  scale_fill_brewer(palette = "YlOrRd")

####COVID-19 Cases by State Line Graph####
NYT_COVID_us_counties <- read_csv("NYT-COVID-us-counties.csv")

tidy_data <- NYT_COVID_us_counties %>%
  drop_na() %>%
  group_by(date, state) %>%
  summarise(State_Cases = sum(cases)) %>%
  mutate(Date = format(date, "%B %d, %Y")) %>%
  mutate(State = state)

st.reg <- data.frame(state = state.name, Region = state.region)

new_tidy_data <- left_join(tidy_data, st.reg , by="state") %>%
  drop_na() %>%
  mutate('Number of Cases' = State_Cases)

options(scipen=999)

d <- SharedData$new(new_tidy_data, ~State)

line_graph <- ggplot(d, aes(x=date, y = State_Cases)) + 
  geom_line(aes(color = State)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text=element_text(family="Georgia", size=14)) +
  labs(title = "Number of Total COVID-19 Cases by State Over Time",
       x = "Date",
       y = "Number of Cases") + 
  scale_y_log10()+
  geom_hline(yintercept=1000,
             linetype="dashed", 
             size=.5, 
             alpha = 0.5) + 
  geom_hline(yintercept=100000, 
             linetype="dashed", 
             size= .5, 
             alpha = 0.5)

ggplotly(line_graph, tooltip = c("State", "State_Cases", "date"))%>%
  highlight(on = "plotly_hover")

####Animated Bar Graph of COVID-19 Cases by State####
ranked_data <- tidy_data %>%
  group_by(date) %>%
  filter(date > "2020-03-09") %>%
  mutate(rank = rank(-State_Cases) * 1,
         Value_lbl = paste0(" ",State_Cases)) %>%
  group_by(state) %>%
  filter(rank <= 10)

anim <- ggplot(ranked_data, aes(rank, 
                                group = state, 
                                fill = as.factor(state), 
                                color = as.factor(state))) +
  geom_tile(aes(y = State_Cases/2,
                height = State_Cases,
                width = 0.9),
            alpha = 0.8,
            color = NA) +
  geom_text(aes(y = 0, label = paste(state, " ")), 
            vjust = 0.2, 
            hjust = 1,
            size = 7) +
  geom_text(aes(y = State_Cases,
                label = Value_lbl,
                hjust = 0),
            size = 8) +
  coord_flip(clip = "off",
             expand = FALSE) +
  scale_x_reverse() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2, 6, 2, 6, "cm")) +
  transition_states(date, transition_length = 4,
                    state_length = 1) +
  ease_aes('linear') +
  view_follow(fixed_x = TRUE) +
  enter_grow() + 
  exit_shrink() +
  labs(title = 'Number of Total Cases: {closest_state}',
       subtitle = "Ranked by Top 10 States",
       caption = "Data Source: NY Times")


animate(anim, 100, fps = 25,  width = 800, 
        height = 900, duration = 100)

anim_save("Animated Barplot Number of Cases by State.gif")

