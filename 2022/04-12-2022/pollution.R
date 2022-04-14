library(tidyverse)
library(rtweet)
library(ggpubr)
library(ggprism)
library(paletteer)
library(patchwork)

datasets <- tidytuesdayR::tt_load('2022-04-12')


indoor_pollution <- datasets$indoor_pollution 
fuel_gdp <- datasets$fuel_gdp
fuel_access <- datasets$fuel_access

# death fuel and death time series are the same
death_timeseries <- datasets$death_timeseries
death_fuel <- datasets$death_fuel
all.equal(death_fuel, death_timeseries)

death_source <- datasets$death_source


# multijoin, no timeseries
dim(indoor_pollution)
dim(death_source)
# equal lengths so we can do left join or right
deaths_static_df <- purrr::reduce(list(indoor_pollution, death_source),
                                  left_join, by = c("Entity", "Code", "Year"))

# extra space in this column name...
fuel_access2 <- fuel_access %>% 
  rename(`Access to clean fuels and technologies for cooking (% of population)` = `Access to clean fuels and technologies for cooking  (% of population)`)

dim(fuel_access2)
dim(fuel_gdp)
fuel_access_df <- purrr::reduce(list(fuel_gdp, fuel_access2),
                                left_join, by = c("Entity", "Code", "Year", "Access to clean fuels and technologies for cooking (% of population)"))

# here's some continent info, remove NA's until we know which are actually unknown, which are mistakes, and which are actually continents
continent_info <- fuel_access_df %>%
  distinct(Entity, Continent) %>%
  na.omit()

# full join and recode the column names
combined_data_df_temp <- full_join(deaths_static_df, fuel_access_df, 
                                   by = c("Entity", "Code", "Year")) 
combined_data_df <- combined_data_df_temp %>%
  setNames(c("entity", "country_code", "year", "deaths_percent", 
             "deaths_rate", "access_to_clean_fuel_perc", 
             "gdp_per_capita", "country_population", "continent")) %>%
  dplyr::select(-continent) %>%
  left_join(continent_info, by = c("entity" = "Entity")) %>%
  rename(continent = Continent) %>%
  filter(year >= 1990, 
         !is.na(continent))

rel_size <- 1
my_theme <- theme_prism(border = TRUE, 
                        base_size = 5) +
  theme(strip.text.x = element_text(size = rel(rel_size*3)),
        title = element_text(size = rel(rel_size*3)),
        legend.box.spacing = unit(1, "cm"),
        legend.text = element_text(size = rel(rel_size*1.5)),
        legend.title = element_text(size = rel(rel_size*0.5)),
        axis.text.y = element_text(size = rel(rel_size*2), angle = 0, vjust = 0.2),
        axis.text.x = element_text(size = rel(rel_size*1.6), angle = 45),
        panel.grid = element_line(color = "gray",
                                  size = 0.15,
                                  linetype = 2),
        panel.spacing = unit(1, "lines"),
        plot.caption = element_text(size = rel_size*8)) 

# colnames(combined_data_df_temp)


countries_only <- combined_data_df %>%
  filter(!is.na(country_code))

# adjust this to test code
test_names <- countries_only %>% 
  distinct(entity) %>%
  # slice(1:100) %>%
  .$entity

plot_df <- countries_only %>% 
  filter(entity %in% test_names,
         continent != "Antarctica") %>%
  group_by(continent, entity) %>%
  summarize(mean_death_perc = mean(deaths_percent, na.rm = TRUE),
            mean_access_perc = mean(access_to_clean_fuel_perc, na.rm = TRUE),
            mean_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE), .groups = "keep")


paletteer::palettes_d_names %>%
  arrange(desc(length)) %>%
  filter(type == "divergent") %>%
  print(n = 50) 

p1 <- ggplot(plot_df, aes(x = mean_access_perc, y = mean_death_perc )) + 
  geom_point(data = plot_df, aes(size = mean_gdp_per_capita, fill = continent), pch = 21) + 
  geom_smooth(method = "loess") +
  scale_fill_paletteer_d("colorblindr::OkabeIto") +
  # facet_wrap(vars(continent),drop = TRUE) + 
  my_theme +
  ggtitle("Global deaths decrease with increased access\nto clean fuels and technology") +
  labs(x = "Average % access to clean fuels/tech",
       y = "% deaths associated with\nlack of clean fuel/tech",
       size = "Mean GDP per capita",
       caption = "Data source: OurWorldInData.org"); p1


paletteer::palettes_c_names %>%
  # arrange(desc(length)) %>%
  # filter(type == "divergent") %>%
  print(n = 50) 

plot_df2 <- countries_only %>% 
  filter(continent %in% c("Africa", "North America", "Europe"))

rstatix::anova_test(data = plot_df2, dv = Year, )
p2 <- ggplot(plot_df2,
       aes(x = year, y = deaths_percent)) +
  geom_point(aes(color = gdp_per_capita), show.legend = T) +
  # geom_line() +
  facet_wrap(vars(continent)) +
  geom_smooth(method = "lm", color = "red") +
  ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~")), 
                   color = "red", geom = "label") +
  scale_color_paletteer_c(palette = "pals::parula") +
  my_theme +
  labs(x = "Year",
       y = "% deaths associated with\nlack of access to clean fuel/tech",
       color = "GDP per capita",
       title = "Continental % Deaths decreases with Increased GDP",
       caption = "Data source: OurWorldInData.org",
       subtitle = "% death reduction probably more associated with GDP than with time\n"); p2

rtweet::post_tweet()

#      x = "access_to_clean_fuel_perc", y = "deaths_rate", 
#      group = "continent", color = "continent",
#      add = c("jitter", "mean_se"), palette = "jco")  +
# my_theme


# csv_fuel_gdp <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/fuel_gdp.csv')
# csv_fuel_access <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/fuel_access.csv')
# csv_indoor_pollution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/indoor_pollution.csv')
# csv_death_source <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_source.csv')
# csv_death_full <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_full.csv')
# csv_death_timeseries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_timeseries.csv')


