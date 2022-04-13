library(tidyverse)
library(data.table)
library(rtweet)
library(ggpubr)
library(ggprism)

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

  # mutate(continent = ifelse(is.na(continent), entity, continent))

# Not worth sifting through the bad codes
# combined_data_df %>% 
#   distinct(entity, continent) %>%
#   mutate(corrected_continent = map2_chr(.x = entity, .y = continent, .f = function(ent, cont){
#     
#     if (is.na(cont)) {
#       res <- str_extract(string = ent, pattern = complex_pattern)
#     }
#   }))
# combined_data_df[complete.cases(combined_data_df),] %>% View()

countries_only <- combined_data_df %>%
  filter(!is.na(country_code))

test_names <- countries_only %>% 
  distinct(entity) %>%
  # slice(1:100) %>%
  .$entity


rel_size <- 1
my_theme <- theme_prism(border = TRUE,
                        base_size = 5) +
  theme(strip.text.x = element_text(size = rel(rel_size*3)),
        title = element_text(size = rel(rel_size*4)),
        legend.box.spacing = unit(1, "cm"),
        legend.text = element_text(size = rel(rel_size*1.5)),
        axis.text.y = element_text(size = rel(rel_size*2), angle = 0, vjust = 0.2),
        axis.text.x = element_text(size = rel(rel_size*1.6), angle = 45),
        panel.grid = element_line(color = "gray",
                                  size = 0.15,
                                  linetype = 2),
        panel.spacing = unit(1, "lines"),
        plot.caption = element_text(size = rel_size*8)) 

colnames(combined_data_df_temp)
test_df <- countries_only %>% 
  filter(entity %in% test_names) %>%
  group_by(continent, entity) %>%
  summarize(mean_death_perc = mean(deaths_percent, na.rm = TRUE),
            mean_access_perc = mean(access_to_clean_fuel_perc, na.rm = TRUE),
            mean_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE), .groups = "keep")

ggscatter(test_df, 
          x = "mean_access_perc",
          y = "mean_death_perc",
          # x = "access_to_clean_fuel_perc", 
          # y = "deaths_percent", 
          # color = "mean_gdp_per_capita",
          facet.by = "continent",
          add = "loess", conf.int = TRUE,
          ylim = c(0, max(test_df$mean_death_perc)),
          palette = "RdYlBlu") + 
  my_theme
          

ggscatter(test_df, 
          x = "mean_access_perc",
          y = "mean_gdp_per_capita",
          # x = "access_to_clean_fuel_perc", 
          # y = "deaths_percent", 
          size = "mean_death_perc",
          facet.by = "continent",
          add = "loess", conf.int = TRUE,
          ylim = c(0, max(test_df$mean_gdp_per_capita))) +
  my_theme +
  ggtitle("Access to clean fuel and percent deaths attributable to pollution\nas a function of GDP") +
  labs(x = "Average % access to clean fuels/technologies",
       y = "Average GDP per capita, since 1990")
          
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

# rtweet::post_tweet()
