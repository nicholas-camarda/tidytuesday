library(modelStudio)
library(tidyverse)
library(tidymodels)
library(rtweet)
library(ggpubr)
library(ggprism)
library(paletteer)
library(patchwork)
library(rstatix)


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
  filter(year >= 1990 & year < 2020,
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
  ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~")), 
                   color = "red", geom = "label") +
  # facet_wrap(vars(continent),drop = TRUE) + 
  my_theme +
  ggtitle("Global deaths decrease with increased access\nto clean fuels and technology") +
  labs(x = "Average % access to clean fuels/tech",
       y = "% deaths associated with\nlack of clean fuel/tech",
       size = "Mean GDP per capita",
       caption = "Data source: OurWorldInData.org"); p1

p1_1 <- ggplot(plot_df, aes(x = log10(mean_gdp_per_capita), y = mean_access_perc)) + 
  # coord_trans("log10") +
  geom_point(data = plot_df, aes(size = mean_death_perc, fill = continent), pch = 21) +
  geom_smooth(method = "loess") +
  scale_fill_paletteer_d("colorblindr::OkabeIto") +
  ggpubr::stat_cor(method = "spearman", 
                   aes(label = paste(..rr.label.., ..p.label.., sep = "~")), 
                   color = "red", geom = "label", label.x = 4.1, label.y = 5) +
  # facet_wrap(vars(continent),drop = TRUE) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) + 
  my_theme +
  ggtitle("Access to clean energy is associated with GDP") +
  labs(x = "Log10 of Average GDP per capita",
       y = "Average % access to clean fuels/tech",
       fill = "Continent", 
       size = "Average % Death",
       caption = "Spearman correlation\nAveraged values over years 1990-2019\nData source: OurWorldInData.org"); p1_1

fn <- "~/Downloads/pollution.png"
ggsave(plot = p1, filename = fn)
fn_1 <- "~/Downloads/pollution2.png"
ggsave(plot = p1_1, filename = fn_1)

# use patchwork to easily display both these plots side by side
p1 + p1_1


# can we predict death risk?
# cool tutorial: https://www.business-science.io/r/2022/02/22/my-4-most-important-explainable-ai-visualizations-modelstudio.html?utm_content=buffer51d7b&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer

plot_df2_temp <- countries_only %>% 
  convert_as_factor(year, entity, continent)

x_ <- plot_df2_temp$gdp_per_capita
qnt <- quantile(x_, seq(0, 1, 0.333), na.rm= TRUE)
new_qnt <- qnt;
names(new_qnt) <- c("Low", "Medium", "High", "Very High")
res <- cut(x_, unique(new_qnt), include.lowest=TRUE)
new_res <- tibble(gdp_fct = levels(res),
                  names_ = c("Low", "Medium", "High")) %>%
  convert_as_factor(gdp_fct, names_)

plot_df2 <- plot_df2_temp %>%
  mutate(gdp_fct = res) %>%
  filter(continent != "Antarctica") %>%
  left_join(new_res) %>%
  dplyr::select(-gdp_fct) %>%
  mutate(gdp_fct_lvls = factor(names_, levels = c("Low", "Medium", "High"))) %>%
  dplyr::select(-country_code, -names_, -gdp_per_capita)


# fit xgboost
fit_xgboost <- boost_tree(learn_rate = 0.3) %>%
  set_mode("regression") %>% # since numeric target
  set_engine("xgboost") %>%
  fit(access_to_clean_fuel_perc ~ . , data = plot_df2)

explainer <- DALEX::explain(
  model = fit_xgboost,
  data = plot_df2,
  y = plot_df2$access_to_clean_fuel_perc,
  label = "XGBoost"
)

modelStudio::modelStudio(explainer)

################ EXTRA #############


# debug_contr_error(dat = plot_df2)


anova_res <- rstatix::anova_test(data = plot_df2, 
                                 within = year,
                                 formula = deaths_percent ~ entity + gdp_fct_lvls*access_to_clean_fuel_perc)
anova_tbl <- get_anova_table(anova_res); anova_tbl

# ggbarplot(plot_df2, x = "year", y = "access_to_clean_fuel_perc", 
#           facet.by = "entity")

plot_df2 %>% 
  group_by(year) %>%
  pairwise_t_test(formula = deaths_percent ~ gdp_fct_lvls)


p2 <- ggplot(plot_df2,
             aes(x = year, y = deaths_percent)) +
  geom_point(aes(color = gdp_fct_lvls), show.legend = T) +
  geom_line(aes(color = gdp_fct_lvls, group = entity)) +
  facet_wrap(vars(continent)) +
  geom_smooth(method = "lm", color = "red") +
  ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~")), 
                   color = "red", geom = "label") +
  # scale_color_paletteer_c(palette = "pals::parula") +
  scale_color_paletteer_d(palette = "colorblindr::OkabeIto", 
                          na.translate = F) + # prevents NAs from showing up in Legend
  my_theme +
  labs(x = "Year",
       y = "% deaths associated with\nlack of access to clean fuel/tech",
       color = "GDP per capita",
       title = "Continental % Deaths decreases with Increased GDP",
       caption = "Data source: OurWorldInData.org",
       subtitle = get_test_label(anova_res, detailed = TRUE)); p2




# csv_fuel_gdp <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/fuel_gdp.csv')
# csv_fuel_access <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/fuel_access.csv')
# csv_indoor_pollution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/indoor_pollution.csv')
# csv_death_source <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_source.csv')
# csv_death_full <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_full.csv')
# csv_death_timeseries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_timeseries.csv')


