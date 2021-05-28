rm(list = ls())
library(data.table)
library(tidyverse)
csv.files <- list.files("inst/extdata/", "weather_data_Iida", full.names = T)

# var.names <- read.csv(csv.files[1], header = F, as.is = T, nrows = 1, skip = 3, fileEncoding = "cp932")

var.names <- c("date", "mean_temp", "max_temp", "min_temp", "rain_total_mm", "sun_dur", "total_snow_cm", "mean_wind", "mean_air_hPa", "humidity", "weather")

df.weather <- data.frame()
for(ii in 1:length(csv.files)) {
  d <- readr::read_csv(csv.files[ii], locale = locale(date_names = "ja", encoding = "cp932"), col_names = F, skip = 6,
                       na = "///")
  colnames(d) <- var.names
  
  df.weather <- rbind.data.frame(
    df.weather,
    d
  )
}
rm(d, ii, var.names, csv.files)
attr(df.weather,"spec") <- NULL
df.weather <- df.weather %>% 
  mutate_at(.vars = 2:ncol(.), .funs = function(x){ as.numeric(ifelse(x == "--", NA, x))}) %>% 
  as_tibble()

rainy_period <- openxlsx::read.xlsx("inst/extdata/rainy_period_data.xlsx") %>% 
  mutate(year = as.numeric(gsub("年", "", year)),
         start = gsub("ごろ", "", start),
         end = gsub("ごろ", "", end)) %>% 
  filter(year >= 2002) %>% 
  separate(start, sep = "月", into = c("st_month", "st_date")) %>% 
  separate(end, sep = "月", into = c("ed_month", "ed_date")) %>% 
  mutate(st_date = as.numeric(gsub("日", "", st_date)),
         ed_date = as.numeric(gsub("日", "", ed_date))) %>% 
  mutate(st_month_week = sprintf("%02s_week%d", st_month, case_when(
    st_date >= 1 & st_date <= 7 ~ 1,
    st_date >= 8 & st_date <= 14 ~ 2,
    st_date >= 15 & st_date <= 21 ~ 3,
    st_date >= 22 & st_date <= 28 ~ 4,
    TRUE ~ 5
  ))) %>% 
  mutate(ed_month_week = sprintf("%02s_week%d", ed_month, case_when(
    ed_date >= 1 & ed_date <= 7 ~ 1,
    ed_date >= 8 & ed_date <= 14 ~ 2,
    ed_date >= 15 & ed_date <= 21 ~ 3,
    ed_date >= 22 & ed_date <= 28 ~ 4,
    TRUE ~ 5
  ))) 

camping.period <- data.frame(
  period = c(1, 2),
  st_month_week = c("05_week4", "06_week4"),
  ed_month_week = c("06_week3", "07_week3")
)



# Visualization ####
NeURon::plot_theme_set(13)
naniar::vis_miss(df.weather)

# Temperature
year.display <- 2020
df.weather %>% 
  filter(str_detect(date, sprintf("%d年", year.display))) %>% 
  ggplot(aes(x = date, y = mean_temp, ymin = min_temp, ymax = max_temp)) +
  geom_point(color = "red") + 
  geom_errorbar() +
  labs(x = NULL, y = "Average temperature", title = sprintf("Year %d", year.display))

# Rainfalls
nrow(df.weather) - floor(nrow(df.weather)/7) * 7


df.weather %>% 
  # mutate(rain_total_mm = as.numeric(ifelse(rain_total_mm == "--", NA, rain_total_mm))) %>% 
  # mutate(id = 1:nrow(.)) %>% 
  mutate(week.id = c(rep(1:floor(nrow(df.weather)/7), each = 7), rep(floor(nrow(df.weather)/7)+1,3))) %>% 
  separate(date, sep = "年", into = c("year", "month_date"), remove = F) %>% 
  separate(month_date, sep = "月", into = c("month", "date")) %>% 
  mutate(date = as.numeric(gsub("日", "", date))) %>% 
  mutate(month_week = sprintf("%s_week%d", month, case_when(
    date >= 1 & date <= 7 ~ 1,
    date >= 8 & date <= 14 ~ 2,
    date >= 15 & date <= 21 ~ 3,
    date >= 22 & date <= 28 ~ 4,
    TRUE ~ 5
  ))) %>% 
  mutate(month = as.numeric(month)) %>% 
  mutate(season = case_when(
    month >= 4 & month <= 6 ~ "spring",
    month >= 7 & month <= 9 ~ "summer",
    month >= 10 & month <= 12 ~ "autumn",
    month >= 1 & month <= 3 ~ "winter"
  )) %>% 
  mutate(season = factor(season, levels = c("spring", "summer", "autumn", "winter"))) %>% 
  mutate(season2 = ifelse(month >= 5 & month <= 8, "rainy", "non-rainy")) %>% 
  filter(season2 == "rainy") %>% 
  # mutate(year = str_split(date, "年")[[1]][1], .before = date) %>% 
  # filter(is.na(year))
  group_by(year, season, month_week) %>% 
  summarise_all(.funs = mean, na.rm = T) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(x = month_week, y = rain_total_mm, color = season, group = 1)) +
  geom_point(aes(x = month_week, y = rain_total_mm, color = season)) +
  # geom_errorbar() +
  geom_rect(data = rainy_period, aes(xmin = st_month_week, xmax = ed_month_week, ymin = -Inf, ymax = Inf), alpha = .6,
            fill = "blue") +
  labs(x = NULL, y = "Total amount of rain (mm)"
       # title = sprintf("Year %d", year.display)
       ) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("#00FF7F", "#FF3030", "#FF7F24", "#87CEFA")) +
  facet_wrap(~ as.factor(year), scales = "free_x") 


df.weather %>% 
  separate(date, sep = "年", into = c("year", "month_date"), remove = F) %>% 
  separate(month_date, sep = "月", into = c("month", "date")) %>% 
  mutate(date = as.numeric(gsub("日", "", date))) %>% 
  mutate(month_week = sprintf("%02d_week%d", as.numeric(month), case_when(
    date >= 1 & date <= 7 ~ 1,
    date >= 8 & date <= 14 ~ 2,
    date >= 15 & date <= 21 ~ 3,
    date >= 22 & date <= 28 ~ 4,
    TRUE ~ 5
  ))) %>% 
  mutate(month = as.numeric(month)) %>% 
  mutate(season = case_when(
    month >= 4 & month <= 6 ~ "spring",
    month >= 7 & month <= 9 ~ "summer",
    month >= 10 & month <= 12 ~ "autumn",
    month >= 1 & month <= 3 ~ "winter"
  )) %>% 
  mutate(season = factor(season, levels = c("spring", "summer", "autumn", "winter"))) %>% 
  mutate(season2 = ifelse(month >= 5 & month <= 8, "rainy", "non-rainy")) %>% 
  filter(season %in% c("spring", "summer")) %>%
  # mutate(year = str_split(date, "年")[[1]][1], .before = date) %>% 
  # filter(is.na(year))
  group_by(year, season, month_week) %>% 
  summarise_all(.funs = mean, na.rm = T) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(x = month_week, y = sun_dur, color = season, group = 1)) +
  geom_point(aes(x = month_week, y = sun_dur, color = season)) +
  geom_rect(data = rainy_period, aes(xmin = st_month_week, xmax = ed_month_week, ymin = -Inf, ymax = Inf), alpha = .6,
            fill = "blue") +
  geom_rect(data = camping.period, aes(xmin = st_month_week, xmax = ed_month_week, ymin = -Inf, ymax = Inf), alpha = .6,
            fill = "yellow") +
  labs(x = NULL, y = "Sunshine duration"
       # title = sprintf("Year %d", year.display)
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("#00FF7F", "#FF3030", "#FF7F24", "#87CEFA")) +
  facet_wrap(~ as.factor(year), scales = "free_x") 
  





