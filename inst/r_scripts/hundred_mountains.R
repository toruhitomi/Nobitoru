library(XML)
library(htmlwidgets)
library(rvest)
library(magrittr)
library(leaflet)
library(leaflet.providers)
convert_coords <- function(coord.min, coord.sec) {
  (as.numeric(coord.min) * 60 + as.numeric(coord.sec))/3600
}


mt.df <- openxlsx::read.xlsx("inst/extdata/hundred_mountains_of_Japan.xlsx") %>% 
  dplyr::select(No, 山名, Name, Pronunciation, Altitude, Prefecture)

for (ii in 1:nrow(mt.df)) {
  # fix mountain names
  url <- dplyr::case_when(
    ii == 4 ~ "https://ja.wikipedia.org/wiki/雌阿寒岳",
    ii == 9 ~ "https://ja.wikipedia.org/wiki/羊蹄山",
    ii == 17 ~ "https://ja.wikipedia.org/wiki/朝日岳_(山形県・新潟県)",
    ii == 18 ~ "https://ja.wikipedia.org/wiki/蔵王連峰",
    ii == 26 ~ "https://ja.wikipedia.org/wiki/平ヶ岳_(群馬県・新潟県)",
    ii == 48 ~ "https://ja.wikipedia.org/wiki/剱岳",
    ii == 52 ~ "https://ja.wikipedia.org/wiki/水晶岳",
    ii == 68 ~ "https://ja.wikipedia.org/wiki/金峰山_(山梨県・長野県)",
    ii == 92 ~ "https://ja.wikipedia.org/wiki/大山_(鳥取県)",
    TRUE ~ sprintf("https://ja.wikipedia.org/wiki/%s", mt.df$山名[ii])
  )
  
  html_data <- read_html(url)
  
  coords <- html_data %>% 
    html_elements("span") %>%
    html_elements(xpath = "span[@class = 'geo']") %>%
    html_text() %>% 
    # stringr::str_subset("緯|経") %>% 
    unique()
  
  if (length(coords) > 0) {
    lat <- as.numeric(stringr::str_split(coords, "; ")[[1]][1])
    lon <- as.numeric(stringr::str_split(coords, "; ")[[1]][2])
    mt.df$lat[ii] <- lat
    mt.df$lon[ii] <- lon
  } else {
    coords2 <- html_data %>% 
      html_elements(".nourlexpansion .text span") %>% 
      html_text()
    
    if (length(coords2) > 0) {
      if (stringr::str_detect(coords2, "北緯")) {
        lat <- strsplit(gsub("北緯|秒", "", stringr::str_split(coords2, "東経")[[1]][1]), "度|分")[[1]]
        lon <- strsplit(gsub("東経|秒", "", stringr::str_split(coords2, "東経")[[1]][2]), "度|分")[[1]]
        
        mt.df$lat[ii] <- as.numeric(lat[1]) + convert_coords(lat[2], lat[3])
        mt.df$lon[ii] <- as.numeric(lon[1]) + convert_coords(lon[2], lon[3])
      } 
    } else {
      mt.df$lat[ii] <- NA
      mt.df$lon[ii] <- NA
    }
  }
}

# mt.df[is.na(mt.df$lat), c("No","山名")]
write_excel_csv(mt.df, "inst/extdata/list_100_mountains.csv")


print(mt.df %>% 
        dplyr::mutate(Name_Altitude = sprintf("%s_%dm", Name, Altitude)) %>% 
        leaflet() %>% 
        addTiles() %>% 
        addMarkers(lng = ~lon, lat = ~lat, popup = ~Name_Altitude))
  
