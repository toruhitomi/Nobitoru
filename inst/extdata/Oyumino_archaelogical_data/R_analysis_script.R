library(rgdal)
library(tidyverse)

shp <- readOGR(dsn = "~/Downloads/oyumino/border/")
plot(shp)

cnt <- readOGR(dsn = "~/Downloads/oyumino/contour")
plot(cnt)

pit <- readOGR(dsn = "~/Downloads/oyumino/pithouses/")
plot(pit)

pithouse <- read_csv("~/Downloads/oyumino/pithousedata/pithousedata.csv")
potphase <- read_csv("~/Downloads/oyumino/potphase/potphase.csv")

gg.shp <- fortify(shp)
gg.pit <- fortify(pit)
gg.shp
g <- ggplot() +
  geom_polygon(data = gg.shp, aes(x = long, y = lat), color = "black", fill = "lightgrey", alpha= .3) +
  geom_point(data = as.data.frame(pit) %>% 
               rename(Pithouse_ID = Code) %>% 
               left_join(pithouse) %>% 
               left_join(potphase) %>% 
               mutate(Era = sprintf("%s~%s", START, END)) %>% 
               drop_na(START, END), aes(x = coords.x1, y = coords.x2, color = Era)) +
  # ggrepel::geom_text_repel(data = as.data.frame(pit) %>% dplyr::filter(coords.x1 > 30500, coords.x1 < 31500, coords.x2 > -49100, coords.x2 < 48900), 
  #                          aes(label = Code, x = coords.x1, y = coords.x2), max.overlaps = 100) +
  geom_point(data = data.frame(x = 29958.39, y = -48976.23), aes(x = x, y = y), shape = 3, size = 3, color = "green") +
  theme_bw() +
  labs(title = "Pit house locations in Oyumino")

plotly::ggplotly(g)

as.data.frame(pit) %>% 
  filter(str_detect(Code, "KAT"))


pithouse %>% 
  as_tibble() %>% 
  filter(str_detect(Pithouse_ID, "KAT"))

