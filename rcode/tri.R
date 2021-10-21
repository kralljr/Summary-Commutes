# assess tri

library(tidyverse)
library(here)
library(janitor)
library(RColorBrewer)

# location data
load(here("data/MainStates1.RData"))


# tri VA
tri2018 <- read.csv(here("data/tri_2018_va.csv"))
tri2019 <- read.csv(here("data/tri_2019_va.csv"))
tri <- bind_rows(tri2018, tri2019)
cnames <- c("ALEXANDRIA (CITY)","ARLINGTON","FAIRFAX","FAIRFAX (CITY)",
            "LOUDOUN"  ,"PRINCE WILLIAM" ,
            "FALLS CHURCH (CITY)")
triva <- filter(tri, `X7..COUNTY` %in% c(cnames))


# tri MD
tri2018 <- read.csv(here("data/tri_2018_md.csv"))
tri2019 <- read.csv(here("data/tri_2019_md.csv"))
tri <- bind_rows(tri2018, tri2019)
cnames <- c("MONTGOMERY", "PRINCE GEORGE&apos;S")
trimd <- filter(tri, `X7..COUNTY` %in% c(cnames))


# tri DC
tri2018 <- read.csv(here("data/tri_2018_dc.csv"))
tri2019 <- read.csv(here("data/tri_2019_dc.csv"))
tridc <- bind_rows(tri2018, tri2019)

# merge
tri <- full_join(triva, trimd) %>% full_join(., tridc)



tri <- select(tri, `X34..CHEMICAL`, `X103..TOTAL.RELEASES`,
              `X7..COUNTY`, `X12..LATITUDE`, `X13..LONGITUDE`) %>%
  rename(county = "X7..COUNTY", Latitude = "X12..LATITUDE",
         Longitude = "X13..LONGITUDE", Chemical = "X34..CHEMICAL",
         TotalRelease = "X103..TOTAL.RELEASES")

highest <- filter(tri, TotalRelease > 10000) %>%
  mutate(sChem = substr(Chemical, 1, 5))


cols <- brewer.pal(8, "Dark2") %>% rep(7)
g2 <- ggplot() +
  geom_polygon( data=MainStates1, aes(x=long, y=lat, group = group),
                color="grey50",fill = "white") +
  geom_point(data = tri, aes(x = Longitude, y = Latitude), alpha = 0.3) +
  geom_jitter(data = highest, aes(x = Longitude, y = Latitude, colour= sChem), width = 0.02 , height = 0.02) +

  #scale_color_manual(values = cols) +
  #theme(legend.position = "none") +
  xlab("Longitude") + ylab("Latitude")
ggsave(g2, file = here("plots/tri.png"), width = 5, height = 4, units = "in")

