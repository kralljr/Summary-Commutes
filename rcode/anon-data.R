# file to anonimize data
library(here)
library(tidyverse)
library(stringr)

# load data
load(here("data/rcommQ.RData"))

set.seed(1509092756)
# create new id
unids <- unique(rcommQ$ID) # %>% sort()
newid <- sample(seq(100, 999), length(unids)) #%>% sort()
# newid <- as.numeric(substring(unids, 6)) + 40
# newid <- paste0("GMU10", str_pad(newid, 2, "left", "0"))
idlink <- data.frame(ID = unids, newid = paste0("id", newid))

# create new date
undate <- unique(rcommQ$date_local) #%>% sort()
newdate <- sample(seq(100, 999), length(undate)) #%>% sort()
newdate <- paste0("d", newdate)
datelink <- data.frame(date_local = undate, newdate = newdate)
#datelink <- mutate(datelink, date_local = as.character(date_local))


data <- rcommQ %>%
  dplyr::select(-c(id3, rdatetime)) %>%
  full_join(idlink) %>%
  full_join(datelink) %>%
  dplyr::select(-c(date_local, ID)) %>%
  rename(ID = newid, date_local = newdate) %>%
  mutate(commid = paste(date_local, group))
    # ID = factor(ID, levels = idlink$ID, labels = idlink$newid),
    #      ID = as.character(ID),
         # date_local2a = as.character(date_local),
         # date_local = factor(date_local2a, levels = datelink$date_local, labels = datelink$newdate)) %>%
  # dplyr::select(-date_local2a)


# remove: lPM (log of PM)
data <- dplyr::select(data, ID, date_local, group, commid, timemin, PM, rtype, mph, qmph, contains("srness"),
                       daily, obsdiff, awnd, tmax, tmin, tavg, RH,
                       cat5sm, prcpbin, snowbin, rushmorn, rusheven)
colnames(data)
write.csv(data, file = here("data/roadden.csv"), row.names = F)
