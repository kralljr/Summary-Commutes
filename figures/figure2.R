# libraries
library(tidyverse)
library(knitr)
library(here)

savedir <- "~/Dropbox/CommuteVar/figures"

# load data
load(here("data/vah.RData"))
load(here("data/gestdc-dates.RData"))

obsdiff <- left_join(dates, vah0) %>%
  mutate(month = month(date_local), day = day(date_local),
         cold = ifelse((month == 12 & day >= 21) | (month %in% c(1, 2)) | (month == 3 & day < 20) , "Winter",
                       ifelse(month > 8, "Fall", "Spring")), fmonth = factor(month),
         smonth = month - 13 + (month < 9) * 12,
         cold = factor(cold, levels = c("Fall", "Winter", "Spring")))

lab1 <- c("Sep", "Jan",  "May")
cols <- c("#e69f00", "#56b4e9","#000000" )
alps <- c(0.7, 0.8, 0.7)


fighourly <- ggplot(obsdiff, aes(x = time_local, y = obsdiff, group = date_local, colour = cold,
                    alpha = cold)) +
  geom_line() +
  scale_color_manual(name = "Season", values = cols) +
  scale_alpha_manual(name = "Season", values = alps) +
  # scale_color_gradient2(name = "Month", labels = lab1, breaks = c(-4,0, 4 ),
  #                       low = "orange", mid = "blue", high = "green")+
  ylab(expression(paste("Change in PM"[2.5], " (", mu, "g/m"^3, ")"))) +#, #"\nrelative to daily average"))) +
  xlab("Hour of day") +
  theme_bw() +
  theme(text = element_text(size = 24))
fighourly
ggsave(file.path(savedir, "figure2-hourly.png"))

fighourly <- list(fighourly, obsdiff, alps, cols)
save(fighourly, file = "~/Documents/git/Summary-Commutes/figures/fighourly.RData")
