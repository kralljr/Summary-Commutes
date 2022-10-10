load(here("data/rcomm2.RData"))
library(tidyverse)
library(nlme)
library(lme4)

lme2 <- lmer(PM ~ rness + (1 | ID) + (1 | ID: group), data = rcomm)
lme2 %>% tidy()

lme1 <- lme(PM ~ rness, random=~1|ID/group, data=rcomm)
lme1 %>% tidy

# fixed effect for ID
lme2 <- lmer(lPM ~ rness + ID +  (1 | ID: group), data = rcomm)
lme2 <- lme2 %>% tidy(conf.int = T)

lme1 <- lme(lPM ~ rness + ID, random=~1|ID/group, data=rcomm)
lme1 <- lme1 %>% tidy(conf.int = T)

lme1 <- mutate(lme1, type = "lme")
lme2 <- mutate(lme2, type = "lmer")
lmeall <- full_join(lme1, lme2)

ggplot(lmeall, aes(x = term, y = estimate, colour = type)) +
  geom_point(position = position_dodge(1)) +
  theme(axis.text.x= element_text(angle = 90, vjust = 0.5, hjust = 1))



