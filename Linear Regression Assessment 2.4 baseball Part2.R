library(Lahman)
library(tidyverse)
library(broom)

dat <- Teams %>%
  filter(yearID == 1971) %>%
  mutate(BB = BB/G,
         HR = HR/G,
         R = R/G)
fit <- dat %>% lm(R ~ BB+HR, data = .)
tidy(fit)

?do()

dat2 <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID, teamID) %>%
  mutate(BB = sum(BB)/sum(G),
         HR = sum(HR)/sum(G),
         R = sum(R)/sum(G)) %>%
  select(yearID,teamID, BB, HR, R)
dat2


for (yearID in 1961:2018){
  fit2 = lm(R ~ BB+HR, data = dat2)
}
fit2

fit2 <- dat2 %>% 
  group_by(yearID)%>%
  summarize(lm(R ~ BB+HR, data = .)$coef[2])
fit2


tidy(fit3, conf.int = TRUE)


library(dplyr)
library(broom)
results <- dat2 %>%
  group_by(yearID)%>%
  do(tidy(lm(R ~ BB+HR, data = .), conf.int=TRUE))
print(results)

results %>%
  filter(term == "BB")%>%
  ggplot(aes(yearID, y=estimate, ymin= conf.low, ymax=conf.high))+
  geom_point()+
  geom_smooth()


dat3 <- results %>%
  filter(term == "BB")
dat3

#Q11. Not sure why the results of this question is incorrect.
fit3 <- dat3 %>% lm(estimate ~ yearID, data = .)
tidy(fit3, conf.int = TRUE)

#########################################################################




