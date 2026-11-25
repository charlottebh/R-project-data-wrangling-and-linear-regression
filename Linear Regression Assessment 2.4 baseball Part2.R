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
#Chapter 2 final assessment
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

#Q1
dat <- Teams_small %>%
  mutate(R = R/G,
         HR = HR/G)
dat %>%
  summarize(lm(avg_attendance ~ R, data = .)$coef[2])

dat %>%
  summarize(lm(avg_attendance ~ HR, data = .)$coef[2])

head(Teams_small)
Teams_small %>% summarize(lm(avg_attendance ~ W, data = .)$coef[2])
fit <- lm(avg_attendance ~ W, data = Teams_small)
tidy(fit)
Teams_small %>% summarize(lm(avg_attendance ~ yearID, data = .)$coef[2])

#Q2 Are wins and runs per game or wins and home runs per game correlated?

library(dplyr)
Teams_small2 <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G,
         R_per_G = R/G,
         HR_per_G = HR/G)
Teams_small2 %>% summarize(r=cor(W, R_per_G))
Teams_small2 %>% summarize(r=cor(W, HR_per_G))


#Q3 








