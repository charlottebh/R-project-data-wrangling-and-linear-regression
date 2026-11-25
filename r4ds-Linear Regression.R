library(tidyverse)
library(dslabs)
library(HistData)

data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    lm(son ~ father, data = .) %>%
    .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0))+
  geom_histogram(binwidth = 5, color = "black")
p2 <- lse %>% ggplot(aes(beta_1)) +
  geom_histogram(binwidth = 0.1, color = "red")
grid.arrange(p1, p2, ncol =2)

#summary statistics 
sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>%
  summary %>%
  .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

#plot predictions and confidence intervals
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

#predict Y directly
fit <- galton_heights %>% lm(son ~ father, data=.)
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)
Y_hat

#plot best fit line
galton_heights %>% 
  mutate(Y_hat = predict(lm(son ~ father, data = .)))%>%
  ggplot(aes(father, Y_hat))+
  geom_line()


###################################
#Asessment 1
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

#Q3 
library (Lahman)
head(Teams)
dat <-Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_G = BB/G, R_per_G = R/G, HR_per_G = HR/G)
dat %>% lm(R_per_G ~ BB_per_G + HR_per_G, data = .)%>%
  summary()

model <- lm(son~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% 
  bind_cols(father = galton_heights$father)
ggplot(data, aes(x = father, y = fit))+
  geom_line(color = "green", size = 1)+
  geom_ribbon(aes(ymin = lwr, ymax=upr), alpha=0.2)+
  geom_point(data=galton_heights, aes(x=father, y=son))


#Assessment Part 2: LSE
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

fit <- female_heights %>% lm(mother ~ daughter, data = .)
fit
predictions <- predict(fit)
data2 <- as_tibble(predictions) %>% 
  bind_cols(mother = female_heights$mother)
data2
female_heights$mother

##################
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002)%>%
  mutate(pa = AB+BB, singles = (H - X2B -X3B - HR)/pa, bb = BB/pa)%>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

?Batting
#the following code has bugs
bat_03 <- Batting %>% filter(yearID %in% 1999:2001)%>%
  mutate(pa = AB+BB)%>%
  filter(pa >= 100) %>%
  group_by(playerID)%>%
  mutate(n_row = n()) %>%
  ungroup()%>%
  mutate(pa = AB+BB, singles =(H - X2B -X3B - HR)/pa, bb = BB/pa)%>%
  mutate(mean_singles = singles/n_row/3, mean_bb = bb/n_row/3)%>%
  select(playerID, singles, mean_singles, bb, mean_bb)

#the following code is corrected
bat_04 <- Batting %>%
  filter(yearID %in% 1999:2001) %>%
  group_by(playerID, yearID) %>%  #combine sints first
  summarize(AB = sum(AB), BB = sum(BB), H = sum(H),
            X2B = sum(X2B), X3B = sum(X3B), HR = sum(HR),
            .group = "drop")%>%
  mutate(pa = AB +BB) %>%
  filter(pa >= 100) %>%
  mutate(singles = (H - X2B -X3B -HR)/pa,
         bb = BB/pa) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles),
            mean_bb = mean(bb),
            n_seasons = n()
            )
  bat_04 %>% filter(mean_singles >0.2) %>% summarize(n())
  bat_04 %>% filter(mean_bb >0.2) %>% summarize(n())
  
?inner_join
comb <- inner_join(bat_02, bat_04, by = join_by(playerID)) 
head(comb)
str(comb)
comb %>% summarize(r=cor(singles, mean_singles))
?cor
comb %>% summarize(r2=cor(bb, mean_bb))

comb %>% ggplot(aes(x=mean_singles, y=singles))+
  geom_point()

comb %>% ggplot(aes(x=mean_bb, y=bb))+
  geom_point()
comb %>% ggplot(aes(x=mean_bb))+
  geom_histogram()
comb %>% ggplot(aes(x=bb))+
  geom_histogram()

fit_l <- lm(mean_singles ~ singles, data = comb)
fit_l
coef(fit_l)

fit_bb <- lm(bb ~ mean_bb, data = comb)
fit_bb

###################
###2.3.Advanced dplyr###########
#stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001)%>%
  mutate(HR = round(HR/G,1),
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R)%>%
  filter(HR >= 0.4 & HR <=1.2)
#calculate slope of regression line to predict runs
#by BB in different HR strata
dat %>%
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

#lm does not work with group_by
dat %>%
  group_by(HR)%>%
  lm(R~BB, data = .)%>%
  .$coef

#include lm inside summarize
dat %>%
  group_by(HR)%>%
  summarize(slope = lm(R~BB)$coef[2])

#tidy function from broom returns estimates in and information 
#in a data frame
library(broom)
fit <- lm(R~BB, data = dat)
tidy(fit)

tidy(fit, conf.int=TRUE)

#combine with group_by and summarize to get the table we want
dat %>%
  group_by(HR)%>%
  summarize(tidy(lm(R~BB), conf.int=TRUE))%>%
  filter(term == "BB")%>%
  select(HR, estimate, conf.low, conf.high)

dat %>%
  group_by(HR)%>%
  summarize(tidy(lm(R~BB), conf.int=TRUE))%>%
  filter(term == "BB")%>%
  select(HR, estimate, conf.low, conf.high)%>%
  ggplot(aes(HR, y=estimate, ymin = conf.low, ymax=conf.high))+
  geom_errorbar()+
  geom_point()
  

































