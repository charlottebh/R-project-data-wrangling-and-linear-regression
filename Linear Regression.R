#Linear regression

#baseball stats
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G)%>%
  ggplot(aes(HR_per_game, R_per_game))+
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB/G, R_per_game = R/G)%>%
  ggplot(aes(SB_per_game, R_per_game))+
  geom_point(alpha = 0.5)  

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G)%>%
  ggplot(aes(BB_per_game, R_per_game))+
  geom_point(alpha = 0.5)  

?Teams


Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G)%>%
  ggplot(aes(AB_per_game, R_per_game))+
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game = R/G, E_per_game = E/G)%>%
  ggplot(aes(R_per_game, E_per_game))+
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G)%>%
  ggplot(aes(X3B_per_game, X2B_per_game))+
  geom_point(alpha = 0.5)


# Correlation
install.packages("HistData")
library(tidyverse)
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

#means and sd (data is normally distributed)
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

#scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son))+
  geom_point(alpha = 0.5)

#correlation coefficient [-1,1]
galton_heights %>% summarize(cor(father, son)) # r=0.433

#sample correlation is a random variable
my_sample <- slice_sample(galton_heights, n = 25, replace = TRUE)
my_sample

set.seed(0)
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(cor(father, son))
R

set.seed(2)
B <- 5
N <- 25
R <- replicate(B, {
  slice_sample(galton_heights, n=N, replace = TRUE)%>%
    summarize(r=cor(father, son)) %>% .$r
})

data.frame(R) %>% ggplot(aes(R)) +
                    geom_histogram(binwidth = 0.05, color = "red")
sd(R)
mean(R)

#QQ-plot to evaluate if N is large enough

data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq()+
  geom_abline(intercept= mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))


Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G)%>%
  summarize(r = cor(X2B_per_game, X3B_per_game)) 

#Stratification
 conditional_avg <- galton_heights %>% 
   filter(round(father) == 72) %>%
     summarize(avg = mean(son)) %>%
     pull(avg)
 conditional_avg

 # stratify fathers' heights to make a boxplot of son heights
 galton_heights %>% mutate(father_strata = factor(round(father))) %>%
   ggplot(aes(father_strata, son)) +
   geom_boxplot()+
   geom_point()
#center of each boxplot
 galton_heights %>%
   mutate(father = round(father)) %>%
   group_by(father) %>%
   summarize(son_conditional_avg = mean(son)) %>%
   ggplot(aes(father, son_conditional_avg)) +
   geom_point()

 #add regression line to standardized data
 r <- galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)
 galton_heights %>%
   mutate(father = scale(father), son = scale(son)) %>% 
   #scale(): Transforms each variable to have a mean of 0 and a standard deviation (or variance) of 1 (z-scores).
   #to put variables on the same unit-less scale so that regression coefficients reflect standard deviation units
   mutate(father = round(father)) %>%
   group_by(father) %>%
   summarize(son=mean(son)) %>%
   ggplot(aes(father, son)) +
   geom_point()+
   geom_abline(intercept = 0, slope = r)
   
#add regression line to original data
 
 mu_x <- mean(galton_heights$father)
 mu_y <- mean(galton_heights$son)
 s_x <- sd(galton_heights$father)
 s_y <- sd(galton_heights$son)
 r <- cor(galton_heights$father, galton_heights$son)
 m <- r*s_y/s_x
 b <- mu_y - m*mu_x

 galton_heights %>%
   ggplot(aes(father, son))+
   geom_point(alpha = 0.5)+
   geom_abline(intercept = b, slope = m)
 
 galton_heights %>% 
   ggplot(aes(scale(father), scale(son))) + 
   geom_point(alpha = 0.5) +
   geom_abline(intercept = 0, slope = r) 
 
 
 galton_heights %>% 
   mutate(z_father = round((father - mean(father))/sd(father)))%>%
   filter(z_father %in% -2:2) %>%
   ggplot()+
   stat_qq(aes(sample=son)) +
   facet_wrap(~z_father)
 
 
 #Two regression lines
 #1. predict son's heights based on father's
 mu_x <- mean(galton_heights$father)
 mu_y <- mean(galton_heights$son)
 s_x <- sd(galton_heights$father)
 s_y <- sd(galton_heights$son)
 r <- cor(galton_heights$father, galton_heights$son)
 m_1 <-  r * s_y / s_x #slope
 b_1 <- mu_y - m_1*mu_x #intercept
mu_x
mu_y 
s_x
s_y 
r   
m_1   
b_1   

#2. predict father's heights based on son's
m_2 <- r*s_x/s_y
b_2 <- mu_x - m_2*mu_y
m_2
b_2

#3. plots
galton_heights |>
  ggplot(aes(father, son)) +
  geom_point(alpha=0.5) +
  geom_abline(intercept = b_1, slope = m_1, col = "blue")+
  geom_abline(intercept = -b_2/m_2, slope = 1/m_2, col ="red")
 
 
##########################################################
#Assessment Part 2. 
#########################################################
#analyze GaltonFamilies mother-daughter data
set.seed(1989)
set.seed(1989, sample.kind="Rounding")  
library(HistData) 
data("GaltonFamilies")

female_heights <- GaltonFamilies |>
  filter(gender == "female") |>
  group_by(family) |>
  sample_n(1) |>
  ungroup()|>
  select(mother, childHeight) |>
  rename(daughter = childHeight)

mu_mo <- mean(female_heights$mother)  
mu_dt <- mean(female_heights$daughter)
s_mo <- sd(female_heights$mother)
s_dt <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
mu_mo
mu_dt
s_mo
s_dt
r
m <- r*s_dt/s_mo  
m  
b <- mu_dt - m*mu_mo  
b  
r**2  
  
m*60 + b  
  
###########################
#Linear Models Overview
# find regression line for predicting runs from BBs (not shown in video)
library(tidyverse)
library(Lahman)  
get_slope <- function(x, y) cor(x,y)*sd(y)/sd(x)
bb_slope <- Teams |>
  filter(yearID %in% 1961:2001) |>
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  summarize(slope = get_slope(BB_per_game, R_per_game))

bb_slope

# compute regression line for predicting runs from singles (not shown in video)  
singles_slope <- Teams |>
  filter(yearID %in% 1961:2001)|>
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G)|>
  summarize(slope = get_slope(Singles_per_game, R_per_game))

singles_slope  
  
# calculate correlation between HR, BB, and singles
Teams |> 
  filter(yearID %in% 1961:2001) |>
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G)|>
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))

#stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams |>
  filter(yearID %in% 1961:2001) |>
  mutate(HR_strata = round(HR/G, 1),
         BB_per_game = BB/G,
         R_per_game = R/G) |>
  filter(HR_strata >= 0.4 & HR_strata<= 1.2)

#scatterplot for each HR stratum
dat |>
  ggplot(aes(BB_per_game, R_per_game))+
  geom_point(alpha=0.5)+
  geom_smooth(method = "lm") +
  facet_wrap(~HR_strata)
#calculate the regression line slope after stratifying by HR
dat |>
  group_by(HR_strata) |>
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))


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
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0 + beta1*galton_heights$father)
  return(sum(resid^2))
}

#plot RSS as a function of beta1 when beta0=25
beta1 = seq(0,1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0=25))
results %>% ggplot(aes(beta1, rss)) + geom_line()+
  geom_line(aes(beta1, rss))

# fit regression line to predict son's height from father's height
fit <- lm(son ~ father, data = galton_heights)
fit

#summary stat
summary(fit)


#Monte Carlo Simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    lm(son ~ father, data = .) %>%
    .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

#Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0))+geom_histogram(binwidth = 5, color = "black")
p2 <- lse %>% ggplot(aes(beta_1))+geom_histogram(binwidth = 0.1, color = "black")
grid.arrange(p1, p2, ncol = 2)

#summary statistics
sample_n(galton_heights, N, replace = TRUE)%>%
  lm(son ~ father, data = .)%>%
  summary %>%
  .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))
lse %>% summarize(cor(beta_0, beta_1)) #strongly correlated

#standardize father heights, which changes xi to xi-x_bar
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father -mean(father))%>%
    lm(son ~ father, data = .) %>%
    .$coef
})
cor(lse[1,], lse[2,]) #correlation becomes much smaller after standardization.


####Advanced dplyr Assessment Part 2 ######
library(tidyverse)
library(HistData)
data("GaltonFamilies") 
set.seed(1, sample.kind = "Rounding")
galton <- GaltonFamilies %>%
  group_by(family, gender)%>%
  sample_n(1) %>%
  ungroup() %>%
  gather(parent, parentHeight, father:mother)%>%
  mutate(child = ifelse(gender =="female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))
galton

galton %>% group_by(pair)%>%
  summarize(n())

galton %>% group_by(pair)%>%
  summarize(r= cor(parentHeight, childHeight))

head(galton)

library(broom)
galton %>% group_by(pair)%>%
 summarize(slope = lm(childHeight ~ parentHeight)$coef[2])

fit <- lm(childHeight ~ parentHeight, data = galton)
tidy(fit, conf.int = TRUE)

?reframe
#To return more or less than 1 row per group, use `reframe()`.
galton %>% group_by(pair)%>%
  reframe(tidy(
                lm(childHeight ~ parentHeight), conf.int = TRUE
                )
            )

galton %>% group_by(pair)%>%
  reframe(tidy(
    lm(childHeight ~ parentHeight), conf.int = TRUE
  )
  ) %>% 
  filter(term == "parentHeight")%>%
  select(pair, estimate, conf.low, conf.high) %>%
  ggplot(aes(pair,y=estimate, ymin = conf.low, ymax=conf.high)) +
  geom_errorbar()+
  geom_point()



#Regression and Baseball
?Lahman
library(Lahman)
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

fit <- Teams %>%
  filter(yearID %in% 1961:2001)%>%
  mutate(BB = BB/G,
         singles =(H - X2B -X3B -HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  lm(R ~ BB + singles +doubles +triples +HR, data = .)
coefs <- tidy(fit, conf.int=TRUE)
coefs


#predict number of runs for each team in 2002 and plot
Teams %>%
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) +
  geom_point() +
  geom_text(nudge_x=0.1, cex =2)+
  geom_abline()


#average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID ==2002)%>%
  group_by(teamID)%>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
  pull(pa_per_game)%>%
  mean
pa_per_game

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>%
  group_by(playerID)%>%
  mutate(PA = BB+AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles=sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G,
            triples = sum(X3B)/G,
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA))%>%
  filter(PA >= 300) %>%
  select(-G)%>%
  mutate(R_hat = predict(fit, newdata = .))

#plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, fill = I("purple"), color = I("black"))
  
  
#add 2022 salary of each player
players <- Salaries %>%
  filter(yearID ==2002)%>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

#add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>%
  filter(yearID == 2002)%>%
  group_by(playerID)%>%
  summarize_at(position_names, sum)%>%
  ungroup()
#create an index for the position name that's most common for a player
pos <- tmp_tab %>%
  select(position_names)%>%
  apply(., 1, which.max)
pos

players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos])%>%
  mutate(POS = str_to_upper(str_remove(POS,"G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by = "playerID")%>%
  filter(!is.na(POS) & !is.na(salary))

# add players' first and last names
# NOTE: In old versions of the Lahman library, the "People" dataset was called "Master"
# The following code may need to be modified if you have not recently updated the Lahman library.
players <- People %>%
  select(playerID, nameFirst, nameLast, debut)%>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by = "playerID")
head(players)
#top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat)%>%
  arrange(desc(R_hat))%>%
  top_n(10)

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS))+
  geom_point()+
  scale_x_log10()

#remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut)<1998)%>%
  ggplot(aes(salary, R_hat, color = POS))+
  geom_point()+
  scale_x_log10()


#Building a Better Offensive Metric for Baseball: Linear Programming
library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01"&debut >"1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <-rbind(constraint_matrix, salary = players$salary)
constraint_matrix
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int =TRUE)
lp_solution

our_team <- players %>%
  filter(lp_solution$solution ==1)%>%
  arrange(desc(R_hat))
our_team %>% select (nameFirst, nameLast, POS, salary, R_hat)

#We note that these players all except for one (Terry Shumpert) have above average BB and HR rates while the same is not true for singles.
my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))

?mad()
  



Sys.getenv("PATH")


install.packages("usethis")

# Set your name, email (use the same email as your GitHub account), and default branch
usethis::use_git_config(
  user.name = "charlottebh",
  user.email = "charlottehbs91@gmail.com",
  init.defaultBranch = "main"
)

usethis::create_github_token()

gitcreds::gitcreds_set()

usethis::use_git()  


  
  
  
  
  
  



















 
 
 
 
 
 







