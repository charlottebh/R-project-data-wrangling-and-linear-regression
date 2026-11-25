#data wrangling
#assessment Part 1

library(dslabs)
library(lubridate)
options(digits = 3)

data("brexit_polls")
head(brexit_polls)
brexit_polls %>% filter(month(startdate) == 4) %>%
  count()
?round_date
class(brexit_polls$enddate) #the format is Date, case sensitive
brexit_polls %>% filter(round_date(enddate, unit= "week", getOption("lubridate.week.start", 7)) == 
                          round_date(as.Date("2016-06-12"), unit ="week", getOption("lubridate.week.start", 7))) %>%
  count()

brexit_polls %>% mutate(weekday = weekdays(enddate)) %>%
  group_by(weekday) %>%
  summarize(n=n())%>%
  arrange(desc(n))


#Q5
data(movielens)
movielens %>% mutate(year = year(as_datetime(timestamp)))%>%
  group_by(year)%>%
  summarize(n=n()) %>%
  arrange(desc(n))

movielens %>% mutate(hr = hour(as_datetime(timestamp)))%>%
  group_by(hr)%>%
  summarize(n=n()) %>%
  arrange(desc(n))

#Assessment Part2
library(tidyverse)
install.packages("gutenbergr")
library(gutenbergr)
library(tidytext)
options(digits = 3)
library(dplyr)

gutenberg_metadata
#df <-gutenberg_metadata %>%
#  filter(
#    str_detect(str_to_upper(title), "^(PRIDE)\\s+(AND)\\s+(PREJUDICE)$")
#  ) 
df <-gutenberg_metadata %>%
  filter(
    str_detect(str_to_upper(title), "PRIDE\\sAND\\s+PREJUDICE")
  ) 
df
df %>%
 summarize(n_distinct(as.character(gutenberg_id)))
  
?gutenberg_works

gutenberg_works(languages = "en", all_languages = FALSE)%>%
  filter(
    str_detect(str_to_upper(title), "PRIDE\\sAND\\s+PREJUDICE")
  ) %>%
  select(gutenberg_id)

book <- gutenberg_download(
  gutenberg_id = 1342,
  mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg/",
  strip = TRUE,
  verbose = TRUE
)
words <- book %>% unnest_tokens(output = word, input = text, token = "words",
              format = "text") 
stop_words %>% summarise(n())
words %>% summarise(n())
present_stop_words <- inner_join(words, stop_words, by="word")
present_stop_words 
present_stop_words %>% summarise(n())
words
remain <-words %>% filter(!word %in% present_stop_words$word) 
remain %>% summarise(n())

remain2 <- remain %>% filter(!str_detect(word, "\\d+"))
remain2 %>% summarise(n())
remain3 <- remain2 %>% group_by(word) %>% mutate(cnt=n()) %>% filter(cnt>100) 
remain3 %>% summarise(n()) %>% summarise(n()) 
remain4 <- remain3 %>% summarise(n=n()) 
remain4 %>% arrange(desc(n))


afinn <- get_sentiments("afinn")
afinn
afinn_sentiments <- inner_join(remain2, afinn, by="word")
afinn_sentiments
afinn_sentiments %>% group_by("word") %>% summarise(n())
afinn_sentiments %>% group_by("word") %>% filter(value > 0) %>% summarise(n()/6353)
afinn_sentiments %>% group_by("word") %>% filter(value ==4) %>% summarise(n())


#Final project - Hurricane Maria
library(tidyverse)
install.packages("pdftools")
library(pdftools)
options(digits = 3)


#locating and reading PDF
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system2("open",args =fn)
?pdf_text
txt <- pdf_text(fn)
str_which(txt, "SEP")
page_9 <- txt[9]
page_9

#Splitting into lines
library(stringr)
x <- str_split(page_9, "\n") #Splits the page at newlines. Returns a list of length 1.
x2 <- str_split(page_9, "\n")[[1]] #Same split, but extracts the character vector (41 lines)
x2
length(x) #1
length(x2) #41
s <- x[[1]] #same as x2
s
length(s)

s2<- str_trim(s) #Removes leading and trailing whitespace on each line.
s2[[1]]
length(s2)

?str_which
header_index <- str_which(s2,"2015")[1] #Index of the first line containing “2015” (the header row).
header_index
s2[header_index]

s3 <- str_split(s2[header_index], "\\s+") #Splits the header on runs of whitespace.
month <- s3[[1]][1]
month
header <- s3[[1]][2:5]
header

tail_index <- str_which(s2, "Total")
tail_index
s2[tail_index]
s2[str_detect(s2, "\\d+")]
n <-  as.numeric(str_count(s2[str_detect(s2, "\\d+")=="TRUE"])) #Counts characters per line (empty pattern).
n[n<10]

#Building the table
s4 <- s2[(header_index+1):(tail_index -1)]
s4
s5 <- str_split(s4, "\\s+") #splits each row into tokens
lengths(s5) #tokens per row; should be 5

#find malformed rows:
bad <- which(lengths(s5) != 5)
s5[bad]

s5 <- str_split(str_trim(s4), "\\s+")
#try to drop stray one-token lines
s6 <- s5[!(str_detect(s5, "\\d") & lengths(s5) == 1)]
s6
length(s6) #30

#bug #s6 is a list, so it’s coerced to text like 'c("1", "65", "67"...)' and then split, producing garbage like c("1",.
#s7<- str_split_fixed(s6, "\\s+", n = 6)[,1:5]
#s7
#colnames(s7) <- c("day", header)
#s7
#data.frame(s7)
str(s6)
class(s6)

#Stack the rows into a character matrix.
s7 <- do.call(rbind, lapply(s6, `[`, 1:5))
#do.call(rbind, s6) suffices once rows are validated.
s7

colnames(s7) <- c("day", header)
s7

dims <- dim(s7)
s7_num <- as.numeric(s7)
dim(s7_num) <- dims
s7_num
colnames(s7_num) <- c("day", header)
s7_num

#Analysis and plot

mean(s7_num[, "2016"])

mean(s7_num[1:19,"2017"])

s7_num[1:19,"2017"]

mean(s7_num[20:30,"2017"])


tab <- s7_num %>%
  as_tibble() %>%
  gather(year, deaths, -day) %>% #use pivot_longer
  mutate(year = as.numeric(year))
tab

tab |> filter(year != "2018") |>
  ggplot(aes(day, deaths, color = factor(year)))+
  geom_line()+
  geom_vline(xintercept = 20)



################################################
#The following is a cleaned-up version:

library(tidyverse)
library(pdftools)
options(digits = 3)

# --- 1. Read the PDF -------------------------------------------------------
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf",
                  package = "dslabs")
stopifnot(nzchar(fn))                      # fails if dslabs isn't installed

txt <- pdf_text(fn)

# --- 2. Split page 9 into trimmed lines ------------------------------------
lines <- txt[9] |>
  str_split("\n") |>
  pluck(1) |>
  str_trim()

# --- 3. Locate header and footer -------------------------------------------
header_index <- str_which(lines, "2015")[1]
header_fields <- str_split(lines[header_index], "\\s+")[[1]]
month  <- header_fields[1]
header <- header_fields[2:5]               # "2015" "2016" "2017" "2018"

total_idx  <- str_which(lines, "Total")
tail_index <- total_idx[total_idx > header_index][1]

# --- 4. Extract the table body ---------------------------------------------
body <- lines[(header_index + 1):(tail_index - 1)]
rows <- str_split(body, "\\s+")

# keep only complete rows: day + one value per year
rows <- rows[lengths(rows) == length(header) + 1]
stopifnot(length(rows) == 30)              # September has 30 days

mat <- do.call(rbind, rows)
#rbind(rows)   # gives a 1-column "list-matrix", not the 30 x 5 matrix you want
#do.call(rbind, rows) does the unpacking for you, making it equivalent to writing out all 30 arguments by hand. 
#It works the same whether you have 30 rows or 300.
#Alternative solutions:
#matrix(unlist(rows), ncol = 5, byrow = TRUE)   # flatten, then reshape
#purrr::reduce(rows, rbind)                     # works, but is slower and clunkier
colnames(mat) <- c("day", header)
storage.mode(mat) <- "numeric"             # convert text to numbers;keeps dim and dimnames

# --- 5. Tidy format --------------------------------------------------------
tab <- mat |>
  as_tibble() |>
  pivot_longer(-day, names_to = "year", values_to = "deaths") |>
  mutate(year = as.integer(year))

# --- 6. Summary: 2017 before vs. after Maria (Sept 20) ---------------------
tab |>
  filter(year == 2017) |>
  group_by(period = if_else(day < 20, "Before Sept 20", "Sept 20 onward")) |>
  summarize(mean_deaths = mean(deaths))

# --- 7. Plot ---------------------------------------------------------------
tab |>
  filter(year != 2018) |>
  ggplot(aes(day, deaths, color = factor(year))) +
  geom_line() +
  geom_vline(xintercept = 20, linetype = "dashed") +
  annotate("text", x = 20.5, y = max(tab$deaths, na.rm = TRUE),
           label = "Hurricane Maria", hjust = 0) +
  labs(title = paste("Daily deaths in Puerto Rico,", month),
       x = "Day of month", y = "Deaths", color = "Year") +
  theme_minimal()


























