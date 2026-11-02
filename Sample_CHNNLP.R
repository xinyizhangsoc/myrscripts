## Sample Script: Chinese NLP and Analysis using Weibo Data
## Name: Xinyi Zhang
###-----------Project Description----------------------------
## This is a research project I conducted in 2021 using data scraped from the Chinese social media platform Weibo, where I investigated online discourse about proposed changes in immigration regulations on the Chinese internet. The project explores the racialized imagery of which immigrants are desired versus despised. Codes complete the following tasks:
##    a.  Creating a function to obtain text and other data from saved .html files, creating a dataset with texts and other relevant variables
##    b.  Using Chinese natural language processing techniques to clean data
##    c.  Latent semantic analysis of word associations for categories of interest

###-------------------------------------------###
### Clear Environment
rm(list = ls())

###------------load packages------------------###
library(rvest)
library(tidyverse)
library(reshape2)
library(tidyverse)
library(stringr)
library(quanteda)
library(jiebaR)
library(tidytext)
library(topicmodels)
library(ldatuning)
library(parallel)
library(lsa)
library(kableExtra)

##---------------------Scrape Data from Saved Html Pages—------------------##
### a function to get the data
get.posts <- function(page){
  nickname <- read_html(page) %>% html_nodes('.content .txt') %>% 
    html_attr('nick-name') %>% as_tibble()
  
  text <- read_html(page) %>% html_nodes('.content .txt') %>% 
    html_text() %>% str_trim() %>% as_tibble()
  
  action <- read_html(page) %>% html_nodes('.card-act') %>% 
    html_text() %>% str_trim() %>% as_tibble() 
  
  data <- cbind(nickname, text) %>%
    setNames(c("nickname","text")) %>%
    filter(!is.na(nickname)) %>% # filter out NA nicknames 
    filter(!grepl("展开全文", text)) # filter out duplicate posts 
  
  action <- colsplit(action$value, "\n", 
                     names = c("save", "forward", "comment", "like")) %>% 
    select(forward, comment, like) %>%
    mutate(forward = readr::parse_number(forward),
           comment = readr::parse_number(comment))
  
  data <- cbind(data, action)
  
  return(data)
}

## test function
page <- 'data/2020-03-04-p2.html'

get.posts(page) ### worked!

## go through all the files in the folder
# read in all file names
file.names <- list.files(path = "./data", pattern = "*.html")

# path names
file.names <- paste("data/", file.names, sep = "")

# create empty data frame to store data
data <- as_tibble()

## get posts for all pages saved in data
for (page in file.names){
  data <- rbind(data, get.posts(page))
}

## create dummy for censorship
data$censored <- rep(0)

# repeat steps for files in "data2," which are pages collected after March 9
file.names.censored <- list.files(path = "./data2", pattern = "*.html") 
file.names.censored <- paste("data2/", file.names.censored, sep = "")

data2 <- as_tibble()

for (page in file.names.censored){
  data2 <- rbind(data2, get.posts(page))
}

data2$censored <- rep(1)

## bind dataset into one
data <- rbind(data, data2)

# save the raw data file
write_csv(data, "data/raw_data.csv") 

##---------------------Cleaning and Pre-processing–—------------------##
## Pre-processing Chinese texts
data <- data %>% 
  mutate(text = as.character(gsub("#外国人永久居留管理条例#","", text))) %>% # remove the hashtag included in all posts
  mutate(text = as.character(gsub("#","", text))) %>% # remove hashtag signs
  mutate(text = as.character(gsub("收起全文d","", text))) %>% # remove some action items
  mutate(video = ifelse((grepl("微博视频", text)) == TRUE, 1, 0)) %>% # create dummy signaling video was included in the post
  mutate(text = as.character(gsub("微博视频|【|】|《|》|\\s|L","", text))) %>% # remove unusual characters, spaces
  mutate(text = as.character(gsub("zzp","走资派", text))) %>% 
  mutate(text = as.character(gsub("[[:punct:]]|[[:alnum:]]","", text))) %>% # remove English punctuations, numbers, and alphabets
  mutate(text = as.character(gsub("o|O","", text)))

# dictionary path
show_dictpath()

# create a customized dictionary for words specific to the corpus
custom_dic <- c("杨宜勇", "罗天昊","外国人永久居留管理条例", "外国人永久居留条例",
                "永久居留", "金灿荣","别样骚气", "孔明爆料", "布隆伯格",
                "舌尖大爆炸","奇葩图鉴","汉魂之心", "伟大复兴",
                "杨老师", "14亿人", "微博", "热搜", "中国人", "马未都", "凯勒奇",
                "21世纪", "潘庆林", "尼哥", "女大学生", "中国女孩", "中国男孩") %>% as_tibble()

write_delim(custom_dic, "custom_dic.txt")

# define parameters for segmentation engine
cutter <- worker(type = "mix", # "mixed" segmentation algorithms
                 stop_word = "stop_words.txt", # use chinese stop words list from jiebaR
                 user = "custom_dic.txt",
                 symbol = F,
                 bylines = T) # tokenize by documents

# test segmentation on first five rows
test.results <- cutter[data$text[1:5]] %>% tokens() # make into a token object
print(test.results[[5]])

# apply segmentation regime to all data
chn_tokens <- cutter[data$text] %>% tokens()

##------------------------------------Analysis—-----------–—------------------##
## Top Terms
## look up terms related to blackness and Africa in the dataset
length(grep("黑人|黑爷|小黑|黑绿|黑黑|非洲|尼日利亚|尼哥|黑哥|黑鬼", data$text)) # 123 posts
123/808

# restricting to uncensored subsample:
dat1 <- data %>% filter(censored == 0) 
length(grep("黑人|黑爷|小黑|黑绿|黑黑|非洲|尼日利亚|尼哥|黑哥|黑鬼", dat1$text)) # 122 posts
122/595

# compare, including "white people"
length(grep("白人|洋垃圾", data$text)) # posts that includes "white people: 15; posts that include "white people" or "foreign trash": 28

length(grep("白人|洋垃圾", dat1$text))
length(grep("白人|洋垃圾|欧洲｜美国", data$text))


## construct a dummy for posts referring to blackness and Africaness
data$black <- ifelse(grepl("黑人|黑爷|小黑|黑绿|黑黑|非洲|尼日利亚|尼哥|黑哥|黑鬼", data$text) == TRUE, 1, 0)

## cross-tabulation of posts referring to blackness and those containing videos
prop.table(table(data$black, data$video)) ## about equal distribution of posts including blackness between ones with and without videos

# cast tokens into a dfm
ch_dfm <- dfm(chn_tokens)
dim(ch_dfm) ### 808 documents 7691 terms
ch_dfm[,1:10] ### 96.0 sparsity

# trim dfm
ch_dfm_trim <- dfm_trim(ch_dfm,
                        min_termfreq = 10, termfreq_type = "count",
                        min_docfreq = 10, docfreq_type = "count")

dim(ch_dfm_trim) ## 808 docs 566 terms
ch_dfm_trim[,1:10]

# look at top features in the dataset, construct table
top.features <- topfeatures(ch_dfm, n = 20) %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  cbind(as_tibble(c("China", "foreigner", "regulation", "country", "solicit opinions",
                    "permanent residency", "Administration of Foreigner Permanent Residency",
                    "Ministry of Justice", "immigration", "draft", "against", "opinion",
                    "speak", "one", "public", "Yang Yiyong", "black people", "people", "will", "problem"))) %>% # add translation 
  setNames(c("Top Feature", "Term Frequency", "Translation"))

# generate table for latex
kable(top.features, "latex", booktabs = T, align = c("l", "c", "l"))

# weight dfm with tf-idf
ch_dfm_tfidf <- dfm_tfidf(ch_dfm)

topfeatures(ch_dfm_tfidf, n = 20) %>% as.data.frame()

### Latent Semantic Analysis
# convert to transposed matrix for lsa
lsa_mat = convert(ch_dfm_trim, to = "lsa")
lsa_mat = lw_logtf(lsa_mat) * gw_idf(lsa_mat)

# estimate lsa model
chn_lsa = lsa(lsa_mat)

# transform list to text matrix
chn_lsa_textmat = as.textmatrix(chn_lsa)

# find nearest associations for "black people"
black <- associate(chn_lsa_textmat, "黑人", "cosine", threshold = .2) %>%
  as.data.frame() %>% rownames_to_column() 

# select relevant for presentation
black <- black[c(2, 3, 4, 5, 6, 12, 13, 14, 17),] %>%
  cbind(as_tibble(c("Guangzhou", "white people", "female college students", "interracial marriage", "discrimination", "low", "Africa", "Chinese girls", "virus"))) %>% setNames(c("Term", "Cosine Similarity", "Translation"))

# africa  
africa <- associate(chn_lsa_textmat, "非洲", "cosine", threshold = .2) %>% # "Africa"
  as.data.frame() %>% rownames_to_column()

africa <- africa[c(2, 3, 4, 5, 7, 8, 9, 10, 14),] %>%
  cbind(as_tibble(c("scholarship", "money", "foreign students", "ability", "low", "standard", "refugee", "black", "black people"))) %>% setNames(c("Term", "Cosine Similarity", "Translation"))

# europe  
europe <- associate(chn_lsa_textmat, "欧洲", "cosine", threshold = .2) %>%# "Europe"
  as.data.frame() %>% rownames_to_column() 

europe <- europe[c(2, 3, 4, 6, 10, 11, 13, 15, 19),] %>%
  cbind(as_tibble(c("economy", "United States", "white people", "history", "opposing vote", "refugee", "black", "civilization", "demography"))) %>% setNames(c("Term", "Cosine Similarity", "Translation"))

# america
america <- associate(chn_lsa_textmat, "美国", "cosine", threshold = .2) %>% # "America"
  as.data.frame() %>% rownames_to_column() 

america <- america[c(2, 4, 5, 7, 9, 18, 19, 23, 26),] %>%
  cbind(as_tibble(c("Europe", "white people", "economy", "children", "citizenship", "civilization", "demography", "high end", "human capital"))) %>% setNames(c("Term", "Cosine Similarity", "Translation"))

# chinese nation
china <- associate(chn_lsa_textmat, "中华民族", "cosine", threshold = .2) %>% # "Chinese nation"
  as.data.frame() %>% rownames_to_column() 

china <- china[c(1, 4, 5, 6, 9, 11, 15, 16, 17),] %>%
  cbind(as_tibble(c("great rejuvenation", "children", "up", "encourage", "Germany", "contribution", "nation (minzu)", "break the law", "France"))) %>% setNames(c("Term", "Cosine Similarity", "Translation"))


lsa <- rbind(black, africa, europe, america, china)
rownames(lsa) <- NULL

## make table
kable(lsa, "latex", booktabs = T, align = c("l", "c", "l"
)) %>%
  pack_rows("Black People", 1, 9) %>%
  pack_rows("Africa", 10, 18) %>%
  pack_rows("Europe", 19, 27) %>%
  pack_rows("America", 28, 36) %>%
  pack_rows("China", 37, 45)
