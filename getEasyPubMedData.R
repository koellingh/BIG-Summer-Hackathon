library(tidyverse)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(Rcpp)
library(easyPubMed)
library(plyr)
# get data from easyPubMed
# gets all data matching query
out <- batch_pubmed_download(pubmed_query_string = "COVID-19 OR SARS-CoV-2", res_cn = 26)
# iterates through list of files (out) and converts them to .csv files
## note: only 25 of 396 are done here
for (i in 1:25) {
  df <- table_articles_byAuth(pubmed_data = out[i])
  # write csv to local dir
  write.csv(df, paste("C:/Users/koell/OneDrive - Carleton College/Extracurriculars/UCLA/Hackathon/epm_data/epm_COVID_", as.character(i + 25), ".csv", sep = ""))
}

#### SUBTASK - KEYWORD FREQUENCY
# narrow down to get keywords and turn into matrix
keywords <- dataset$keywords
a <- toString(keywords)
array <- str_split(a, "[;,]")
data <- sort(table(array))
# make wordcloud
dfwc <- as.data.frame(data)
dfwc_filter<-dfwc[!grepl("covid",dfwc$array),]
dfwc_filter<-dfwc_filter[!grepl("sars",dfwc_filter$array),]
dfwc_filter<-dfwc_filter[!grepl("corona",dfwc_filter$array),]
dfwc_filter %>% 
  with(wordcloud(array, Freq,random.order = FALSE, scale=c(1,.1), min.freq=5, colors=brewer.pal(8, "Dark2")))
# ggplot of frequency
df_order <- dfwc_filter %>% 
  arrange(desc(Freq))%>%
  slice(1:10) %>%
  ggplot(., aes(x=array, y=Freq, fill=array))+
  geom_bar(stat='identity')+
  coord_flip()+
  labs(x = "key words", y = "frequency", fill = "key words")
df_order

### MAIN TASK - AUTHOR FIELD FREQUENCY
# regex to get phrases from address of authors
addresses <- dataset$address
lAddresses <- str_split(toString(addresses), "[ ,/.;:]")
tAddresses <- as.data.frame(table(lAddresses))
regAddresses <- tAddresses %>%
  filter(str_detect(lAddresses, "ogy|iatry|ics|omy|Chem|Neuro|Gastro|Infect|Dent|Cardio|Comput|Phys|Bio"))
regAddresses %>% 
  with(wordcloud(lAddresses, Freq,random.order = FALSE, scale=c(1,.2), min.freq=5, colors=brewer.pal(8, "Dark2")))
sort(regAddresses$Freq)
# ggplot frequencies
df_order <- regAddresses %>% 
  arrange(sort(Freq))%>%
  slice(1:10) %>%
  ggplot(., aes(x=lAddresses, y=Freq, fill=lAddresses))+
  geom_bar(stat='identity')+
  coord_flip()+
  labs(x = "key words", y = "frequency", fill = "key words")

regAddresses <- sort(regAddresses$Freq
                  )
ggplot(regAddresses, )
df_order



# merge csv files (do once)
setwd("C:/Users/koell/OneDrive - Carleton College/Extracurriculars/UCLA/Hackathon/epm_data/")
dataset <- ldply(list.files(), read.csv, header=TRUE)
