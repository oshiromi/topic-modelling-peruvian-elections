library(RJSONIO)
library(twitteR)
library(ROAuth)
library(streamR)
library(bit)
library(grid)
library(maps)
library(rtweet)
library(base64enc)
library(httr)
library(bit64)
library(rtweet)
library(rvest)
library(jsonlite)
library(httpuv) 
library(dplyr)
library(tidytweetjson)
library(tm)
library(quanteda)
library(tidyverse)
library(SnowballC)
library(stringr)
library(textclean)
library(dplyr)
library(tidyr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(wordcloud)
library(stopwords)
library(stringi)
library(tidyverse)
library(cowplot)


###### Big Data for Social Sciences // Winter 2022
###### Prof. Mihaly Fazekas
###### Alessandra Oshiro

###### Chronicle of an Impeachment Foretold? Exploring the Reasons Why Peruvians Want Castillo Out
##### The aim of this topic is to explore the main themes behind the support for Pedro Castillo's impeachment. 
##### For this, Twitter data will be analyzed in two steps: 1) LDA topic modelling, 2) Dictionary-approach. 
##### The idea is to identify the topics and see how their presence varies between February 15th and March 15th. 
##### In the final stage of the project, also tweets around the impeachment debate of March 28th were added (March 26th - 30th).

#### NOTE: The data collection code is commented out to avoid scraping data in case of running all the script.

#### Data Collection with Twitter API
### Setting Up the API
## Keys
#access 
#access_secret 
#consumer 
#consumer_secret 

## Twitter connection
#setup_twitter_oauth(consumer_key=consumer,
                    #consumer_secret=consumer_secret,access_token=access,
                    #access_secret=access_secret)

## Rtweet package token
#create_token(app = "peruvian_election_2021",
             #consumer_key=consumer,
             #consumer_secret=consumer_secret,
             #access_token=access,
             #access_secret=access_secret)

### Building Up the Dataset
## To scrape the first tweets, popular hashtags among supporters of Pedro Castillo's impeachment were used as keywords.
## Fortunately, the specification of the language and the location (mostly unavailable) is not needed because the hashtags are unique to Peru. 
## Retweets are excluded from the scraping to avoid inflating the topic proportion. 
#bigdata_df <-  search_tweets("#VacanciaPresidencialYa OR #VacanciaPedroCastilloYa 
                             #OR #VacanciaPedroCastillo OR #CastilloRenunciaYa OR #FueraCastillo",
                             #n = 200,
                             #include_rts = FALSE)

## To expand the first dataset, a second dataframe was created, using the same code. 
#new_df <- search_tweets("#VacanciaPresidencialYa OR #VacanciaPedroCastilloYa 
                        #OR #VacanciaPedroCastillo OR #CastilloRenunciaYa OR #FueraCastillo",
                        #n = 200, 
                        #include_rts = FALSE)

## The two resulting dataframes were merged. 
## Duplicates were dropped using the 'tweet id' variable as primary key. 
#bigdata_df <- rbind(bigdata_df, new_df)
#bigdata_df <- distinct(bigdata_df, id, .keep_all = TRUE)

## The dataset was further expanding by scraping tweets daily.
## For the first week, the code was run around three times a day, requesting 500 tweets. 
## After the presentation, this was switched to running the code once, by the end of the day, and requesting 5000 tweets at once. 
## The second approach had big amounts of duplicates, but it was overall more effective for expanding the dataset. 
## The "since" argument was used in an attempt to have tweets for everyday.
#new_df <- search_tweets("#VacanciaPresidencialYa OR #VacanciaPedroCastilloYa 
                        #OR #VacanciaPedroCastillo OR #CastilloRenunciaYa OR #FueraCastillo",
                        #n = 5000, 
                        #include_rts = FALSE,
                        #since = "2022-03-15")

## The new dataset was added to the former one, thus increasing the number of tweets daily. 
## Duplicates were dropped using the "tweet id" variable as primary key after every rbind(). 
#bigdata_df <- rbind(bigdata_df, new_df)
#bigdata_df <- distinct(bigdata_df, id, .keep_all = TRUE)

## Given that the analysis involves time series, the number of tweets per date was constantly checked. 
## Unfortunately, the API was unable to provide tweets for some specific dates.
## Additionaly, there is high variation between the number of tweets obtains for each date. 
#clean_df <- add_date(bigdata_df)
#clean_df %>% count(date)

### Saving the Dataset 
## The data was saved as a csv file after every addition. Only relevant variables were kept. 
#clean_df <- clean_df[c('date','created_at', 'id', 'full_text', 'retweet_count', 'favorite_count', 'text')]
#write.csv(clean_df,"/Users/ale/desktop/big_data.csv", row.names = FALSE)

#### Description of the Dataset 
### Loading the datasets
## Pre-Impeachment dataset
impeachment <- read.csv("~/Desktop/Big Data/big_data.csv")

## Post-Impeachment dataset
# This dataset was built with the same code as the "impeachment" dataset. The dates go from March, 26th to March, 30th. 
# The impeachment was debated, and rejected on March, 28th. 
# The inclusion of this data was not part of the original project, as there was no approved motion by the time it was outlined.
# However, the post-impeachment dataset was included after the suggestion of doing a topic comparison during the class feedback. 
post_impeachment <- read.csv("~/Desktop/Big Data/post_imp.csv")

### Total number of observations before the data cleaning
total_obs_pre <- nrow(impeachment)
total_obs_pre

total_obs_post <- nrow(post_impeachment)
total_obs_post


### Number of observations per day. 
## As the plot shows, the datasets are unbalanced in the amount of tweets per day. 
## There is one missing date (16/02) in the "impeachment" dataset. The Twitter API was unable to retrieve tweets from that date for unknown reasons.
impeachment$date <- as.Date (impeachment$date)
impeachment %>% count(date)

ggplot(impeachment, aes(date)) + 
  geom_bar(na.rm=TRUE) +
  ylim(0, 6000) + 
  scale_x_date(date_label="%e/%m", breaks = "1 day", expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Number of Tweets per Day ('Impeachment' Dataset)")

post_impeachment$date <- as.Date (post_impeachment$date)
post_impeachment %>% count(date)

ggplot(post_impeachment, aes(date)) + 
  geom_bar(na.rm=TRUE) +
  ylim(0, 4500) + 
  scale_x_date(date_label="%e/%m", breaks = "1 day", expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Number of Tweets per Day ('Post-Impeachment' Dataset)")

#### Text Analysis 
### Preparing the Data for the Analysis
## Both the 'Impeachment' Dataset and the 'Post-Impeachment' Dataset will be cleaned.
## Only the relevant variables were kept. The 'Post-Impeachment' Dataset was saved only with these relevant variables.
impeachment <- impeachment[c('date', 'id', 'text')]

## The tweets were cleaned so that they were useful for the text analysis 
# A new variable was created to store the clean text.
# I kept the original tweets as a separate variable to check if the cleaning was working correctly. 
# To avoid encoding problems, everything was converted to "UTF-8". 
impeachment$clean_text <- iconv(impeachment$text, to='UTF-8', sub = "byte")
post_impeachment$clean_text <- iconv(post_impeachment$text, to='UTF-8', sub = "byte")

# Mentions and URLs were removed, because they are not relevant for the analysis. 
impeachment$clean_text <- gsub("@\\w+", "", impeachment$clean_text)
impeachment$clean_text <- gsub("https?://.+", "", impeachment$clean_text)

post_impeachment$clean_text <- gsub("@\\w+", "", post_impeachment$clean_text)
post_impeachment$clean_text <- gsub("https?://.+", "", post_impeachment$clean_text)

# All words were converted to lower case to avoid missing words due to wrong capitalization or unevenly capitalized hashtags.
impeachment$clean_text <- tolower(impeachment$clean_text)

post_impeachment$clean_text <- tolower(post_impeachment$clean_text)

# Punctuation was removed, including the "#" sign. Hashtags are still identifiable because they lack spaces between words.
impeachment$clean_text <- gsub("[[:punct:]]", " ", impeachment$clean_text)

post_impeachment$clean_text <- gsub("[[:punct:]]", " ", post_impeachment$clean_text)

# Accented letters were replaced by unaccented ones. 
# This is important because Spanish uses accented letters, but it is not uncommon to omit in online text. 
# Fortunately, accents do not make a difference in the words relevant for this analysis. 
impeachment$clean_text <- stri_trans_general(impeachment$clean_text, "Latin-ASCII")

post_impeachment$clean_text <- stri_trans_general(post_impeachment$clean_text, "Latin-ASCII")

# Stopwords and additional general words were removed. 
# The hashtags used as keywords in the scraping were also removed, as they would show as the most frequent words otherwise.
# The process of building the list of words was iterative. That is, I updated it based on the outcomes of the LDA topic modeling. 
# The "removable" word list includes some prepositions, adverbs, verbs, pronouns, references to Castillo, and other hashtags that clearly state support for his impeachment, but without signaling any intention. 
stopwords <- stopwords("spanish")
keywords <- c("vacanciapedrocastilloya", "vacanciapedrocastillo", "vacanciapresidencialya", "fueracastillo", "castillorenunciaya")
removable_words <-c("vacanciaya", "castillorenunciaya", "castillo", "pedro", "vacancia", "q", "si",
                    "solo", "vacanciapedrocastilloydinaboluarte", "hoy", "ahora", "ser", "renunciacastillo", "mas", "va", "x", "marte", "vamos", "quiere",
                    "vacanciacastilloyboluarte", "favor", "d", "hacer", "vez", "hace", "dice", "van", "mejor", "yaaa", "sabe", "siempre", "toda", "gracias",
                    "perú", "asi", "vez", "esta", "puede", "dia", "tambien", "da", "manana", "claro", "quiero", "ningun", "debe", "vacanciaa", "renuncia", "campo", "chanchita", "estan", "gran", "xq",
                    "decir", "pedrocastillo", "estan", "acusacioncastilloya", "peru", "87votos", "congreso87votosya", "gente", "bien", "tiempo", "congreso", "gobierno", "sera", "quieren",
                    "ustedes", "queremos", "presidente", "aun", "igual", "votos", "hora", "queremos", "plaza", "palacio", "cada", "ver", "pide", "renuncia","siguen", "voto", 
                    "representa", "lado", "ahi", "quieres", "anos", "urgente", "sombrero", "chota", "exije", "llego", "dos", "etc", "cualquier", "usted", "sosperu", "presidencial", 
                    "niunamenos", "mal", "uds", "pueden", "quieres", "profe", "contraatacara", "san", "cuidado", "cara", "haber", "momento", "largate", "dan", "sola", "28votos", 
                    "acusacionacastilloya", "hablar", "parece", "menos", "voz", "siguen", "buscaran", "martin", "nuevo", "mujer", "gano", "juntando", "hacen", "objetivo", "nunca", "elcongresosequeda", "poder",
                    "pais", "millones", "peruanos", "p", "tener", "mismo", "pues", "vivalasmujeres", "ministro", "popular", "libre",
                    "vaya", "prosor", "basta", "dar", "cree", "nadie", "marzo", "sino", "seguir", "vacar", "falta", "sabado",
                    "vas", "sigue", "congreso87votoya", "partido", "ganar", "lima", "caer", "habla", "peruana", "peruano", "panorama",
                    "bueno", "tan", "verdad", "cerron", "mocion")

impeachment$clean_text <- removeWords(impeachment$clean_text, words=c(keywords,stopwords,removable_words))

post_impeachment$clean_text <- removeWords(post_impeachment$clean_text, words=c(keywords,stopwords,removable_words))

# Spaces, newlines, emojis, and tabs were removed. 
# This was the final cleaning step to remove the spaces or gaps left by the removal of other characters and words.
impeachment$clean_text <- gsub("\n", " ", impeachment$clean_text)
impeachment$clean_text <- gsub("^\\s+", "", impeachment$clean_text)
impeachment$clean_text <- gsub("\\s+$", "", impeachment$clean_text)
impeachment$clean_text <- gsub("[ |\t]+", " ", impeachment$clean_text)

post_impeachment$clean_text <- gsub("\n", " ", post_impeachment$clean_text)
post_impeachment$clean_text <- gsub("^\\s+", "", post_impeachment$clean_text)
post_impeachment$clean_text <- gsub("\\s+$", "", post_impeachment$clean_text)
post_impeachment$clean_text <- gsub("[ |\t]+", " ", post_impeachment$clean_text)

# Although stemming is a usual procedure when pre-processing data for text analysis, this did not work well for this case. 
# One problem, was that the stemming was not uniform. That is, some words were not trimmed despite sharing the root with others which were cut.
# The other problem is that tweets use considerable amount of slang, abbreviations, and local expressions.
# Because of this, stemming was not used in this analysis. 

## The dataset was further cleaned by dropping NAs and tweets that had no other content than the keywords used to scrape or irrelevant words.
impeachment <- impeachment[!(is.na(impeachment$clean_text) | impeachment$clean_text==""), ]

post_impeachment <- post_impeachment[!(is.na(post_impeachment$clean_text) | post_impeachment$clean_text==""), ]

ggplot(impeachment, aes(date)) + 
  geom_bar(na.rm=TRUE) +
  ylim(0, 6000) + 
  scale_x_date(date_label="%e/%m", breaks = "1 day", expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Number of Tweets per Day After Pre-Processing ('Impeachment' Dataset)")


ggplot(post_impeachment, aes(date)) + 
  geom_bar(na.rm=TRUE) +
  ylim(0, 6000) + 
  scale_x_date(date_label="%e/%m", breaks = "1 day", expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Number of Tweets per Day After Pre-Processing ('Post-Impeachment' Dataset)")



### Text Analysis 1: Topic Modeling with LDA
## For the topic exploration, only the 'Pre-Impeachment' Dataset will be taken into account. 
## One reason for this are the time limitations (as the 'Post-Impeachment' idea was incorporated very late into the project).
## Another is that, to compare the change between the proportion of topics between the 'Impeachment' and 'Post-Impeachment' tweets,
## it makes more sense to keep the same topics and dictionary terms to make both periods comparable. 

## Before the LDA model, an exploration of frequent terms was done with the quanteda package.
# Creation of the quanteda corpus
clean_tweets <- as.vector(impeachment$clean_text)
quanteda_corpus<- corpus(clean_tweets)

# Checking the most frequent words. This was also used to update the removable word list.
DFM <- dfm(tokens_remove(tokens(quanteda_corpus), pattern = c(stopwords, keywords, removable_words)))
topfeatures(DFM, 100)

## The topic modeling was done mostly with the "tm" package. 
# A "tm" corpus was created, as the functions did not work with the quanteda one. 
corpus_tweets <- VCorpus(VectorSource(clean_tweets))

# Visualizing frequent words with a wordcloud. 
pal <- brewer.pal(8, "Dark2")
wordcloud(corpus_tweets, min.freq=2, max.words = 150, random.order = TRUE, col = pal)

# Next, a document term matrix was created for the LDA. 
DTM <- DocumentTermMatrix(corpus_tweets)
# This and the next step were needed to avoid errors. 
raw.sum <- apply(DTM, 1, FUN = sum) 
DTM <- DTM[raw.sum != 0,]

# The LDA model was used for the topic modeling.
# k=3 was chosen because of the expectations of finding "communism", "corruption", and "incompetence" as main topics. 
# These expectations were drawn from my familiarity with the Peruvian context and debates. 
# Another reason for a small k is that it is more practical for doing pair-wise comparisons to identify discriminant words.
impeachment_LDA <- LDA(DTM, k=3, control = list(seed = 1111))

# Calculation of the betas to check the top 10 terms for each topic.
impeachment_topics <- tidy(impeachment_LDA, matrix = "beta")

impeachment_top_terms <- impeachment_topics%>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

impeachment_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Then, the most discriminant words for each topic were also calculated. 
# Different beta scores were tried as filters to see whether this contributed to making the topics more clear. 
# The beta level provided as an example in class (.001) gave words that were too common to be insightful.
# Therefore, a higher beta score was set to look for less common words.
# Moreover, some common words found with the lower filters were added to the removable words list.

# Most discriminant words between T1 and T2
beta_spread <- impeachment_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .0025 | topic2 > .0025) %>%
  mutate(log_ratio = log2(topic1/topic2))

beta_spread %>%
  mutate(absratio = abs(log_ratio)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, absratio) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()

# Most discriminant words between T1 and T3
beta_spread <- impeachment_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .0025| topic3 > .0025) %>%
  mutate(log_ratio = log2(topic1/topic3))

beta_spread %>%
  mutate(absratio = abs(log_ratio)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, absratio) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 3 / topic 1") +
  coord_flip()

# Most discriminant words between T2 and T3
beta_spread <- impeachment_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic2 > .0025 | topic3 > .0025) %>%
  mutate(log_ratio = log2(topic2/topic3))

beta_spread %>%
  mutate(absratio = abs(log_ratio)) %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, absratio) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 3 / topic 2") +
  coord_flip()


### Text Analysis 2: Dictionary-Approach for Topic Time Series Analysis
## Based on the exploration done through the LDA topic modeling, topic dictionaries were manually created. 
# The dictionaries do not correspond directly to the LDA topics, but build on those findings. 
# The exploration fo the most frequent words was also useful for building the dictionaries.
# The dictionaries were further expanded by reading some tweets. 

# Three expected topics based on my familiarity with the Peruvian context.
topic1_corruption <- c("delincuente", "mafia", "corrupcion", "corrupto", "corruptos",
                       "fiscalescomplices", "zoraidaavalosfiscaldelacorrupcion", "fueracastillopordelincuente",
                       "embarrado", "delincuentes", "castillocorrupto", "quesevayantodos", "karelim", "criminal", 
                       "banda", "ladron", "dinamicos", "carcel", "justicia", "repartija", "roba", "robar",
                       "choro", "corrupta", "sinverguenza", "sinverguenzas", "lagartos", "lagartijas", "robar", "fueracorrupto")
topic2_incompetence <- c("incapaz", "burro", "asno", "ignorante", "analfabeto", "inutil", "botenalburro", "bruto", "inepto", "desgobierno",
                         "incapacidad", "discapacitado", "ingobernabilidad", "mediocre", "incapaces", "estupido", "incapacidad",
                         "moral", "improvisado")
topic3_communism <- c("vacanciaasendero", "terrorismonuncamas", "caviares", "terroristas", "comunistas",
                      "comunismo", "noalcomunismo", "comunismoesmiseria", "terrorismo", "sendero", "sendero luminoso",
                      "sl", "fueracaviaresparasitos", "izquierda", "venezuela", "cuba", "comunismonuncamas", "terruco", "castro", "chavistas",
                      "terrorista", "izquierdistas", "fueracomunistas", "fueraterrucos", "socialismo", "rojetes", "zurdos", "izquierdamiserable",
                      "rojo", "rojos", "cojudignos", "yonomarchoconcaviares", "cojudignos")

# Three unexpected topics included based on the findings from the LDA model
topic4_patriotism <- c("traidores", "patriotas", "traidor", "patria", "traicion", "defender", "futuro",
                       "hijos", "patriota", "valientes", "heroes", "vendepatria")
topic5_mobilization <- c("salgamostodos", "marchaapalacio", "palacio", "20mperu", "5mperu", "5marzo", "granmarcha", "marcha",
                         "protesta", "marchar", "tomemoslacalle", "calles", "calle", "plaza", "campo", "zapatillaslistas")
topic6_democracy <- c("democracia", "fraude", "totalitarios", "autoritarismo")

## The dictionaries were used to create a binary variable for each topic. 
## The process was done both for the 'Impeachment' and the 'Post-Impeachment' dataset.
## This, because one of the goals is to compare the daily topic proportion between those two periods.

# A separate variable was created to have the words split for the coding. 
# The word split was not done on the "clean_text" variable used before because the split words do not work properly for the LDA.
impeachment$split_text <- str_split(impeachment$clean_text, " ")
post_impeachment$split_text <- str_split(post_impeachment$clean_text, " ")

# A function was created to make the coding cleaner and faster. 
# Each tweet was coded "1" if it had any word contained in the respective dictionary, and "0" otherwise.
topic_encoding <- function(df, topic, variable){
  df[[variable]] <- 0
  for (i in 1:nrow(df)) {
    for (j in 1:lengths(df$split_text[i])) {
      if (df$split_text[[i]][j] %in% topic){
        df[[variable]][i] <- 1} 
    }
  }
  return(df)
}

# The function was applied to the five topics, creating a new variables in the dataframe for each. 
impeachment <- topic_encoding(impeachment, topic1_corruption, "T1_Corruption")
impeachment <- topic_encoding(impeachment, topic2_incompetence, "T2_Incompetence")
impeachment <- topic_encoding(impeachment, topic3_communism, "T3_Communism")
impeachment <- topic_encoding(impeachment, topic4_patriotism, "T4_Patriotism")
impeachment <- topic_encoding(impeachment, topic5_mobilization, "T5_Mobilization")
impeachment <- topic_encoding(impeachment, topic6_democracy, "T6_Democracy")

post_impeachment <- topic_encoding(post_impeachment, topic1_corruption, "T1_Corruption")
post_impeachment <- topic_encoding(post_impeachment, topic2_incompetence, "T2_Incompetence")
post_impeachment <- topic_encoding(post_impeachment, topic3_communism, "T3_Communism")
post_impeachment <- topic_encoding(post_impeachment, topic4_patriotism, "T4_Patriotism")
post_impeachment <- topic_encoding(post_impeachment, topic5_mobilization, "T5_Mobilization")
post_impeachment <- topic_encoding(post_impeachment, topic6_democracy, "T6_Democracy")


## With these variables, the proportion of each topic per day can be calculated. 
# First, the amount of tweets per day was calculated. 
date_count_pre <- impeachment %>% count(date)

date_count_post <- post_impeachment %>% count(date)

# Next, a new dataframe to store the proportion was created. 
topic_proportion_pre <- data.frame(as.Date(date_count_pre$date))
names(topic_proportion_pre)[names(topic_proportion_pre) == "as.Date.date_count_pre.date."] <- "Date"

topic_proportion_post <- data.frame(as.Date(date_count_post$date))
names(topic_proportion_post)[names(topic_proportion_post) == "as.Date.date_count_post.date."] <- "Date"

# Finally, the proportion of each topic was calculated for each day.
# Given that some tweets cover more than one topic, each topic was calculated separately.
# For this, a function was created. 
calculate_proportion <- function(df, variable, date){
                        prop <- aggregate(df[[variable]], list(df$date), sum)
                        return(prop$x/date$n*100)
                        }

# The function was used to calculate the daily proportion of each topic and add them to the dataframe.
topic_proportion_pre$T1_Corruption <- calculate_proportion(impeachment, "T1_Corruption", date_count_pre)
topic_proportion_pre$T2_Incompetence <- calculate_proportion(impeachment, "T2_Incompetence", date_count_pre)
topic_proportion_pre$T3_Communism <- calculate_proportion(impeachment, "T3_Communism", date_count_pre)
topic_proportion_pre$T4_Patriotism <- calculate_proportion(impeachment, "T4_Patriotism", date_count_pre)
topic_proportion_pre$T5_Mobilization <- calculate_proportion(impeachment, "T5_Mobilization", date_count_pre)
topic_proportion_pre$T6_Democracy <- calculate_proportion(impeachment, "T6_Democracy", date_count_pre)

topic_proportion_post$T1_Corruption <- calculate_proportion(post_impeachment, "T1_Corruption", date_count_post)
topic_proportion_post$T2_Incompetence <- calculate_proportion(post_impeachment, "T2_Incompetence", date_count_post)
topic_proportion_post$T3_Communism <- calculate_proportion(post_impeachment, "T3_Communism", date_count_post)
topic_proportion_post$T4_Patriotism <- calculate_proportion(post_impeachment, "T4_Patriotism", date_count_post)
topic_proportion_post$T5_Mobilization <- calculate_proportion(post_impeachment, "T5_Mobilization", date_count_post)
topic_proportion_post$T6_Democracy <- calculate_proportion(post_impeachment, "T6_Democracy", date_count_post)

## Given that they were calculated separately (that is, the five topics not add up to 100%), they are plotted separately. 
## For easier comparison, the 'Impeachment' proportions time series and the 'Post-Impeachment' proportions time series are plotted next to each other. 
## The blue dashed line signals the day of the impeachment debate (March 28th)

# Topic 1 -- Corruption 
# The red dashed line in the plot on the right signals the announcement of an investigation of a corruption scandal that involves Castillo.
# This investigation will be done by the National Comptroller.
corruption_pre <- ggplot(topic_proportion_pre, aes(Date, T1_Corruption)) + 
                        geom_point() +
                        geom_line() +
                        ylim(0, 70) + 
                        ylab("% of tweets about corruption") +
                        xlab("date") + 
                        ggtitle("Daily % of Tweets Refering to Corruption (Topic 1)") +
                        scale_x_date(date_label="%e/%m", date_breaks = "2 days") +
                        theme_minimal() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                        geom_vline(xintercept=as.Date("2022-02-27"), linetype="dashed", color = "red") 

corruption_post <- ggplot(topic_proportion_post, aes(Date, T1_Corruption)) + 
                          geom_point() +
                          geom_line() +
                          ylim(0, 70) + 
                          ylab("% of tweets about corruption") +
                          xlab("date") + 
                          scale_x_date(date_label="%e/%m", date_breaks = "1 day") +
                          theme_minimal() +
                          ggtitle(" ") +
                          geom_vline(xintercept=as.Date("2022-03-28"), linetype="dashed", color = "blue") +
                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

plot_grid(corruption_pre, corruption_post)

# Topic 2 -- Incompetence
incompetence_pre <- ggplot(topic_proportion_pre, aes(Date, T2_Incompetence)) + 
                          geom_point() +
                          geom_line() +
                          ylim(0, 70) + 
                          ylab("% of tweets about incompetence") +
                          ggtitle("Daily % of Tweets Refering to Incompetence (Topic 2)") +
                          scale_x_date(date_label="%e/%m", date_breaks = "2 days") +
                          theme_minimal() +
                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

incompetence_post <- ggplot(topic_proportion_post, aes(Date, T2_Incompetence)) + 
                          geom_point() +
                          geom_line() +
                          ylim(0, 70) + 
                          ylab("% of tweets about incompetence") +
                          scale_x_date(date_label="%e/%m", date_breaks = "1 day") +
                          theme_minimal() +
                          ggtitle(" ") +
                          geom_vline(xintercept=as.Date("2022-03-28"), linetype="dashed", color = "blue") +
                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

plot_grid(incompetence_pre, incompetence_post)

# Topic 3 -- Communism
communism_pre <- ggplot(topic_proportion_pre, aes(Date, T3_Communism)) + 
                        geom_point() +
                        geom_line() +
                        ylim(0, 70) + 
                        ylab("% of tweets about communism") +
                        ggtitle("Daily % of Tweets Refering to Communism (Topic 3)")
                        scale_x_date(date_label="%e/%m", date_breaks = "2 days") +
                        theme_minimal() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

communism_post <- ggplot(topic_proportion_post, aes(Date, T3_Communism)) + 
                        geom_point() +
                        geom_line() +
                        ylim(0, 70) + 
                        ylab("% of tweets about communism") +
                        scale_x_date(date_label="%e/%m", date_breaks = "1 day") +
                        theme_minimal() +
                        ggtitle(" ") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                        geom_vline(xintercept=as.Date("2022-03-28"), linetype="dashed", color = "blue")

plot_grid(communism_pre, communism_post)

# Topic 4 -- Patriotism
patriotism_pre <- ggplot(topic_proportion_pre, aes(Date, T4_Patriotism)) + 
                          geom_point() +
                          geom_line() +
                          ylim(0, 70) + 
                          ylab("% of tweets about patriotism") +
                          ggtitle("Daily % of Tweets Refering to Patriotism (Topic 4)") +
                          scale_x_date(date_label="%e/%m", date_breaks = "2 days") +
                          theme_minimal() +
                          theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

patriotism_post <- ggplot(topic_proportion_post, aes(Date, T4_Patriotism)) + 
                          geom_point() +
                          geom_line() +
                          ylim(0, 70) + 
                          ylab("% of tweets about patriotism") +
                          scale_x_date(date_label="%e/%m", date_breaks = "1 day") +
                          theme_minimal() +
                          ggtitle(" ") +
                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                          geom_vline(xintercept=as.Date("2022-03-28"), linetype="dashed", color = "blue")

plot_grid(patriotism_pre, patriotism_post)

# Topic 5 -- Mobilization
# The red dashed line in the plot on the right signals a big protest in favor of the impeachment (March 5th)
mobilization_pre <- ggplot(topic_proportion_pre, aes(Date, T5_Mobilization)) + 
                          geom_point() +
                          geom_line() +
                          ylim(0, 70) + 
                          ylab("% of tweets about mobilization") +
                          ggtitle("Daily % of Tweets Refering to Mobilization (Topic 5)") +
                          scale_x_date(date_label="%e/%m", date_breaks = "2 days") +
                          theme_minimal() +
                          geom_vline(xintercept=as.Date("2022-03-05"), linetype="dashed", color = "red") +
                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

mobilization_post <- ggplot(topic_proportion_post, aes(Date, T5_Mobilization)) + 
                          geom_point() +
                          geom_line() +
                          ylim(0, 70) + 
                          ylab("% of tweets about mobilization") +
                          ggtitle(" ") +
                          scale_x_date(date_label="%e/%m", date_breaks = "1 day") +
                          theme_minimal() +
                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                          geom_vline(xintercept=as.Date("2022-03-28"), linetype="dashed", color = "blue")

plot_grid(mobilization_pre, mobilization_post)

# Topic 6 -- Democracy
democracy_pre <- ggplot(topic_proportion_pre, aes(Date, T6_Democracy)) + 
                        geom_point() +
                        geom_line() +
                        ylim(0, 70) + 
                        ylab("% of tweets about democracy") +
                        ggtitle("Daily % of Tweets Refering to Democracy (Topic 6)") +
                        scale_x_date(date_label="%e/%m", date_breaks = "2 days") +
                        theme_minimal() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

democracy_post <- ggplot(topic_proportion_post, aes(Date, T6_Democracy)) + 
                        geom_point() +
                        geom_line() +
                        ylim(0, 70) + 
                        ylab("% of tweets about democracy") +
                        ggtitle(" ") +
                        scale_x_date(date_label="%e/%m", date_breaks = "1 day") +
                        theme_minimal() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                        geom_vline(xintercept=as.Date("2022-03-28"), linetype="dashed", color = "blue")

plot_grid(democracy_pre, democracy_post)

# All the topics
# This is messy given the many topics, I am including it just in case anyone wants to visualize all the topics.
# The comparison of the pre vs. post impeachment topics is better seen in the next barplot. 
all_topics_pre <- ggplot() + 
                  geom_line(data = topic_proportion_pre, aes(x = Date, y = T1_Corruption, color = "Corruption")) +
                  geom_point(data = topic_proportion_pre, aes(x = Date, y = T1_Corruption, color = "Corruption")) +
                  geom_line(data = topic_proportion_pre, aes(x = Date, y = T2_Incompetence, color = "Incompetence")) +
                  geom_point(data = topic_proportion_pre, aes(x = Date, y = T2_Incompetence, color = "Incompetence")) +
                  geom_line(data = topic_proportion_pre, aes(x = Date, y = T3_Communism, color = "Communism")) +
                  geom_point(data = topic_proportion_pre, aes(x = Date, y = T3_Communism, color = "Communism")) +
                  geom_line(data = topic_proportion_pre, aes(x = Date, y = T4_Patriotism, color = "Patriotism")) +
                  geom_point(data = topic_proportion_pre, aes(x = Date, y = T4_Patriotism, color = "Patriotism")) +
                  geom_line(data = topic_proportion_pre, aes(x = Date, y = T5_Mobilization, color = "Mobilization")) +
                  geom_point(data = topic_proportion_pre, aes(x = Date, y = T5_Mobilization, color = "Mobilization")) +
                  geom_line(data = topic_proportion_pre, aes(x = Date, y = T6_Democracy, color = "Democracy")) +
                  geom_point(data = topic_proportion_pre, aes(x = Date, y = T6_Democracy, color = "Democracy")) +
                  xlab('% of Topic Presence') +
                  ylab('Date') +
                  ggtitle("Daily % of Each Topic") +
                  labs(color="Topics") +
                  scale_x_date(date_label="%e/%m", date_breaks = "2 days") +
                  theme_minimal() +
                  ylim(0, 70) + 
                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

all_topics_post <- ggplot() + 
                  geom_line(data = topic_proportion_post, aes(x = Date, y = T1_Corruption, color = "Corruption")) +
                  geom_point(data = topic_proportion_post, aes(x = Date, y = T1_Corruption, color = "Corruption")) +
                  geom_line(data = topic_proportion_post, aes(x = Date, y = T2_Incompetence, color = "Incompetence")) +
                  geom_point(data = topic_proportion_post, aes(x = Date, y = T2_Incompetence, color = "Incompetence")) +
                  geom_line(data = topic_proportion_post, aes(x = Date, y = T3_Communism, color = "Communism")) +
                  geom_point(data = topic_proportion_post, aes(x = Date, y = T3_Communism, color = "Communism")) +
                  geom_line(data = topic_proportion_post, aes(x = Date, y = T4_Patriotism, color = "Patriotism")) +
                  geom_point(data = topic_proportion_post, aes(x = Date, y = T4_Patriotism, color = "Patriotism")) +
                  geom_line(data = topic_proportion_post, aes(x = Date, y = T5_Mobilization, color = "Mobilization")) +
                  geom_point(data = topic_proportion_post, aes(x = Date, y = T5_Mobilization, color = "Mobilization")) +
                  geom_line(data = topic_proportion_post, aes(x = Date, y = T6_Democracy, color = "Democracy")) +
                  geom_point(data = topic_proportion_post, aes(x = Date, y = T6_Democracy, color = "Democracy")) +
                  xlab('% of Topic Presence') +
                  ylab('Date') +
                  labs(color="Topics") +
                  ggtitle(" ") +
                  scale_x_date(date_label="%e/%m", date_breaks = "1 day") +
                  theme_minimal() +
                  ylim(0,70) + 
                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  geom_vline(xintercept=as.Date("2022-03-28"), linetype="dashed", color = "blue")
  
plot_grid(all_topics_pre, all_topics_post)

# Comparison of average topic presence ('Impeachment' vs. 'Post Impeachment')
# Calculating average daily percentage of each topic
prop_pre <- topic_proportion_pre[-1]
avg_prop_pre <- colMeans(prop_pre)

prop_post <- topic_proportion_post[-1]
avg_prop_post <- colMeans(prop_post)

# Formatting to be able to plot
avg_prop_comp <- data.frame(avg_prop_pre)
avg_prop_comp <- cbind(newColName <- rownames(avg_prop_comp), avg_prop_comp)
rownames(avg_prop_comp) <- 1:nrow(avg_prop_comp)
colnames(avg_prop_comp) <- c("Topic", "Average_Prop")
avg_prop_comp$Period <- c("Before the impeachment\n(February, 15th to March, 15th)", "Before the impeachment\n(February, 15th to March, 15th)", "Before the impeachment\n(February, 15th to March, 15th)", 
                          "Before the impeachment\n(February, 15th to March, 15th)", "Before the impeachment\n(February, 15th to March, 15th)", "Before the impeachment\n(February, 15th to March, 15th)")

avg_prop_comp2 <- data.frame(avg_prop_post)
avg_prop_comp2 <- cbind(newColName <- rownames(avg_prop_comp2), avg_prop_comp2)
rownames(avg_prop_comp2) <- 1:nrow(avg_prop_comp2)
colnames(avg_prop_comp2) <- c("Topic", "Average_Prop")
avg_prop_comp2$Period <- c("Around the impeachment\n(March, 27th - 30th)", "Around the impeachment\n(March, 27th - 30th)", "Around the impeachment\n(March, 27th - 30th)",
                           "Around the impeachment\n(March, 27th - 30th)", "Around the impeachment\n(March, 27th - 30th)", "Around the impeachment\n(March, 27th - 30th)")

avg_prop_bind <- rbind(avg_prop_comp2, avg_prop_comp)
avg_prop_bind$Period <- factor(avg_prop_bind$period, levels = c("Before the impeachment\n(February, 15th to March, 15th)", "Around the impeachment\n(March, 27th - 30th)"))

# Grouped Barplot
ggplot(avg_prop_bind, aes(fill=Period, y=Average_Prop, x=Topic)) + 
  geom_bar(position="dodge", stat="identity") +
  xlab("Topics") +
  ylab("Average Daily Topic %") + 
  ggtitle("Comparison of the Average Daily Topic % Between Periods") +
  scale_x_discrete(labels=c("Corruption", "Incompetence", "Communism", "Patriotism", "Mobilization", "Democracy")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
