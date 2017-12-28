
# -------------------------------
# Analysis of Sermons
# -------------------------------

# Load Packages
# -------------
library(tidytext)
library(dplyr)
library(tm)
library(ggplot2)
library(syuzhet)
library(gridExtra)

# Get Data
# --------
sermons <- read.csv("C:/Users/Miles/Documents/R/Sermon Analysis/Sermons.csv")
sermons <- sermons[,-c(6,7)]

# Clean the Data
# --------------

sermons$Sermon <- as.character(sermons$Sermon)
sermons$Sermon_clean <- tolower(sermons$Sermon) #make it lower case
sermons$Sermon_clean <- gsub('[[:punct:]]', '', sermons$Sermon_clean) #remove punctuation
sermons$Sermon_clean <- gsub('[[:digit:]]+', '', sermons$Sermon_clean) #remove numbers
sermons$Sermon_clean <- Corpus(VectorSource(sermons$Sermon_clean))
sermons$Sermon_clean <- tm_map(sermons$Sermon_clean, removeWords, stopwords('english')) #remove stopwords
sermons$Sermon_clean <- lapply(sermons$Sermon_clean[1:nrow(sermons)], as.character)
sermons$Sermon_clean <- unlist(sermons$Sermon_clean)

# tf-idf Analysis
# Men v. Women
# ---------------
words <- sermons %>% unnest_tokens(word, Sermon) %>%
  count(Gender, word, sort = TRUE) %>%
  ungroup() %>% bind_tf_idf(word, Gender, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(tf_idf = tf_idf)

# Visualize Top td-idf terms
# --------------------------
windows(14,7)
words %>% group_by(Gender) %>%
  top_n(20) %>% 
  ggplot(aes(x=reorder(word,tf_idf),y=tf_idf)) + geom_col() +
  coord_flip() + 
  xlab("") + ylab("Term Frequency-Inverse Document Frequency") + theme_bw() + 
  theme(text=element_text(family="serif"),
        axis.text=element_text(color="black")) +
  facet_wrap("Gender",ncol=2,scales="free")
ggsave("plot1.pdf",width=14,height=9)

# Sentiment Analysis
# Men v. Women
# ------------------
sentiment <- get_nrc_sentiment(as.character(words$word))
w1 <- cbind(words,sentiment) %>% summarySE(measurevar="positive",groupvars="Gender") %>%
  mutate(emotion=positive) %>% select(emotion,ci,Gender) %>%
  mutate(sentiment=c("Positive"))
w2 <- cbind(words,sentiment) %>% summarySE(measurevar="negative",groupvars="Gender")%>%
  mutate(emotion=negative) %>% select(emotion,ci,Gender) %>%
  mutate(sentiment="Negative")
w3 <- cbind(words,sentiment) %>% summarySE(measurevar="trust",groupvars="Gender")%>%
  mutate(emotion=trust) %>% select(emotion,ci,Gender)%>%
  mutate(sentiment="Trust")
w4 <- cbind(words,sentiment) %>% summarySE(measurevar="surprise",groupvars="Gender")%>%
  mutate(emotion=surprise) %>% select(emotion,ci,Gender)%>%
  mutate(sentiment="Surprise")
w5 <- cbind(words,sentiment) %>% summarySE(measurevar="sadness",groupvars="Gender")%>%
  mutate(emotion=sadness) %>% select(emotion,ci,Gender)%>%
  mutate(sentiment="Sadness")
w6 <- cbind(words,sentiment) %>% summarySE(measurevar="joy",groupvars="Gender")%>%
  mutate(emotion=joy) %>% select(emotion,ci,Gender)%>%
  mutate(sentiment="Joy")
w7 <- cbind(words,sentiment) %>% summarySE(measurevar="fear",groupvars="Gender")%>%
  mutate(emotion=fear) %>% select(emotion,ci,Gender)%>%
  mutate(sentiment="Fear")
w8 <- cbind(words,sentiment) %>% summarySE(measurevar="disgust",groupvars="Gender")%>%
  mutate(emotion=disgust) %>% select(emotion,ci,Gender)%>%
  mutate(sentiment="Disgust")
w9 <- cbind(words,sentiment) %>% summarySE(measurevar="anticipation",groupvars="Gender")%>%
  mutate(emotion=anticipation) %>% select(emotion,ci,Gender)%>%
  mutate(sentiment="Anticipation")
w10 <- cbind(words,sentiment) %>% summarySE(measurevar="anger",groupvars="Gender")%>%
  mutate(emotion=anger) %>% select(emotion,ci,Gender)%>%
  mutate(sentiment="Anger")
windows(14,9)
rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10) %>%
  mutate(sentiment=factor(sentiment,
                             levels=c("Positive",
                                      "Negative",
                                      "Anger",
                                      "Anticipation",
                                      "Disgust",
                                      "Fear",
                                      "Joy",
                                      "Sadness",
                                      "Surprise",
                                      "Trust"))) %>%
  ggplot(aes(y=emotion,x=Gender,color=Gender)) + geom_point() +
  geom_errorbar(aes(ymin=emotion-ci,ymax=emotion+ci),
                width=.35) +
  theme_bw() + xlab("") + ylab("Mean Sentiment") +
  theme(text=element_text(family="serif"),
        axis.text=element_text(color="black"),
        legend.position = "none") +
  coord_flip() +
  facet_wrap("sentiment",scales="free")
ggsave("plot2.pdf",width=14,height=9)

# Sentiment Arc
# -------------
arc <- sermons %>% 
  mutate(X=1:nrow(sermons)) %>%
  unnest_tokens(word, Sermon, token = "sentences") %>% 
  select(c(word,Name,Gender,X)) %>% mutate(Name = as.character(Name)) %>% 
  mutate(Gender = as.character(Gender))

Cronology <- function(data){
  data$cronology <- NA
  data$cronology[1:(nrow(data)*.2)] <- "(1)\n
  Beginning"
  data$cronology[-(1:(nrow(data)*.2))] <- "(2)\n
  ..."
  data$cronology[-(1:(nrow(data)*.4))] <- "(3)\n
  Middle"
  data$cronology[-(1:(nrow(data)*.6))] <- "(4)\n
  ..."
  data$cronology[-(1:(nrow(data)*.8))] <- "(5)\n
  End"
  return(data)
}
result <- list()
for(i in 1:max(arc$X)){
  result[[i]] <- arc %>% filter(X==i) %>% Cronology() %>% 
    select(cronology)
}

sent <- get_nrc_sentiment(as.character(arc$word))
cronology <- unlist(result)
a1 <- cbind(arc,sent,cronology) %>% summarySE(measurevar="positive",groupvars=c("Gender","cronology")) %>%
  mutate(emotion=positive) %>% select(-positive) %>%
  mutate(sentiment=c("Positive"))
a2 <- cbind(arc,sent,cronology) %>% summarySE(measurevar="negative",groupvars=c("Gender","cronology")) %>%
  mutate(emotion=negative) %>% select(-negative) %>%
  mutate(sentiment="Negative")
a3 <- cbind(arc,sent,cronology) %>% summarySE(measurevar="trust",groupvars=c("Gender","cronology")) %>%
  mutate(emotion=trust) %>% select(-trust)%>%
  mutate(sentiment="Trust")
a4 <- cbind(arc,sent,cronology) %>% summarySE(measurevar="surprise",groupvars=c("Gender","cronology")) %>%
  mutate(emotion=surprise) %>% select(-surprise)%>%
  mutate(sentiment="Surprise")
a5 <- cbind(arc,sent,cronology) %>% summarySE(measurevar="sadness",groupvars=c("Gender","cronology")) %>%
  mutate(emotion=sadness) %>% select(-sadness)%>%
  mutate(sentiment="Sadness")
a6 <- cbind(arc,sent,cronology) %>% summarySE(measurevar="joy",groupvars=c("Gender","cronology")) %>%
  mutate(emotion=joy) %>% select(-joy)%>%
  mutate(sentiment="Joy")
a7 <- cbind(arc,sent,cronology) %>% summarySE(measurevar="fear",groupvars=c("Gender","cronology")) %>%
  mutate(emotion=fear) %>% select(-fear)%>%
  mutate(sentiment="Fear")
a8 <- cbind(arc,sent,cronology) %>% summarySE(measurevar="disgust",groupvars=c("Gender","cronology")) %>%
  mutate(emotion=disgust) %>% select(-disgust)%>%
  mutate(sentiment="Disgust")
a9 <- cbind(arc,sent,cronology) %>% summarySE(measurevar="anticipation",groupvars=c("Gender","cronology")) %>%
  mutate(emotion=anticipation) %>% select(-anticipation)%>%
  mutate(sentiment="Anticipation")
a10 <- cbind(arc,sent,cronology) %>% summarySE(measurevar="anger",groupvars=c("Gender","cronology")) %>%
  mutate(emotion=anger) %>% select(-anger)%>%
  mutate(sentiment="Anger")
windows(14,9)
rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) %>%
  mutate(sentiment=factor(sentiment,
                          levels=c("Positive",
                                   "Negative",
                                   "Anger",
                                   "Anticipation",
                                   "Disgust",
                                   "Fear",
                                   "Joy",
                                   "Sadness",
                                   "Surprise",
                                   "Trust"))) %>%
  ggplot(aes(y=emotion,x=cronology,color=Gender,group=Gender)) + 
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin=emotion-ci,ymax=emotion+ci),
                width=.35) +
  theme_bw() + xlab("") + ylab("Mean Sentiment") +
  theme(text=element_text(family="serif"),
        axis.text=element_text(color="black"),
        legend.position = c(.6,.15)) +
  facet_wrap("sentiment",scales="free")
ggsave("plot3.pdf",width=14,height=9)

# 1st Person Singular Pronouns
# ----------------------------
p1 <- words %>% filter(word == "i" | word == "me") %>%
  mutate(Person = "1st, Singular")
p2 <- words %>% filter(word == "we" | word == "us") %>% 
  mutate(Person = "1st, Plural")
p3 <- words %>% filter(word == "my" | word == "mine") %>%
  mutate(Person = "1st, Singular Possessive")
p4 <- words %>% filter(word == "our" | word == "ours") %>%
  mutate(Person = "1st, Plural Possessive")
windows()
rbind(p1,p2,p3,p4) %>% 
  ggplot(aes(x=word,y=tf,fill=Gender)) + 
  geom_col(position="dodge") + 
  xlab("") + ylab("Term Frequency") +
  theme_bw() +
  theme(text=element_text(family="serif"),
        axis.text=element_text(color="black"))
ggsave("plot4.pdf",width=7,height=7)

# Tentative Speech
# ----------------
windows()
words %>% filter(word == "maybe" | word == "probably" | word == "might" | word == "usually" | 
                   word == "may" | word == "possibly" | word == "sometimes") %>%
  ggplot(aes(x=word,y=tf,fill=Gender)) +
  geom_col(position="dodge") +
  xlab("") + ylab("Term Frequency") +
  theme_bw() +
  theme(text=element_text(family="serif"),
        axis.text=element_text(color="black"))
ggsave("plot5.pdf",width=7,height=7)

# Political Speech
# ----------------
windows()
words %>% filter(word == "politics" | word == "vote" | word == "lgbt" | word == "gay" |
                   word == "homosexual" | word == "homosexuality" | word == "terrorism" |
                   word == "president" | word == "america" | word == "usa" | 
                   word == "lesbian" | word == "abortion" | word == "clinton" |
                   word == "trump" | word == "government" | word == "terrorist") %>% 
  ggplot(aes(x=word,y=tf,fill=Gender)) +
  geom_col(position="dodge") +
  xlab("") + ylab("Term Frequency") +
  theme_bw() +
  theme(text=element_text(family="serif"),
        axis.text=element_text(color="black"),
        axis.text.x=element_text(angle=45,hjust=1))
ggsave("plot6.pdf",width=9,height=9)
