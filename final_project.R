library(bitops)
library(RCurl)
library(rjson)
library(streamR)
library(RColorBrewer)
library(NLP)
library(tm)
library(ggplot2)
library(sp)
library(maps)
library(maptools)
library(rworldmap)
library(grid)
library(stringr)
library(plyr)
library(psych)
library(doBy)
library(gmodels)

# stopped loading libraries here. 
library(Rstem)
gpclibPermit()
library(rjson)


####################################################
######## Loading and creating subsets of data ######
####################################################
#load("tweets_all_sent_mapped_r.Rdata")
#tweetsUS <- tweets_all_sent_mapped

tweetsUS <- read.csv(file.choose())
tweetsUS$text <- sapply(tweetsUS$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
#t <- head(tweetsUS, 50)
t <- tweetsUS

t$HC <- 0
t$HC[grepl("Clinton | clinton | Hillary | hillary | Hillaryclinton | hillaryclinton | Hillary Clinton | hillary clinton | HillaryClinton | @HillaryClinton | HILLARY | CLINTON | imwithher", t$text)] <- 1
t$BS <- 0
t$BS[grepl("Berniesanders | berniesanders | Bernie Sanders  | bernie sanders | Bernie | bernie | @Sensanders | sensanders | @BernieSanders | BERNIE | SANDERS  | @Berniesanders | feelthebern", t$text)] <- 1
t$TC <- 0
t$TC[grepl("Cruz | cruz | Ted | ted | Tedcruz | tedcruz | Ted Cruz | ted cruz | @tedcruz | @SenTedCruz | TED | CRUZ" , t$text)] <- 1
t$DT <- 0
t$DT[grepl("Donaldtrump  | donaldtrump | Donald Trump | donald trump | Trump | trump | Donald | donald | Trumpf | trumpf |realDonaldTrump | @realDonaldTrump | DONALD | TRUMP | alwaystrump | nevertrump | makeamericagreatagain" , t$text)] <- 1
t$MR <- 0
t$MR[grepl("Marcorubio | marcorubio | Marco Rubio | marco rubio | @marcorubio | MARCO | RUBIO | Rubio" , t$text)] <- 1
t$Rep <- 0
t$Rep[grepl("Republican | republican | rep | Rep", t$text)] <- 1
t$Dem <- 0
t$Dem[grepl("Democrat | democrat | dem | Dem ", t$text)] <- 1

t$republican <- with(t, ifelse(t$Rep == 1 | t$DT == 1 | t$TC == 1 | t$MR == 1, 1, 0))
t$democrat <- with(t, ifelse(t$Dem == 1 | t$HC == 1 | t$BS == 1, 1, 0))

text_analysis <- data.frame(t$text, t$republican, t$democrat)
names(text_analysis) <- c("text", "republican", "democrat")
txt_anl <- unique(text_analysis)
txt_anl$text <- sapply(txt_anl$text, tolower)

View(txt_anl)

save(txt_anl, file = "t_a.Rdata")

####
# Loading data again:
load("t_a.Rdata")

td <- head(txt_anl, 100)

tc <- function(filename){
  filename$text <- sapply(filename$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  TweetCorpus<-paste(unlist(filename$text), collapse =" ")
  TweetCorpus <- Corpus(VectorSource(TweetCorpus))
  TweetCorpus <- tm_map(TweetCorpus, removePunctuation)
  TweetCorpus <- tm_map(TweetCorpus, removeWords, stopwords("english"))
  #TweetCorpus <- tm_map(TweetCorpus, stemDocument)
  TweetCorpus <- tm_map(TweetCorpus, content_transformer(tolower),lazy=TRUE)
  TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
  return(TweetCorpus)
}



#used to find top 5 curse words in corpus

revs <- tm_map(tc(txt_anl), content_transformer(tolower)) 
revs <- tm_map(revs, removeWords, stopwords("english")) 
revs <- tm_map(revs, removePunctuation) 
revs <- tm_map(revs, removeNumbers) 
revs <- tm_map(revs, stripWhitespace) 
dtm <- DocumentTermMatrix(revs)


findFreqTerms(dtm, 6000)



##############################################
########## Sentiment Analysis ################
##############################################

# profanity list: https://www.cs.cmu.edu/~biglou/resources/bad-words.txt

# key terms lexicon
lexicon <- read.csv("lexicon_ps.csv", stringsAsFactors=F)
econ.words <- lexicon$word[lexicon$polarity=="economy"]
imm.words <- lexicon$word[lexicon$polarity=="immigration"]
health.words <- lexicon$word[lexicon$polarity=="health_care"]
military.words <- lexicon$word[lexicon$polarity=="military"]
gun.words <- lexicon$word[lexicon$polarity=="gun_control"]
china.words <- lexicon$word[lexicon$polarity=="china"]
trade.words <- lexicon$word[lexicon$polarity=="trade"]
race.words <- lexicon$word[lexicon$polarity=="race"]
climate.words <- lexicon$word[lexicon$polarity=="climate_change"]
religion.words <- lexicon$word[lexicon$polarity=="religion"]
curse.words <- lexicon$word[lexicon$polarity=="curse"]
dem.words <- c("Democrat", "democrat" , "dem" , "Dem", "Berniesanders",
               "berniesanders",  "Bernie Sanders", "bernie sanders", "Bernie",
               "bernie",  "@Sensanders", "sensanders", "@BernieSanders", "BERNIE",
               "SANDERS",  "@Berniesanders", "feelthebern", "Clinton", "clinton",
              "Hillary",  "hillary",  "Hillaryclinton", "hillaryclinton", "Hillary Clinton",
              "hillary clinton",  "HillaryClinton", "@HillaryClinton", "HILLARY", "CLINTON",
              "imwithher")
rep.words <- c("Republican", "republican", "rep", "Rep", "Marcorubio", "marcorubio", "Marco Rubio", 
               "marco rubio", "@marcorubio", "MARCO", "RUBIO", "Rubio", "Donaldtrump",  "donaldtrump",
              "Donald Trump", "donald trump", "Trump",  "trump", "Donald", "donald", "Trumpf", "trumpf",
              "realDonaldTrump", "@realDonaldTrump", "DONALD", "TRUMP", "alwaystrump", "nevertrump",
              "makeamericagreatagain", "Cruz", "cruz", "Ted", "ted", "Tedcruz", "tedcruz",
              "Ted Cruz", "ted cruz",  "@tedcruz",  "@SenTedCruz",  "TED", "CRUZ")


cnvrt_df <- function(filename){
  filename$X <- NULL
  filename <-t(filename)
  filename <- data.frame(filename)
  names(filename)[1]<-paste("num")
  filename$term <- rownames(filename)
  filename$rate <- filename$num / sum(filename$num)
  return(filename)
}

tc_countT <- function(filename, fname){
  econ <- sum(str_count(tc(filename), econ.words))
  imm <- sum(str_count(tc(filename), imm.words))
  health <- sum(str_count(tc(filename), health.words))
  military <- sum(str_count(tc(filename), military.words))
  gun <- sum(str_count(tc(filename), gun.words))
  china <- sum(str_count(tc(filename), china.words))
  trade <- sum(str_count(tc(filename), trade.words))
  race <- sum(str_count(tc(filename), race.words))
  climate <- sum(str_count(tc(filename), climate.words))
  religion <- sum(str_count(tc(filename), religion.words))
  curse <- sum(str_count(tc(filename), curse.words))
  dem <- sum(str_count(tc(filename), dem.words))
  rep <- sum(str_count(tc(filename), rep.words))
  
  fn_df = data.frame(econ, 
                     imm, 
                     health, 
                     military, 
                     gun, 
                     china,
                     trade,
                     race, 
                     climate, 
                     religion,
                     curse, 
                     dem, 
                     rep)
  write.csv(fn_df, file = fname)
  return(cnvrt_df(fn_df))
}

####################################################
#### gets me a count for each group - then graphs it
####################################################

test_rates <- tc_countT(td, "test_rate")
all_rates <- tc_countT(txt_anl, "all_rate")


term_plots <- function(data, title, color){
  ggplot(data=data, aes(x=term, y=rate, fill=term)) +
    geom_bar(stat="identity", position=position_dodge())  +
    scale_fill_manual(values=color) +
    ggtitle(title)
}

cbPalette10 <- c('#9e0142', '#d53e4f', 
                 '#f46d43', '#fdae61',
                 '#fee08b', '#e6f598',
                 '#abdda4', '#66c2a5',
                 '#3288bd','#5e4fa2')

cbPalette11 <- c('#9e0142','#d53e4f',
                 '#f46d43','#fdae61',
                 '#fee08b','#ffffbf',
                 '#e6f598','#abdda4',
                 '#66c2a5','#3288bd',
                 '#5e4fa2')


term_plots(test_rates, "Rate of Topics", cbPalette11)
term_plots(all_rates, "Rate of Topics", cbPalette11)


####################################################
#### Create counts of each category and saves in df
####################################################

count_topic <- function(filename){
  filename$econ_c <- str_count(filename$text, paste(econ.words, collapse='|'))
  filename$imm_c <- str_count(filename$text, paste(imm.words, collapse='|'))
  filename$health_c <- str_count(filename$text, paste(health.words, collapse='|'))
  filename$military_c <- str_count(filename$text, paste(military.words, collapse='|'))
  filename$gun_c <- str_count(filename$text, paste(gun.words, collapse='|'))
  filename$china_c <- str_count(filename$text, paste(china.words, collapse='|'))
  filename$trade_c <- str_count(filename$text, paste(trade.words, collapse='|'))
  filename$climate_c <- str_count(filename$text, paste(climate.words, collapse='|'))
  filename$race_c <- str_count(filename$text, paste(race.words, collapse='|'))
  filename$religion_c <- str_count(filename$text, paste(religion.words, collapse='|'))
  filename$curse_c <- str_count(filename$text, paste(curse.words, collapse='|'))
  filename$dem_c <- str_count(filename$text, paste(dem.words, collapse = '|'))
  filename$rep_c <- str_count(filename$text, paste(rep.words, collapse = '|'))
  return(filename)
}

a <- count_topic(td)
total_counts <- count_topic(txt_anl)

complete_t <- total_counts[!!rowSums(abs(total_counts[-c(1:2)])),]


write.csv(complete_t, file = "complete_t.csv")

###### now to recreate code from classes




gss <- read.csv(file.choose())
table(gss$jbintfam)
summary(gss$jbintfam)
prop.table(table(gss$jbintfam)) ## this gives us the proportions for this variable

describe(gss$wrkstat)
ddply(gss, "jbintfam", summarise, Mean = mean(hrs1, na.rm = T), SD = sd(hrs1, na.rm = T))
summaryBy(hrs1~jbintfam, data=gss, FUN=c(mean, sd), na.rm=T)
boxplot(hrs1~jbintfam, data=gss)
p = ggplot(gss, aes(factor(jbintfam), hrs1))
p + geom_boxplot() + geom_jitter()
p + geom_boxplot(aes(fill = factor(jbintfam)))
CrossTable(gss$jbintfam, gss$wrkstat, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, format="SPSS")


summary(complete_t$dem_c)
summary(complete_t$rep_c)

lm1 = lm(curse_c ~ dem_c, data=complete_t) 
summary(lm1)

#plot(complete_t$rep_c, complete_t$curse_c, main=" Mentions by Curses", 
 #    xlab=" Mentions", ylab="Curses", pch=19)
#abline(lm(curse_c ~ rep_c, data=complete_t), col="red")


lm2 = lm(curse_c ~ dem_c + econ_c + rep_c, data=complete_t) 
summary(lm2)

lm3 = lm(curse_c ~ dem_c  + econ_c  + rep_c + imm_c + health_c + military_c + gun_c
         + china_c + trade_c + climate_c + race_c + religion_c, data=complete_t) 
summary(lm3)

anova(lm2, lm3)

lm4 = lm(curse_c ~  econ_c  + dem_c + rep_c + imm_c + health_c + military_c + gun_c
         + china_c + trade_c + climate_c + race_c + religion_c + dem_c*rep_c + dem_c*race_c, data=complete_t) 
summary(lm4)


lm5 = lm(curse_c ~  econ_c  + dem_c + rep_c + imm_c + health_c + military_c + gun_c
         + china_c + trade_c + climate_c + race_c + religion_c + dem_c*rep_c + dem_c*race_c
         + dem_c*econ_c + dem_c*imm_c + dem_c*health_c + dem_c*military_c + dem_c*gun_c
         + dem_c*china_c + dem_c*trade_c + dem_c*climate_c + dem_c*religion_c, data=complete_t) 
summary(lm5)


anova(lm4, lm5)

## error!
#complete_t$ln.curse_c <- log(complete_t$curse_c)
#complete_t$ln.dem_c <- log(complete_t$dem_c)

#lm6 = lm(ln.curse_c ~ ln.dem_c, data = complete_t, na.action=na.exclude)
#summarize(lm6)



logit.curse <- glm(curse_u ~ dem_c, data=complete_t, family=binomial)
summary(logit.curse)

logit.curse2 <- glm(curse_u ~ dem_c  + econ_c  + rep_c + imm_c + health_c + military_c + gun_c
                    + china_c + trade_c + climate_c + race_c + religion_c, data=complete_t, family=binomial)

summary(logit.curse2)
