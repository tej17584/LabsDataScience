###################################################
## Guatemala, julio del 2020
## Data Science 
## Alejandro Tejada 17584
## Diego Sevilla 17238
## Rodrigo Samayoa 17XXX
## Laboratorio 6: analisis de texto y redes sociales
## Catedr?tico: Lynette Garc?a
###################################################


install.packages("twitteR")
install.packages("RCurl")
install.packages("wordcloud")
install.packages("tm")
install.packages("DataExplorer")
install.packages("randomForest")
install.packages("ranger")
install.packages("Metrics")
install.packages("ROCit")
install.packages("kableExtra")

### LIBRERIAS PARA LEER LOS DATOS ----------------------------------------------------------------
library(twitteR)
library(RCurl)
library(wordcloud)
library(tm)

## ANALISIS
library(lattice)
library(DataExplorer)
library(grDevices)
library(factoextra)

library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

library(ranger)
library(Metrics)
library(ROCit)
library(kableExtra)
####-----------------------------------------------------------------------------------------------

consumerKey<-"K8x1yVtCpHvMnMzVhR46yuu19"
consumerSecret<-"dYhw54JKCEKEhewHWSGPvJ4YxRokT3Ov1bfVmUbf87wnfXSPmc"

accessToken <-	"409713602-aBMMJOsk0Vc1zWCOUSyBPz56nnboWjHLYsyKNeM9"
accessTokenSecret <-	"XK3jfmZjB8QOGAoTMndpZLs3HFvjIhtmi02gAYCf44sUH"

setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
tweets<-searchTwitteR("traficogt", n=1000, lang = "es")
tweetdf<-twListToDF(tweets)
dif_UTC_gt<-6*60*60
tweetdf$created2<-tweetdf$created-dif_UTC_gt
write.csv(tweetdf,"tweets.csv", fileEncoding = "UTF-8")

## Leer csv
data <- read.csv("tweets.csv", fileEncoding = "UTF-8")

## para analisis de retweets (mas abajo)
data_re <- read.csv("tweets.csv", fileEncoding = "UTF-8")

View(data)
View(data_re)

## LIMPIEZA DE DATOS ________________________________________________________________
#####################################################################################

# Quitar columnas
data<-data[,-c(5, 8, 9, 10, 11, 12, 16, 17)]


# Quitar URLs
data$text <- gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", data$text)

# Quitar #
data$text <- gsub('#\\S+', '', data$text)

# Quitar palabras o caracteres que no sirven
data$text <- gsub("RT", "", data$text)
data$text <- gsub("/", "", data$text)
data$text <- gsub("|", "", data$text)

# Quitar Emojis
data$text <- iconv(data$text, "latin1", "ASCII", sub="")

# Quitar tildes
data$text <- gsub("(á)|(Á)", "a", data$text)
data$text <- gsub("(é)|(É)", "e", data$text)
data$text <- gsub("(í)|(Í)", "i", data$text)
data$text <- gsub("(ó)|(Ó)", "o", data$text)
data$text <- gsub("(ú)|(Ú)", "u", data$text)

#Quitar numeros
data$text <- gsub('[0-9]+', '', data$text)

# Convertir a minusculas
data$text <-tolower(data$text)
View(data)
##  -------------------FIN PRIMERA LIMPIEZA DE LOS TWEETS--------------------


## ANALISIS GENERAL DE LOS TWEETS___________________________________________________________
###########################################################################################

## LIBRERIAS --------------------------------------------
library(readr)
library(dplyr)

library(ggplot2)
library(gridExtra)
library(stringr)
library(wordcloud)

## FUNCIONES------------------------------------------
# funcion para frecuencias de categoria
FreqCategory <- function(value) {
  strCategory <- ifelse(value <=5,     "      5",
                ifelse(value <=10,     "     10",
                ifelse(value <=20,     "     20",
                ifelse(value <=50,     "     50",
                ifelse(value <=100,    "    100",
                ifelse(value <=500,    "    500",
                ifelse(value <=1000,   "  1,000",
                              ">1,000")))))))
                strCategory
}
###----------------------------------------------------

dim(data)         #dimensiones
any(is.na(data))  #chequeo de valores faltantes
## Chequeo de columnas con valores faltantes
sapply(data, function(x){sum(is.na(x))}) 
str(data)
summary(data)
plot_intro(data)


# Conteo de tweets ______________________________________________________________-

head(data$text)
intLineCount <- length(data$text)
intLineCount

# PROMEDIO DE PALABRAS POR LINEA _________________________________________________
# split
lstUNPrfLines <- str_split(data$text," ")
# palabras por linea
vciUNPrfWperL <- unlist(lapply(lstUNPrfLines, length))
# promedio de palabras por linea
mean(vciUNPrfWperL)

# CONTEO DE PALABRAS _____________________________________________________________
# unlist para obtener el vector de palabras
vcsUNPrfWords <- unlist(lstUNPrfLines)
# numero de palabras = len(vector)
intWordCount <- length(vcsUNPrfWords)
# print
intWordCount

head(vcsUNPrfWords,100)

## LIMIPIANDO PALABRAS DE NUEVO _________________________________________________
# lower case
vcsUNPrfWords <- str_to_lower(vcsUNPrfWords)
# remove numbers
vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="[[:digit:]]", "")
# remove punctuation
vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="[[:punct:]]", "")
# remove white spaces
vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="[[:space:]]", "")
# remove special chars
vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="[~@#$%&-_=<>]", "")
# remove empty vectors
vcsUNPrfWords <- vcsUNPrfWords[vcsUNPrfWords != ""]
# hack & remove $
vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="$", "")
# head
head(vcsUNPrfWords,100)


# METEMOS TODO EN UN DATAFRAME ____________________________________________________
dfrUNPrfWords <- data.frame(vcsUNPrfWords)
colnames(dfrUNPrfWords) <- c("Words")
dfrUNPrfWords$Words <- as.character(dfrUNPrfWords$Words)
# normal word count
head(dfrUNPrfWords,10)

# resumen de los datos _____________________________________________________________
dfrUNPrfFreq <- dfrUNPrfWords %>% 
  group_by(Words) %>% 
  summarise(Freq=n()) %>% 
  arrange(desc(Freq))
head(dfrUNPrfFreq)

# nube de palabras existentes
wordcloud(dfrUNPrfFreq$Words[1:100], dfrUNPrfFreq$Freq[1:100], random.order=F, max.words=100,scale=c(3,.6))

#remover palabras cortas de menos de 3 letras
dfrUNPrfWords <- filter(dfrUNPrfWords, str_length(Words)>3)
head(dfrUNPrfWords)

# resumen de palabras significaticas __________________________________________________
dfrUNPrfFreq <- dfrUNPrfWords %>% 
  group_by(Words) %>% 
  summarise(Freq=n()) %>% 
  arrange(desc(Freq))
head(dfrUNPrfFreq)

tail(dfrUNPrfFreq) # palabras con muy poca frecuencia
# Se remueven alabras con una frecuencia <= 5
dfrUNPrfFreq <- filter(dfrUNPrfFreq, Freq>5)
tail(dfrUNPrfFreq)

# CONTEO DE PALABRAS SIGNIFICATIVAS _______________________________________________
intWordCountFinal <- length(dfrUNPrfFreq$Words)
# print
intWordCountFinal

# FRECUENCIAS CATEGORICAS ____________________________________________________________
# add FrequencyCategory colum
dfrUNPrfFreq <- mutate(dfrUNPrfFreq, Fcat=FreqCategory(dfrUNPrfFreq$Freq))
# new data frame for Frequency Of Categorized Frequencies ... 
dfrUNPrfFocf <- dfrUNPrfFreq %>% group_by(Fcat) %>% summarise(Rfrq=n())
# 
dfrUNPrfFocf$Fcat <- factor(dfrUNPrfFocf$Fcat, levels=dfrUNPrfFocf$Fcat, ordered=T)
# head
head(dfrUNPrfFocf,10)

## NUBE DE PALABRAS SIGNIFICATIVAS
wordcloud(dfrUNPrfFreq$Words[1:50], dfrUNPrfFreq$Freq[1:50], random.order=F, max.words=100, colors=brewer.pal(8, "Dark2"),scale=c(2,.10))

# graficos de palabras ____________________________________________________________________________________________
ggplot(slice(dfrUNPrfFreq,1:35), aes(x=reorder(Words,-Freq),y=Freq)) +
  geom_bar(stat="identity", fill=rainbow(35)) +
  ylab("Frecuencia") +
  xlab("Palabras") +
  ggtitle("Frecuencia de plabras - Palabras Top 35") +
  theme(plot.title=element_text(size=rel(1.5), colour="blue")) +
  coord_flip()

# Tabla de frecuencia del conteo de palabras
ggplot(dfrUNPrfFocf, aes(Fcat,Rfrq))+
  geom_bar(stat="identity", width=0.8, fill=rainbow(length(dfrUNPrfFocf$Fcat))) +
  xlab("Palabras con frecuencia menor a") + ylab("Frecuencia") +
  theme(axis.text.x=element_text(angle=60, hjust=1, vjust=1),axis.text.y=element_text(angle=60, hjust=1, vjust=1),plot.title=element_text(size=rel(1.5), colour="blue")) +
  ggtitle("Frecuencia del conteo de palabras")


dfrUNPrfChrs <- data.frame(Chars=nchar(dfrUNPrfFreq$Words))
#intRowCount <- nrow(table(dfrUNPrfChrs))
ggplot(dfrUNPrfChrs, aes(x=Chars)) +
  geom_histogram(binwidth=1, fill='blue') +
  geom_vline(xintercept=mean(nchar(dfrUNPrfFreq$Words)), color='black', size=1.5, alpha=.5) +
  xlab("Longitud de las palabras") + ylab("frecuancia de palabras")

################################################################################################
## FIN ANALISIS GENERAL DE LOS TWEETS___________________________________________________________





##_________________________________________________________________________________________________________________________________________________________





#######################################################################################################
## ANALISIS DE LOS TWEETS CON MAS RETWEETS______________________________________________________
###########################################################################################


#PARA ANALISIS DE RETWEETS (mas abajo)
data_re<-data_re[,-c(5, 8, 9, 10, 11, 12, 16, 17)]

# Quitar URLs
data_re$text <- gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", data_re$text)

# Quitar #
data_re$text <- gsub('#\\S+', '', data_re$text)

# Quitar palabras o caracteres que no sirven
data_re$text <- gsub("RT", "", data_re$text)
data_re$text <- gsub("/", "", data_re$text)
data_re$text <- gsub("|", "", data_re$text)

# Quitar Emojis
data_re$text <- iconv(data_re$text, "latin1", "ASCII", sub="")

# Quitar tildes
data_re$text <- gsub("(á)|(Á)", "a", data_re$text)
data_re$text <- gsub("(é)|(É)", "e", data_re$text)
data_re$text <- gsub("(í)|(Í)", "i", data_re$text)
data_re$text <- gsub("(ó)|(Ó)", "o", data_re$text)
data_re$text <- gsub("(ú)|(Ú)", "u", data_re$text)

#Quitar numeros
data_re$text <- gsub('[0-9]+', '', data_re$text)

# Convertir a minusculas
data_re$text <-tolower(data_re$text)
View(data_re)


#Histogramas de las variables cuantitativas_______________________________________
plot_histogram(data_re)
###Ver tweets que tengas la mayor cantidad de favoritos
favorites <- subset(data_re, data_re$favoriteCount >= 10)
sum(favorites)
View(favorites)

###Ver tweets que tengas la mayor cantidad de retweets
retweets <- subset(data_re, data_re$retweetCount >= 100)
sum(retweets)
View(retweets)


# Conteo de tweets ______________________________________________________________-

head(retweets$text)
intLineCount <- length(retweets$text)
intLineCount

# PROMEDIO DE PALABRAS POR LINEA _________________________________________________
# split
lstUNPrfLines <- str_split(retweets$text," ")
# palabras por linea
vciUNPrfWperL <- unlist(lapply(lstUNPrfLines, length))
# promedio de palabras por linea
mean(vciUNPrfWperL)

# CONTEO DE PALABRAS _____________________________________________________________
# unlist para obtener el vector de palabras
vcsUNPrfWords <- unlist(lstUNPrfLines)
# numero de palabras = len(vector)
intWordCount <- length(vcsUNPrfWords)
# print
intWordCount

head(vcsUNPrfWords,100)

## LIMIPIANDO PALABRAS DE NUEVO _________________________________________________
# lower case
vcsUNPrfWords <- str_to_lower(vcsUNPrfWords)
# remove numbers
vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="[[:digit:]]", "")
# remove punctuation
vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="[[:punct:]]", "")
# remove white spaces
vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="[[:space:]]", "")
# remove special chars
vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="[~@#$%&-_=<>]", "")
# remove empty vectors
vcsUNPrfWords <- vcsUNPrfWords[vcsUNPrfWords != ""]
# hack & remove $
vcsUNPrfWords <- str_replace_all(vcsUNPrfWords, pattern="$", "")
# head
head(vcsUNPrfWords,100)


# METEMOS TODO EN UN DATAFRAME ____________________________________________________
dfrUNPrfWords <- data.frame(vcsUNPrfWords)
colnames(dfrUNPrfWords) <- c("Words")
dfrUNPrfWords$Words <- as.character(dfrUNPrfWords$Words)
# normal word count
head(dfrUNPrfWords,10)

# resumen de los datos _____________________________________________________________
dfrUNPrfFreq <- dfrUNPrfWords %>% 
  group_by(Words) %>% 
  summarise(Freq=n()) %>% 
  arrange(desc(Freq))
head(dfrUNPrfFreq)

# nube de palabras existentes
wordcloud(dfrUNPrfFreq$Words[1:50], dfrUNPrfFreq$Freq[1:50], random.order=F, max.words=100,scale=c(1,.100))

#remover palabras cortas de menos de 3 letras
dfrUNPrfWords <- filter(dfrUNPrfWords, str_length(Words)>3)
head(dfrUNPrfWords)

# resumen de palabras significaticas __________________________________________________
dfrUNPrfFreq <- dfrUNPrfWords %>% 
  group_by(Words) %>% 
  summarise(Freq=n()) %>% 
  arrange(desc(Freq))
head(dfrUNPrfFreq)

tail(dfrUNPrfFreq) # palabras con muy poca frecuencia
# Se remueven alabras con una frecuencia <= 5
dfrUNPrfFreq <- filter(dfrUNPrfFreq, Freq>5)
tail(dfrUNPrfFreq)

# CONTEO DE PALABRAS SIGNIFICATIVAS _______________________________________________
intWordCountFinal <- length(dfrUNPrfFreq$Words)
# print
intWordCountFinal

# FRECUENCIAS CATEGORICAS ____________________________________________________________
# add FrequencyCategory colum
dfrUNPrfFreq <- mutate(dfrUNPrfFreq, Fcat=FreqCategory(dfrUNPrfFreq$Freq))
# new data frame for Frequency Of Categorized Frequencies ... 
dfrUNPrfFocf <- dfrUNPrfFreq %>% group_by(Fcat) %>% summarise(Rfrq=n())
# 
dfrUNPrfFocf$Fcat <- factor(dfrUNPrfFocf$Fcat, levels=dfrUNPrfFocf$Fcat, ordered=T)
# head
head(dfrUNPrfFocf,10)

## NUBE DE PALABRAS SIGNIFICATIVAS
wordcloud(dfrUNPrfFreq$Words[1:13], dfrUNPrfFreq$Freq[1:13], random.order=F, max.words=100, colors=brewer.pal(8, "Dark2"),scale=c(2,.10))

# graficos de palabras ____________________________________________________________________________________________
ggplot(slice(dfrUNPrfFreq,1:10), aes(x=reorder(Words,-Freq),y=Freq)) +
  geom_bar(stat="identity", fill=rainbow(10)) +
  ylab("Frecuencia") +
  xlab("Palabras") +
  ggtitle("Frecuencia de plabras - Palabras Top 10") +
  theme(plot.title=element_text(size=rel(1.5), colour="blue")) +
  coord_flip()

# Tabla de frecuencia del conteo de palabras
ggplot(dfrUNPrfFocf, aes(Fcat,Rfrq))+
  geom_bar(stat="identity", width=0.8, fill=rainbow(length(dfrUNPrfFocf$Fcat))) +
  xlab("Palabras con frecuencia menor a") + ylab("Frecuencia") +
  theme(axis.text.x=element_text(angle=60, hjust=1, vjust=1),axis.text.y=element_text(angle=60, hjust=1, vjust=1),plot.title=element_text(size=rel(1.5), colour="blue")) +
  ggtitle("Frecuencia del conteo de palabras")


dfrUNPrfChrs <- data.frame(Chars=nchar(dfrUNPrfFreq$Words))
#intRowCount <- nrow(table(dfrUNPrfChrs))
ggplot(dfrUNPrfChrs, aes(x=Chars)) +
  geom_histogram(binwidth=1, fill='blue') +
  geom_vline(xintercept=mean(nchar(dfrUNPrfFreq$Words)), color='black', size=1.5, alpha=.5) +
  xlab("Longitud de las palabras") + ylab("frecuancia de palabras")

################################################################################################
## FIN ANALISIS___________________________________________________________




