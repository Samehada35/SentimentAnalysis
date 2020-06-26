library(twitteR)

#clés pour la connexion
consumer_key <- "2nHji7ad66TYKAvE7RUYYHsmL"
consumer_secret <- "uYTdrwx9OBkm57bVuVRq3AeGojs9yc32PtsAhWjIBq7nhoDNLD"
access_token <- "1161003528489488384-OwPuy3mLdx1wLiYQzL9bC279r6Zzpi"
access_secret <- "GLXTeKgxDCWoquEg4POO9gl5M0AtvBgWwkoUrdmjvXHey"
#Créer une connexion avec Twitter
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets <- searchTwitter("#coronavirus",n=3000,lang="en")
df <- twListToDF(tweets)
write.csv2(df,"tweets.csv",row.names = FALSE)


#Pour avoir les mêmes résultats, commencer à partir de cette ligne
df <- read.csv2("tweets.csv",header=TRUE)

#Statistiques descriptives

#Dimensions de la dataframe
print(dim(df))

#Colonnes de la dataframe
print(colnames(df))

#Nombre d'auteurs de tweets uniques
print(length(unique(df$screenName)))

#Distribution des auteurs de tweets
auteurs_count = table(df$screenName)
barplot(auteurs_count[auteurs_count>3],las=2,col="blue",main="Distribution auteurs tweets")

#Nombre de retweets
print(table(df$isRetweet)['TRUE'])

#Nombre de messages originaux
print(table(df$isRetweet)['FALSE'])

#Distribution des auteurs de tweets originaux
auteurs_originaux_count = table(df$screenName[which(df$isRetweet==FALSE)])
barplot(auteurs_originaux_count[auteurs_originaux_count>1],las=2,col="green")

#Analyse des trends

#Tweets les plus retweetés (on ne prend que les 20 premiers)
df_retweets = df[df$isRetweet,]
retweets_order = order(df_retweets$retweetCount,decreasing = TRUE)
ordered_retweets = df_retweets[retweets_order,c('screenName','id','retweetCount')]
print(ordered_retweets[1:20,])
barplot(ordered_retweets$retweetCount[1:20],names.arg=ordered_retweets$screenName[1:20],las=2,col='orange')

#Distribution des tweets selon nombre retweets
hist(df$retweetCount,xlab="Nombre Retweets",main="Distribution des tweets selon nombre retweets",col="yellow",breaks=1000,xlim=c(0,3000))

#Analyse thèmes
get_hashtags = function(x){
  tokens = unlist(strsplit(unlist(x)," "));
  hashtags_regex = regexpr("^#\\w+",tokens);
  regmatches(tokens,hashtags_regex)
}

hashtags = lapply(df$text,get_hashtags)
hashtags

#On réduit les hashtags de chaque tweet dans une seule liste
hashtags = do.call(c,hashtags)
hashtags

#Distribution hashtags
hashtags_uniques = sort(table(hashtags),decreasing = TRUE)
print(hashtags_uniques[1:10])
#Les hashtags les plus populaires sont : #coronavirus,#covid19 et #confinement

barplot(hashtags_uniques[1:10],las=2,col=rainbow(10))


#Preprocessing des tweets

#Encodage des tweets en UTF-8
install.packages("utf8")
library(utf8)

Encoding(df$text) <- rep("UTF-8",nrow(df))

#Elimination de doublons
df = df[!duplicated(df$text),]
print(nrow(df))
#Il reste 1686 tweets uniques

#Nettoyage des tweets
to_lower <- function(x){ tolower(x)}
rem_blanks <- function(x){ gsub("[\n\r\t]+"," ",x)}
rem_links <- function(x){ gsub("\\bhttps?://(www\\.)?\\S+\\s*"," ",x)}
rem_emails <- function(x){gsub('([a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-]+)'," ",x)}
rem_tags <- function(x){ gsub("@\\S+"," ",x)}
replace_accents <- function(x){ y=gsub("[éèêë]","e",x);z=gsub("[àâ]","a",y);gsub("[ûù]","u",z)}
rem_special_chars <- function(x){ gsub("[^[:alnum:]#]+"," ",x)}
rem_extra_spaces <- function(x){ trimws(gsub("\\s{2,}"," ",x))}
rem_rt <- function(x){ gsub("^rt","",x)}
rem_hashtags <- function(x){ gsub("#\\w+","",x)}

df$text

#Normaliser tous les tweets en minuscule
df$text = lapply(df$text,to_lower)
df$text

#Retirer tabulations, sauts de lignes et retours chariot
df$text = lapply(df$text,rem_blanks)
df$text

#Retirer les liens (N'importe quel protocole, pas forcément http ou https)
df$text = lapply(df$text,rem_links)
df$text

#Retirer les adresses mail
df$text = lapply(df$text,rem_emails)
df$text

#Retirer les tags (@nom_personne)
df$text = lapply(df$text,rem_tags)
df$text

#Retirer les accents
df$text = lapply(df$text,replace_accents)
df$text

#Retirer les caractères spéciaux (Tout ce qui n'est pas alphanumérique)
df$text = lapply(df$text,rem_special_chars)
df$text

#Retirer RT
df$text = lapply(df$text,rem_rt)
df$text

#On retire les hashtags
df$text = lapply(df$text,rem_hashtags)
df$text

#Retirer les espaces superflux
df$text = lapply(df$text,rem_extra_spaces)
df$text


#Après nettoyage des données
df = df[!duplicated(df$text),]
print(nrow(df))


#Text mining

#On crée notre corpus
library(tm)
library(SnowballC)
corpus <- Corpus(VectorSource(df$text))

#Affichage du corpus
corpus$content

#création de la MDT à partir du corpus (On précise ici de supprimer les ponctuations, nombres,stopwords etc...)
#Nous faisons également un stemming des mots
mdt <- DocumentTermMatrix(corpus,control=list(weighting=weightBin,
                                              language="fr",
                                              removePunctuation = TRUE,
                                              removeNumbers = TRUE,
                                              stopwords=TRUE,
                                              stemming=TRUE))
print(mdt)

#Termes apparaissant le plus souvent
mdt_mat = as.matrix(mdt)
terms_frequency = sort(colSums(mdt_mat),decreasing=TRUE)

barplot(terms_frequency[1:20],las=2,col=heat.colors(20))
#Les termes qui apparaissent le plus souvent sont : pandémie, crise, masque, confinement








# Analyse des sentiments
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(textdata)


get_sentiments("nrc")
table(get_sentiments("nrc")["sentiment"])

df$text = unlist(df$text)

glimpse(df)

tokens = df %>% unnest_tokens(word,text)
View(tokens)
dim(tokens)

df_sent = tokens %>%
  inner_join(get_sentiments("nrc")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0)

dist_sentiments = data.frame(unlist(df_sent))
dist_sentiments

dist_sentiments['sentiments'] = rownames(dist_sentiments)
colnames(dist_sentiments)[1] = "count"
rownames(dist_sentiments) = 1:nrow(dist_sentiments)
dist_sentiments

ggplot(dist_sentiments,aes(x="",y=count,fill=sentiments)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(label = count), color = "white", size=7, position = position_stack(vjust = 0.5))+
  labs(title="Distribution sentiments des tweets")

barplot(count~sentiments,dist_sentiments,col=rainbow(nrow(dist_sentiments)),main="Distribution sentiments des tweets",las=2)
#On remarque que les sentiments positives et negative sont les plus présents, suivis par trust et fear
#Cela n'est pas étonnant vu que les tweets parlent du coronavirus, et donc la plupart des gens ont peur
#Ou au contraire se sentent confiants

GetSentiment <- function(row){
  toks = row %>% unnest_tokens(word,text)
  sentiments = toks %>%
    inner_join(get_sentiments("nrc")) %>% # pull out only sentiment words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0)
  sentiments
}


most_popular_tweets = df[order(df$retweetCount,decreasing=TRUE),][1:100,]
most_popular_tweets_sents = GetSentiment(most_popular_tweets)
most_popular_tweets_sents

most_popular_tweets_sents_df = data.frame(unlist(most_popular_tweets_sents))

most_popular_tweets_sents_df['sentiments'] = rownames(most_popular_tweets_sents_df)
colnames(most_popular_tweets_sents_df)[1] = "count"
rownames(most_popular_tweets_sents_df) = 1:nrow(most_popular_tweets_sents_df)
most_popular_tweets_sents_df

barplot(count~sentiments,most_popular_tweets_sents_df,col=rainbow(nrow(most_popular_tweets_sents_df)),main="Distribution sentiments des tweets les plus populaires",las=2)
#Les tweets les plus populaires sont ceux qui sont négatifs et portent un sentiment de peur
#Nous remarquons aussi que le sentiment "sadness" est plus présent, la tristesse gagne sur la confiance
#Dans les tweets les plus populaires.

df2 = df
df2$sentimentDominant = rep(0,nrow(df2))

for(i in 1:nrow(df2)){
  sentiments_tweet = GetSentiment(df2[i,]) #On réccupère la liste des sentiments d'un tweet unique
  if(length(sentiments_tweet)==0){
    df2[i,"sentimentDominant"] = "none"
  }else{
    sentiment_dominant = colnames(sentiments_tweet)[which(sentiments_tweet == max(sentiments_tweet))][1] #On réccupère le sentiment dominant
    df2[i,"sentimentDominant"] = sentiment_dominant
  }
}

#Nous avons maintenant une variable illustrative qui indique le sentiment dominant de chaque tweet
#C'est ce qu'on utilisera pour les différentes techniques d'acp et clsutering
df2$sentimentDominant

library(tm)
corpus <- Corpus(VectorSource(df2$text))
corpus$content
mdt <- DocumentTermMatrix(corpus,control=list(weighting=weightTfIdf,
                                              language="fr",
                                              removePunctuation = TRUE,
                                              removeNumbers = TRUE,
                                              stopwords=TRUE,
                                              stemming=TRUE))
data.frame(df[c(69,280,444,831,899),"text"])

textes.actifs = data.frame(as.matrix(mdt))
textes.illus = data.frame(df2$sentimentDominant)

dim(textes.actifs)
dim(textes.illus)

textes.actifs = textes.actifs[-c(69,280,444,831,899),]
textes.illus = data.frame(textes.illus[-c(69,280,444,831,899),])

dim(textes.actifs)
dim(textes.illus)

colnames(textes.illus) = c('sentimentDominant')

print(textes.actifs)
print(textes.illus)


library (FactoMineR)
textes.acp = PCA(textes.actifs,ncp=11,graph=T)

print(textes.acp)

print(summary(textes.acp))

print(textes.acp$eig)

print(textes.acp$var$contrib)

plot(textes.acp,choix="ind",ylim=c(-1,+1),xlim=c(-1,1))

print(textes.acp$eig[500,3])
#Les 500 premières composantes expliquent environ 60% de la variance

#Traitement variables illustratives

#positionner les modalités de la variable illustrative + calcul des valeurs test
K <- nlevels(textes.illus[,1])
var.illus <- unclass(textes.illus[,1])
m1 <- c()
m2 <- c()
for (i in 1:K){m1[i] <- mean(textes.acp$ind$coord[var.illus==i,1])}
for (i in 1:K){m2[i] <- mean(textes.acp$ind$coord[var.illus==i,2])}
cond.moyenne <- cbind(m1,m2)
rownames(cond.moyenne) <- levels(textes.illus[,1])
print(cond.moyenne)


#graphique
plot(c(textes.acp$ind$coord[,1],m1),c(textes.acp$ind$coord[,2],m2),xlab="Comp.1",ylab="Comp.2",main="Positionnement var.illus catégorielle",type="n",ylim = c(-1,1),xlim=c(-1,1))
abline(h=0,v=0)
text(textes.acp$ind$coord[,1],textes.acp$ind$coord[,2],rownames(textes.actifs),cex=0.5)
text(m1,m2,rownames(cond.moyenne),cex=1.5,col="red")

#AFC
textes.afc <- CA(textes.actifs,ncp=11,graph=TRUE,row.sup=NULL,col.sup=NULL)

textes.afc

#statistiques sur le tableau de données
print(textes.afc$call)

#tableau des valeurs propres et Scree plot
print(textes.afc$eig)
plot(textes.afc$eig[,1],type="b",main="Scree plot")

#coordonnées, contributions et cos2 - lignes
print(textes.afc$row)

#coordonn?es, contributions et cos2 - colonnes
print(textes.afc$col)

#graphique
plot(textes.afc,cex=0.75,ylim=c(-40,+40))

#positionner les modalités de la variable illustrative + calcul des valeurs test
K <- nlevels(textes.illus[,1])
var.illus <- unclass(textes.illus[,1])
m1 <- c()
m2 <- c()
for (i in 1:K){m1[i] <- mean(textes.afc$row$coord[var.illus==i,1])}
for (i in 1:K){m2[i] <- mean(textes.afc$row$coord[var.illus==i,2])}
cond.moyenne <- cbind(m1,m2)
rownames(cond.moyenne) <- levels(textes.illus[,1])
print(cond.moyenne)


#graphique
plot(c(textes.afc$row$coord[,1],m1),c(textes.afc$row$coord[,2],m2),xlab="Comp.1",ylab="Comp.2",main="Positionnement var.illus catégorielle",type="n",ylim = c(-0.5,0.5),xlim=c(-0.5,0.5))
abline(h=0,v=0)
text(textes.afc$row$coord[,1],textes.afc$row$coord[,2],rownames(textes.actifs),cex=0.5)
text(m1,m2,rownames(cond.moyenne),cex=0.95,col="red")


#######
#ACH
#######

#Centrage et r?duction des donn?es
data<-scale(textes.actifs, center = TRUE, scale = TRUE)
#Calcul du tableau des distances
dc<-dist(data, method ="euclidean", diag=FALSE, upper=FALSE)

hc <- hclust(dc,method='ward.D')
hc

plot(hc)

#Pour visualiser le dendrogramme
plot(hc,hang=-1)

#Pour avoir la partition en 11 classes :
class=cutree(hc,11)

pairs(textes.actifs[1:10],col=class[1:10])


unclass(hc)

#La composante height donne les valeurs (ou indices) de la hi?rarchie :

hc$height

# tracer un rectangle sur les classes
plot(hc,hang=-1)
rect.hclust(hc, k=11, border="red") 

# Visualiser les classes sur le premier plan factoriel
library(cluster)

clusplot(dc,class,diss=T,shade=T,color=T,labels=11,main="")
abline(v=0,h=0)

#Avec ACP
plot(textes.acp,choix="ind", habillage="ind",col.hab=class,xlim=c(-1,1),ylim=c(-1,1))


# Faire une CAH selon diff?rents critères d'agrégation
dc=dist(textes.actifs)
par(mfrow=c(2,3))
possible <- c("ward.D", "single", "complete", "average", "mcquitty","median",
              "centroid")
for(k in 2:6) plot(hclust(dc,possible[k]),hang=-1,main=possible[k])
plot(hclust(dc^2,"ward.D"),hang=-1,main="inertie interclasse")
par(mfrow=c(1,1))

cbind(sort(class),textes.illus$sentimentDominant)


# choix du nombre de classes (utliser les indices de la hi?rarchie hc$height ? utilier avec ward sur le carr? de la distance dc^2)
par(mfrow=c(1,2))
plot(hc$height[67 :1],type="b")
plot(hc$height[67 :50],type="b")
par(mfrow=c(1,1))

# # choix du nombre de classes : utiliser le Rsquare (R2) et PseudoF
#Choix du nombre de classes

m=ncol(textes.actifs)
n=20

R2=0
x=textes.actifs
Iinter = 0
g = apply(x, 2, mean)
I = sum(diag(var(x))) # Inertie totale

dendro=hclust(dc,"ward.D")

for(i in 2:n){
  class = cutree(dendro,k=i)
  ncl = unique(class)
  d = numeric(length(ncl))
  nb = integer(length(ncl))
  for(j in ncl){
    nb[j] = sum(class==j)
    if(nb[j]>1){
      m = apply(x[class==j,], 2, mean)
    } else {
      m = x[class==j,]
    }
    d[j] = sum((m-g)^2)
  }
  Iinter = (1/n)*sum(nb*d)
  R2[i] = Iinter/I
}
R2
ncl = 2:(n-1)
PseudoF = (R2[ncl]/(ncl-1))/((1-R2[ncl])/(n-ncl))

plot(1:(n-1),R2[1:(n-1)], type = 'b', xlab = "Nombre de classes", ylab = "Rsquare")
title("Indice du R2")

plot(ncl[1:n-1],PseudoF[1:n-1], type = 'b', xlab = "Nombre de classes", ylab = "PseudoF")
title("Indice du PseudoF")


layout(matrix(c(1:2),1,2))
plot(1:(n-1),R2[1:(n-1)], type = 'b', xlab = "Nombre de classes", ylab = "Rsquare")
title("Indice du R2")
plot(ncl[1:(n-1)],PseudoF[1:(n-1)], type = 'b', xlab = "Nombre de classes", ylab = "PseudoF")
title("Indice du PseudoF")
layout(1)

Intra=I-R2
Intra
plot(1:(n-1),Intra[1:(n-1)], type = 'b', xlab = "Nombre de classes", ylab = "Inertie Intracalsse")
title("Inertie intraclasse")











#KMeans
km=kmeans(textes.actifs,11)

print(km$cluster)

clus_sent = data.frame(cbind(km$cluster,textes.illus$sentimentDominant))

colnames(clus_sent) = c('cluster','sentiment')

clus_sent

data=textes.actifs

wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Nombre de classes",ylab="Somme des carrés intra classe") 


# Evolution de linertie interclasse
bss=0
for (i in 2:15) bss[i] <- sum(kmeans(data, centers=i)$betweenss)
plot(1:15, bss, type="b", xlab="Nombre de classes",ylab="Somme des carrés inter classe") 


layout(matrix(c(1:2),1,2))
plot(1:15, wss, type="b", xlab="Nombre de classes",ylab="Somme des carr?s intra classe") 
plot(1:15, bss, type="b", xlab="Nombre de classes",ylab="Somme des carr?s inter classe") 
layout(1)


layout(1)
#Projection des classes sur les premières composantes principales
plot(textes.acp$ind$coord,col=km$cluster, pch=16, cex=1,xlim=c(-0.5,0.5),ylim=c(-0.5,0.5))
text(textes.acp$ind$coord[,1],textes.acp$ind$coord[,2],row.names(textes.acp$ind$coord),cex=0.3)
text(m1,m2,rownames(cond.moyenne),cex=0.95,col="red")
abline(v = 0, h = 0)

#Projection des classes sur les premières composantes principales (AFC)
plot(textes.afc$row$coord,col=km$cluster, pch=16, cex=2,xlim=c(-1,1),ylim=c(-1,1))
text(textes.afc$row$coord[,1],textes.afc$row$coord[,2],row.names(textes.afc$row$coord))
abline(v = 0, h = 0)
