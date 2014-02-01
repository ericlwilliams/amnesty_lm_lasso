library(tm)
library(ggplot2)
library(data.table)
library(plyr)
library(wordcloud)
library(RColorBrewer)
library("glmnet")
library("boot")


data.path <- "./data/aiua.csv"

# split data up into wfu (with follow-up) and wofu (without-followup)
# use random 75% of entries of each 

# build text corpus
ua.dt <- read.csv(data.path,sep=",",stringsAsFactors=FALSE)
ua.dt <- data.table(ua.dt)

ua.dt<-ua.dt[,list(id=data_id,body=body,status=action,idate=issue_date)]
# drop if no data in body


ua.dt<-ua.dt[which(body!=""),]

# get number of times each id has a ua
id.counts <- data.table(count(ua.dt,"id"))
# for logistic
id.counts[,wfu:=as.numeric(freq!=1)]
# ad to dt
ua.dt<-merge(ua.dt,id.counts[2:nrow(id.counts),],by="id")
# For those with followups, only interested in first entry, remove duplicates by id
ua.dt<-ua.dt[!duplicated(ua.dt$id),]



body.lengths <- data.frame(freq=sapply(ua.dt$body, function(s) nchar(s)))
ggplot(body.lengths,aes(x=log(freq))) +
geom_histogram(binwidth=.1)
ggsave("./figures/h_body_nchar.pdf")
# Simplify to random selection of 500 UAs
indices <- sample(1:nrow(ua.dt), 750)
ua.dt <- ua.dt[indices,]

body.lengths <- data.frame(freq=sapply(ua.dt$body, function(s) nchar(s)))
ggplot(body.lengths,aes(x=log(freq))) +
geom_histogram(binwidth=.1)
ggsave("./figures/h_trim_body_nchar.pdf")

browser()
documents <- data.frame(Text = ua.dt$body)

row.names(documents) <- 1:nrow(documents)
corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

aiua.dtm <- DocumentTermMatrix(corpus)

aiua.dtm <- as.matrix(aiua.dtm)

# For word cloud
v <- sort(rowSums(t(aiua.dtm)),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("./figures/aiua_wordcloud.png", width=1280,height=800)
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()

# Get data in glmnet readible form
x <- as.matrix(aiua.dtm)
# First 50 (top ranked) assigned 1, lower ranked 50 assigned 0
y <- ua.dt$wfu

# Cross-validation
# cvfit <- cv.glmnet(x,y,nfolds=50,lambda=c(0.0001, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.1, 0.5),family="binomial",type.measure="class")

# Does k-fold cross-validation for glmnet, produces a plot, and returns a value for lambda

cvfit <- cv.glmnet(x,y,nfolds=10,family="binomial",type.measure="class")

pdf("./figures/aiua_follow_up_misclass_v_lambda.pdf")
plot(cvfit)
dev.off()

best.lambda <- cvfit$lambda.min

# glmfit.test <- cvfit.glmnet.fit1

pdf("./figures/aiua_follow_up_coef_v_lambda.pdf")
plot(glmfit.test,xvar="lambda")
# x=matrix(rnorm(100*20),100,20)
# y=rnorm(100)
# g2=sample(1:2,100,replace=TRUE)
# g4=sample(1:4,100,replace=TRUE)
# fit1=glmnet(x,y)
# predict(fit1,newx=x[1:5,],s=c(0.01,0.005))
# predict(fit1,type="coef")
# fit2=glmnet(x,g2,family="binomial")
# predict(fit2,type="response",newx=x[2:5,])
# predict(fit2,type="nonzero")
# fit3=glmnet(x,g4,family="multinomial")
# predict(fit3,newx=x[1:3,],type="response",s=0.01)

