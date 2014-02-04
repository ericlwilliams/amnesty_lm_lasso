library(tm)
library(ggplot2)
library(data.table)
library(plyr)
library(wordcloud)
library(RColorBrewer)
library(glmnet)
library(boot)

data.path <- "./data/aiua.csv"

# Sparsity 
sparsity = 0.99

# Category dictionary
# [1] "cat_abduction"               "cat_arbitrary_arrest"
#  [3] "cat_arbitrary_detention"     "cat_conscientious_objector"
#  [5] "cat_corporate_abuse"         "cat_death_penalty"
#  [7] "cat_death_threat"            "cat_enforced_disappearance"
#  [9] "cat_excessive_force"         "cat_extrajudicial_execution"
# [11] "cat_fear_for_safety"         "cat_fear_of_torture"
# [13] "cat_forced_eviction"         "cat_forced_return"
# [15] "cat_freedom_of_expression"   "cat_harassment"
# [17] "cat_harsh_prison_conditions" "cat_health_concern"
# [19] "cat_house_arrest"            "cat_hunger_strike"
# [21] "cat_ill_treatment"           "cat_imminent_execution"
# [23] "cat_incommunicado_detention" "cat_legal_concern"
# [25] "cat_lgbti_rights"            "cat_mass_arrest"
# [27] "cat_medical_concern"         "cat_other"
# [29] "cat_prisoner_of_conscience"  "cat_refugee_rights"
# [31] "cat_risk_of_torture"         "cat_torture"
# [33] "cat_unfair_trial"            "cat_unjust_arrest"
# [35] "cat_unjust_imprisonment"     "cat_unlawful_killing"
# [37] "cat_women_s_rights"          "cat_youth_children_s_rights"

# category = "all"
category = "cat_freedom_of_expression"
save_path = paste("./figures/",category,sep="")

dir.create(save_path, showWarnings = FALSE)


ua.dt <- read.csv(data.path,sep=",",stringsAsFactors=FALSE)

if(category == "all"){
	cat_ind <- -1
}else{
	cats <- seq(6,43)
	names(cats) <- colnames(ua.dt)[cats]
	cat_ind <- cats[category]
}



# split data up into wfu (with follow-up) and wofu (without-followup)
# use random 75% of entries of each 


if(cat_ind>=0){
	ua.dt <- ua.dt[which(ua.dt[,cat_ind]==1),]
}
# skim for category

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


word.freq <- sapply(ua.dt$body, function(s) nchar(s))
body.lengths <- data.frame(freq=word.freq)

# default body entry very common 
# "urgent  action uanetwork office aiusa 600 pennsylvania ave se, washington dc 20003 t. 202.544.0200 f. 202.675.8566 e. uan@aiusa.org amnestyusa.org/urgent/ "
#  to find and optionally remove index 155
# print(ua.dt$body[which(body.lengths$freq==155)])

ggplot(body.lengths,aes(x=log(freq))) +
geom_histogram(binwidth=.1)
ggsave(paste(save_path,"h_body_nchar.pdf",sep="/"))


# Simplify to random selection of 500 UAs


# indices <- sample(1:nrow(ua.dt), 2000)
# ua.dt <- ua.dt[indices,]

body.lengths <- data.frame(freq=word.freq)
ggplot(body.lengths,aes(x=log(freq))) +
geom_histogram(binwidth=.1)
ggsave(paste(save_path,"h_trim_body_nchar.pdf",sep="/"))


documents <- data.frame(Text = ua.dt$body)

row.names(documents) <- 1:nrow(documents)
corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'),"amnesty","international","information","human","rights","uan@aiusa.org","(m)","amnestyusa.org/urgent/","()","(),"))

aiua.dtm <- DocumentTermMatrix(corpus)
# testing sparcity
# Removes terms which have at least sparse fraction of sparsity 
# (eg. sparse = 0.9 <- Remove terms that *aren't* in 90% or more of documents)


aiua.dtm <- removeSparseTerms(aiua.dtm,sparse=sparsity)

aiua.dtm <- as.matrix(aiua.dtm)

# browser()
# # Only keep 100 top terms
# aiua.dtm <- sort(rowSums(aiua.dtm),decreasing=TRUE)
# aiua.dtm <- head(aiua.dtm,100)
# browser()

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

pdf(paste(save_path,"aiua_follow_up_misclass_v_lambda.pdf",sep="/"))
plot(cvfit)
dev.off()



glmfit.test <- cvfit$glmnet.fit

pdf(paste(save_path,"aiua_follow_up_coef_v_lambda.pdf",sep="/"))
plot(glmfit.test,xvar="lambda")
dev.off()

best.lambda <- cvfit$lambda.min

fits <- glmnet(x,y,family="binomial",lambda=best.lambda)


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


