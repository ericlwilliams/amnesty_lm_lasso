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

category = "all"
# category = "cat_freedom_of_expression"
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

documents <- data.frame(Text = ua.dt$body)

row.names(documents) <- 1:nrow(documents)
corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'),"amnesty","international","information","human","rights","uan@aiusa.org","(m)","amnestyusa.org/urgent/","()","(),"," ()", "() "," (),","(), "))

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

png(paste(save_path,"aiua_wordcloud.png",sep="/"), width=1280,height=800)
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()


# Get data in glmnet readible form
x <- as.matrix(aiua.dtm)
y <- ua.dt$wfu

# Use 80% for train/cv, 20% for test
training.indices <- sort(sample(1:nrow(aiua.dtm), round(0.8*nrow(aiua.dtm))))
test.indices <- which(! 1:nrow(aiua.dtm) %in% training.indices)

train.x <- x[training.indices,]
train.y <- y[training.indices]

test.x <- x[test.indices,]
test.y <- y[test.indices]


# Cross-validation
# Does k-fold cross-validation for glmnet, produces a plot, and returns a value for lambda
cvfit <- cv.glmnet(train.x,train.y,nfolds=10,family="binomial",type.measure="class")

pdf(paste(save_path,"aiua_follow_up_misclass_v_lambda.pdf",sep="/"))
plot(cvfit)
dev.off()

glmfit.test <- cvfit$glmnet.fit
pdf(paste(save_path,"aiua_follow_up_coef_v_lambda.pdf",sep="/"))
plot(glmfit.test,xvar="lambda")
dev.off()

# print best miss-classification rate
predictions <- predict(cvfit,test.x, s = cvfit$lambda.min)
predictions <- as.numeric(predictions > 0)
mse <- mean(predictions != test.y)
print(sprintf("Logit best (CV lambda = %.2e)",cvfit$lambda.min))
print(sprintf("test miss-classification: %.3f",mse))
