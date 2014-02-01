
```r
library(tm)
library(ggplot2)
library(data.table)
```

```
## data.table 1.8.10  For help type: help("data.table")
```

```r
library(plyr)
library(wordcloud)
```

```
## Loading required package: Rcpp
## Loading required package: RColorBrewer
```

```r
library(RColorBrewer)
library("glmnet")
```

```
## Loading required package: Matrix
## Loaded glmnet 1.9-5
```

```r
library("boot")


data.path <- "./data/aiua.csv"

# split data up into wfu (with follow-up) and wofu (without-followup) use
# random 75% of entries of each

# build text corpus
ua.dt <- read.csv(data.path, sep = ",", stringsAsFactors = FALSE)
```

```
## Warning: cannot open file './data/aiua.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r
ua.dt <- data.table(ua.dt)
```

```
## Error: object 'ua.dt' not found
```

```r

ua.dt <- ua.dt[, list(id = data_id, body = body, status = action, idate = issue_date)]
```

```
## Error: object 'ua.dt' not found
```

```r
# drop if no data in body


ua.dt <- ua.dt[which(body != ""), ]
```

```
## Error: object 'ua.dt' not found
```

```r

# get number of times each id has a ua
id.counts <- data.table(count(ua.dt, "id"))
```

```
## Error: object 'ua.dt' not found
```

```r
# for logistic
id.counts[, `:=`(wfu, as.numeric(freq != 1))]
```

```
## Error: object 'id.counts' not found
```

```r
# ad to dt
ua.dt <- merge(ua.dt, id.counts[2:nrow(id.counts), ], by = "id")
```

```
## Error: object 'ua.dt' not found
```

```r
# For those with followups, only interested in first entry, remove
# duplicates by id
ua.dt <- ua.dt[!duplicated(ua.dt$id), ]
```

```
## Error: object 'ua.dt' not found
```

```r



body.lengths <- data.frame(freq = sapply(ua.dt$body, function(s) nchar(s)))
```

```
## Error: object 'ua.dt' not found
```

```r
ggplot(body.lengths, aes(x = log(freq))) + geom_histogram(binwidth = 0.1)
```

```
## Error: object 'body.lengths' not found
```

```r
ggsave("./figures/h_body_nchar.pdf")
```

```
## Error: plot should be a ggplot2 plot
```

```r
# Simplify to random selection of 500 UAs
indices <- sample(1:nrow(ua.dt), 750)
```

```
## Error: object 'ua.dt' not found
```

```r
ua.dt <- ua.dt[indices, ]
```

```
## Error: object 'ua.dt' not found
```

```r

body.lengths <- data.frame(freq = sapply(ua.dt$body, function(s) nchar(s)))
```

```
## Error: object 'ua.dt' not found
```

```r
ggplot(body.lengths, aes(x = log(freq))) + geom_histogram(binwidth = 0.1)
```

```
## Error: object 'body.lengths' not found
```

```r
ggsave("./figures/h_trim_body_nchar.pdf")
```

```
## Error: plot should be a ggplot2 plot
```

```r

browser()
```

```
## Called from: eval(expr, envir, enclos)
```

```r
documents <- data.frame(Text = ua.dt$body)
```

```
## Error: object 'ua.dt' not found
```

```r

row.names(documents) <- 1:nrow(documents)
```

```
## Error: object 'documents' not found
```

```r
corpus <- Corpus(DataframeSource(documents))
```

```
## Error: object 'documents' not found
```

```r
corpus <- tm_map(corpus, tolower)
```

```
## Error: object 'corpus' not found
```

```r
corpus <- tm_map(corpus, stripWhitespace)
```

```
## Error: object 'corpus' not found
```

```r
corpus <- tm_map(corpus, removeNumbers)
```

```
## Error: object 'corpus' not found
```

```r
corpus <- tm_map(corpus, removeWords, stopwords("english"))
```

```
## Error: object 'corpus' not found
```

```r

aiua.dtm <- DocumentTermMatrix(corpus)
```

```
## Error: object 'corpus' not found
```

```r

aiua.dtm <- as.matrix(aiua.dtm)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for function 'as.matrix': Error: object 'aiua.dtm' not found
```

```r

# For word cloud
v <- sort(rowSums(t(aiua.dtm)), decreasing = TRUE)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for function 'rowSums': Error in t(aiua.dtm) : 
##   error in evaluating the argument 'x' in selecting a method for function 't': Error: object 'aiua.dtm' not found
```

```r
d <- data.frame(word = names(v), freq = v)
```

```
## Error: object 'v' not found
```

```r
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("./figures/aiua_wordcloud.png", width = 1280, height = 800)
wordcloud(d$word, d$freq, scale = c(8, 0.3), min.freq = 2, max.words = 100, 
    random.order = T, rot.per = 0.15, colors = pal, vfont = c("sans serif", 
        "plain"))
```

```
## Error: object 'd' not found
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r

# Get data in glmnet readible form
x <- as.matrix(aiua.dtm)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for function 'as.matrix': Error: object 'aiua.dtm' not found
```

```r
# First 50 (top ranked) assigned 1, lower ranked 50 assigned 0
y <- ua.dt$wfu
```

```
## Error: object 'ua.dt' not found
```

```r

# Cross-validation cvfit <- cv.glmnet(x,y,nfolds=50,lambda=c(0.0001, 0.001,
# 0.0025, 0.005, 0.01, 0.025, 0.1,
# 0.5),family='binomial',type.measure='class')

# Does k-fold cross-validation for glmnet, produces a plot, and returns a
# value for lambda

cvfit <- cv.glmnet(x, y, nfolds = 10, family = "binomial", type.measure = "class")
```

```
## Error: object 'x' not found
```

```r

pdf("./figures/aiua_follow_up_misclass_v_lambda.pdf")
```

```
## Error: cannot open file './figures/aiua_follow_up_misclass_v_lambda.pdf'
```

```r
plot(cvfit)
```

```
## Error: object 'cvfit' not found
```

```r
dev.off()
```

```
## null device 
##           1
```

```r

best.lambda <- cvfit$lambda.min
```

```
## Error: object 'cvfit' not found
```

```r

# glmfit.test <- cvfit.glmnet.fit1

pdf("./figures/aiua_follow_up_coef_v_lambda.pdf")
```

```
## Error: cannot open file './figures/aiua_follow_up_coef_v_lambda.pdf'
```

```r
plot(glmfit.test, xvar = "lambda")
```

```
## Error: object 'glmfit.test' not found
```

```r
# x=matrix(rnorm(100*20),100,20) y=rnorm(100)
# g2=sample(1:2,100,replace=TRUE) g4=sample(1:4,100,replace=TRUE)
# fit1=glmnet(x,y) predict(fit1,newx=x[1:5,],s=c(0.01,0.005))
# predict(fit1,type='coef') fit2=glmnet(x,g2,family='binomial')
# predict(fit2,type='response',newx=x[2:5,]) predict(fit2,type='nonzero')
# fit3=glmnet(x,g4,family='multinomial')
# predict(fit3,newx=x[1:3,],type='response',s=0.01)

```

