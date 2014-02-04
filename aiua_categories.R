# Plot most common UA categories
library(ggplot2)
library(tm)
library(RColorBrewer)
library(wordcloud)

data.path <- "./data/aiua.csv"
ua.dt <- read.csv(data.path,sep=",",stringsAsFactors=FALSE)

cats <- seq(6,43)


# cat_ind <- cats[category]

cat_abs_freqs <- colSums(ua.dt[,6:43])
cat_freqs <- cat_abs_freqs/nrow(ua.dt)

names(cat_freqs) <- sapply(colnames(ua.dt)[cats], function(nm) gsub("cat_","",nm))
names(cat_freqs) <- sapply(names(cat_freqs), function(nm) gsub("_"," ",nm))
names(cat_freqs) <- sapply(names(cat_freqs), function(nm) gsub(" s ","s ",nm))

# Print barplot
pdf("./figures/h_aiua_categories.pdf")
op <- par(mar = c(12,4,4,2) + 0.1)
barplot(sort(cat_freqs,decreasing=TRUE), las = 2)
par(op)

dev.off()

create.cat.doc <- function(categories,frequencies)
{
	ret_doc <- c()
	for(i in 1:length(categories))
	{
		ret_doc <- c(ret_doc,paste(rep(categories[i],frequencies[[i]]),collapse=" "))
	}
	return(ret_doc)

}
cat_dtm <- create.cat.doc(names(cat_freqs),cat_abs_freqs)

documents <- data.frame(Text = cat_dtm)
browser()
row.names(documents) <- 1:nrow(documents)
corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'),"amnesty","international","information","human","rights","uan@aiusa.org","(m)","amnestyusa.org/urgent/","()","(),"))

aiua.dtm <- DocumentTermMatrix(corpus)
# testing sparcity

aiua.dtm <- as.matrix(aiua.dtm)


# For word cloud
v <- sort(rowSums(t(aiua.dtm)),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("./figures/category_wordcloud.png", width=1280,height=800)
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()
