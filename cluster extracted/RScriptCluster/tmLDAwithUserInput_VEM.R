#topic clustering using LDA VEM with user input
set.seed(100)
library(tm)
cat("--Enter the address of directory where .txt docs to be clustered are located --\n")
cat("--For clustering sample docs, just provide address of folder sampleDocs --   \n ")
cat("\n")
x <- readLines("stdin", 1)
cat(" \n--How many cluster do you want? --\n")
cat("\n")
k <- as.numeric(readLines("stdin", 1))
#setwd("C:\\Users\\sajan\\Documents\\TextMining")
setwd(x)
filenames <- list.files(getwd(), pattern="*.txt", full.names=F,include.dirs = F , recursive=TRUE)
docs=Corpus(DirSource(directory = getwd(), pattern = "*.txt", recursive = TRUE, mode = "text"))

old <- Sys.time() 

#setwd("C:\\Users\\sajan\\Documents\\TextMining")
#filenames <- list.files(getwd(),pattern="*.txt")
#files <- lapply(filenames,readLines)
#docs <- Corpus(VectorSource(files))
#writeLines(as.character(docs[[30]]))
docs <-tm_map(docs,content_transformer(tolower))

toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "’")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, "•")

docs <- tm_map(docs, toSpace, "”")
docs <- tm_map(docs, toSpace, "“")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs,stemDocument)
docs <- tm_map(docs, content_transformer(gsub), pattern = "team-", replacement = "team")
myStopwords <- c("can", "say","one","way","use", "also","howev","tell","will","went","dont", "didnt" ,"much","need","take","tend","even", "like","particular","rather","said", "get","well","make","ask","come","end", "first","two","help","often","may", "might","see","someth","thing","point", "post","look","right","now","think","’ve ", "’re ")
docs <- tm_map(docs, removeWords, myStopwords)
docs <- tm_map(docs, removeWords, stopwords("english"))
#writeLines(as.character(docs[[30]]))

cat("\n  --Wait, the documents are being clustered\n ")

dtm <- DocumentTermMatrix(docs)
m <- as.matrix(dtm)
#write.csv(m,file=paste("dtm_for_each",k,"docs.csv"))
rownames(dtm) <- filenames

freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq,decreasing=TRUE)

#freq[ord]
#write.csv(freq[ord],file=paste("word_freq_", k ,"_total.csv"))
#using Topic Modelling
library(topicmodels)

#VEM cluster method

cat("\n  --Wait, the documents are being clustered\n ")
ldaCluster <-LDA(dtm,k, method = "VEM")


ldaCluster.topics <- as.matrix(topics(ldaCluster))
write.csv(ldaCluster.topics,file=paste("LDA_VEM DOCs to ", k ," Cluster.csv"))


ldaCluster.terms <- as.matrix(terms(ldaCluster,6))
#write.csv(ldaCluster.terms,file=paste("LDA_VEM", k ,"TopicsToTerms.csv"))

#topics name for cluster
write.csv(terms(ldaCluster),file=paste("LDA_VEM Topic names for ", k ," clusters.csv"))

#extra added
#totalDocsNumber=30
#document_Size<-seq(1:totalDocsNumber)
#x <- data.frame(document_Size)
#tmp <- read.csv("LDAGibbs 5 DocsToTopics.csv")
#tmp <- cbind(tmp, x)
#write.csv(tmp, "Only_DocsToTopics.csv")
#data=read.csv("Only_DocsToTopics.csv")


#data=read.csv("LDAGibbs 5 DocsToTopics.csv")


#data=read.csv("LDAGibbs 15 DocsToTopics.csv")
#attach(data)
#plot(1:totalDocsNumber, V1, type='b', axes=TRUE, xlab="Documents", ylab="Topics")
#axis(side=1, at=c(1:totalDocsNumber))
#axis(side=2, at=seq(0, 600, by=100))
#box()
#grid (nx=30,NULL, lty = 6, col = "Red") 

cat("\n  clustering finished,\n ")
newTime <- Sys.time() - old
print(newTime)
x_time=as.character(print(newTime))
time_taken= as.data.frame(x_time, row.names="time taken")
#time_taken= as.data.frame(newTime, row.names("time taken"))

print("The output results are saved as .csv file in ")
print(x)


