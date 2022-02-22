library(stringr)
rm(list = ls())
codes <- read.csv("codes.txt", header=F, sep= " ")
sub <- codes[c(12:15)]
uni_len <- c(2,3,4,7) # unique lengths

# Part 1
length(which(nchar(sub$V12) %in% uni_len)) +
  length(which(nchar(sub$V13) %in% uni_len)) +
  length(which(nchar(sub$V14) %in% uni_len)) +
  length(which(nchar(sub$V15) %in% uni_len))
# 476

# Part 2
answers <- sub
decoder <- codes[c(1:10)]
key <- data.frame(c(seq(0,9)),c(6,2,5,5,4,5,6,3,7,6))
colnames(key) <- c("Digit","Length")
key$has <- NA

for (x in 1:nrow(decoder)) {
  key[2,3] <- decoder[x,][nchar(decoder[x,])==2]
  key[8,3] <- decoder[x,][nchar(decoder[x,])==3]
  key[5,3] <- decoder[x,][nchar(decoder[x,])==4]
  key[9,3] <- decoder[x,][nchar(decoder[x,])==7]
  fourdiff <- Reduce(setdiff, strsplit(c(key[5,3],key[2,3]), split=""))
  top <- Reduce(setdiff, strsplit(c(key[8,3],key[2,3]), split=""))
  match <- which(rowSums(sapply(fourdiff, grepl, decoder[x,]))==2)
  key[6,3] <- decoder[x,][nchar(decoder[x,])==5&rowSums(sapply(fourdiff, grepl, decoder[x,]))==2]
  key[4,3] <- decoder[x,][nchar(decoder[x,])==5&rowSums(sapply(unlist(strsplit(key[2,3], split = "")), grepl, decoder[x,]))==2]
  key[10,3] <- decoder[x,][nchar(decoder[x,])==6&rowSums(sapply(unlist(strsplit(key[5,3], split = "")), grepl, decoder[x,]))==4]
  key[7,3] <- decoder[x,][nchar(decoder[x,])==6&rowSums(sapply(fourdiff, grepl, decoder[x,]))==2&decoder[x,]!=key[10,3]]
  key[3,3] <- decoder[x,][nchar(decoder[x,])==5&decoder[x,]!=key[6,3]&decoder[x,]!=key[4,3]]
  key[1,3] <- decoder[x,][nchar(decoder[x,])==6&decoder[x,]!=key[7,3]&decoder[x,]!=key[10,3]]
  key$ordered <- NA
  for (i in seq_len(nrow(key))) {
    output<-str_sort(unlist(strsplit(key[i,3],split="")))
    key[i,4] <- trimws(paste(output,sep="",collapse=""))
  }
  answers[x,"pos1"] <- key$Digit[paste(paste(str_sort(unlist(strsplit(answers[x,1],split=""))),sep="",colapse=""),collapse = "")==key[,4]]
  answers[x,"pos2"] <- key$Digit[paste(paste(str_sort(unlist(strsplit(answers[x,2],split=""))),sep="",colapse=""),collapse = "")==key[,4]]
  answers[x,"pos3"] <- key$Digit[paste(paste(str_sort(unlist(strsplit(answers[x,3],split=""))),sep="",colapse=""),collapse = "")==key[,4]]
  answers[x,"pos4"] <- key$Digit[paste(paste(str_sort(unlist(strsplit(answers[x,4],split=""))),sep="",colapse=""),collapse = "")==key[,4]]
}
answers$combined<-apply(answers[5:8], 1, function(x) paste(x, collapse = ""))
sum(as.numeric(answers$combined))
# 1011823