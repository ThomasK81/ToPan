setwd("~/OneDrive/GithubProjects/ToPan")
library(wordVectors)
library(tsne)
research_corpus <- readRDS("./www/phi0972.phi001Parsed.rds")

research_corpus <- as.character(research_corpus[,2])
research_corpus <- gsub("[[:punct:]]", "", research_corpus)  # replace punctuation with space
research_corpus <- trimws(research_corpus)
research_corpus <- tolower(research_corpus)
paste(research_corpus, collapse = " ")

write(research_corpus, file = "./www/temptraining.txt")

model = train_word2vec("./www/temptraining.txt",output="temp_vectors.bin",threads = 3,vectors = 200,window=15, force = TRUE)
plot(model)