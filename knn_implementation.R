# my_knn <- function(train, train_labels, test=NA, k=1, metric="euclidean")
  


# library(tidyverse)
library(dplyr)
library(philentropy)

cancer_link <- "https://resources.oreilly.com/examples/9781784393908/raw/ac9fe41596dd42fc3877cfa8ed410dd346c43548/Machine%20Learning%20with%20R,%20Second%20Edition_Code/Chapter%2003/wisc_bc_data.csv"
bcd <- read.csv(cancer_link)


my_knn <- function(train_data, train_labels, test=NA, k=7, metric="euclidean", normalize = TRUE){
  if(normalize){
    train_data = as.data.frame(lapply(train_data, scale, center = TRUE, scale = TRUE))
  }
  
  if(is.na(test)){
    test = train_data
  }
  
  prediction = vector("logical", length(test[,1]))
  for(i in 1:nrow(test)){
    distances = vector("logical", length(train_data[,1]))
    labels = vector("logical", length(train_data[,1]))
    sample = test[i,]
    
    for (j in 1:nrow(train_data)){
      sample_dist = distance(rbind(sample, train_data[j,]), method = metric)
      distances[[j]] = sample_dist[1]
      # labels[[j]] = train_labels[j] # dont need
    }
    
    Vectorize(function(x, y, method){
      return(distance(rbind(x, y), method))
    })
    
        
    results <- as.data.frame(cbind(as.vector(distances), as.vector(I(labels))))
    colnames(results) <- c("Distances", "Labels")
    
    # print(results$Distances)
    results = results[order(results$Distances),]
    k_results = results[1:k,]
    
    count = k_results %>% count(Labels)
    
    predicted_category = count$Labels[which.max(count$n)]
    
    prediction[i] = predicted_category
  }
  print(prediction)
  return(prediction)
}


my_dist <- Vectorize(function(x, y, method){
  return(distance(rbind(x, y), method))
})

# prediction = my_knn(bcd[3:(length(bcd)-1)], bcd$diagnosis, metric = "euclidean")

prediction = my_knn(iris[1:(length(iris)-1)], iris$Species)

pred_diag <- as.data.frame(cbind(prediction, bcd$diagnosis))

colnames(pred_diag) <- c("prediction", "labels")

(pred_diag %>% filter(prediction == labels) %>% count()) / dim(pred_diag[,0])





Vectorize(function(x, y, method){
  return(distance(rbind(x, y), method))
})

train_data = iris[1:(length(iris)-1)]
test = train_data

s <- data.frame(cbind(train_data$Sepal.Length, test$Sepal.Length))
outer(s[[1]], s[[2]], Vectorize(function(x, y, method) distance(rbind(x, y), method)))

