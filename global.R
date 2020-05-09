library(tidyverse)
library(shiny)
library(ggplot2)
library(GEOquery)
library(R.utils)
library(reshape2)
library(ggplot2)
library(limma)
library(dplyr)
library(plotly)
library(ggpubr)

clinical_outcome <-getGEO("GSE120396")
clinical_outcome<- clinical_outcome$GSE120396_series_matrix.txt.gz

rejection_status  <- clinical_outcome$characteristics_ch1.1
rejection_status <- unlist(lapply( strsplit(as.character(rejection_status), ": " ) , `[[` , 2)  )

gse <- read.csv("GSE120396_expression_matrix.txt",header = TRUE,row.names = 'X')

create_fit <- function(){
  groupname <- factor(rejection_status)
  design <- model.matrix(~ groupname +0)

  fit <- lmFit(gse, design)
  cont.matrix <- makeContrasts(groupnameYes-groupnameNo, levels=design)
  fit2 <- contrasts.fit(fit, cont.matrix)
  fit2 <- eBayes(fit2)
  return(fit2)
}

F1score <- function(mat) {
  TN <- mat[2,2]
  FP <- mat[1,2]
  TP <- mat[1,1]
  FN <- mat[2,1]
  return(2*TP/(2*TP+FP+FN))
}

fs <- function(n,fit){
  df_f <- topTable(fit, number=n, genelist=rownames(gse))
  keep =c()
  for (i in 1:nrow(df_f)){
    keep = c(keep,rownames(df_f)[i])
  }
  idx<-which(rownames(gse) %in% keep)
  gse_f <- gse[idx,]
}

get_df <- function(s,model,m1,n_sim){
  # s <- seq(10,n,1)
  fit_mod = create_fit()
  c1 = c2 = c3 =c()
  for (x in 10:s){
    fs1<-fs(x,fit_mod)
    X <- as.matrix(t(fs1))
    y = rejection_status
    cvK = 5  # number of CV folds
    cv_50acc5 = cv_50_f = c()
    cv_acc = c()
    for (i in 1:n_sim) {
      cvSets = cvTools::cvFolds(nrow(X), cvK)
      cv_acc = cv_acck = c()

      for (j in 1:cvK) {
        test_id = cvSets$subsets[cvSets$which == j]
        X_test = X[test_id, ]
        X_train = X[-test_id, ]
        y_test = y[test_id]
        y_train = y[-test_id]

        mod <- model(x = X_train, y = as.factor(y_train))
        fit <- predict(mod, X_test)
        # mat = table(fit,y_test)
        cv_acck[j] = F1score(table(factor(fit,levels = c('No','Yes')),factor(y_test,levels = c('No','Yes'))))
        # cv_acck[j] = F1score(mat)
        cv_acc[j] = table(fit, y_test) %>% diag %>% sum %>% `/`(length(y_test))

      }
      cv_50acc5 <- append(cv_50acc5, mean(cv_acc))
      cv_50_f <- append(cv_50_f,mean(cv_acck))

    }
    c1 <- append(c1,mean(cv_50acc5))



    if(x == (s)){
      c2= append(c2,cv_50acc5)
      c3= append(c3,cv_50_f)
    }
  }
  # line_df$n <<- s
  line_df[m1] <<- c1
  box_df[m1] <<- c2
  f1_df[m1]<<- c3
  return(fs1)
}

map_mod<- function(v){
  if(length(v)==1){
    if(v ==1){
      m <- randomForest::randomForest
      ms<-'Random Froest'
      return(list(m,ms))
    }else if(v ==2){
      m <- e1071::svm
      ms <- 'SVM'
      return(list(m,ms))
    }else {
      m <- class::knn
      ms <- 'KNN'
      return(list(m,ms))
    }
  }
  if (length(v)==2){
    for(i in 1:2){
      if(v[1]==1 ){
        m<-randomForest::randomForest
        ms<-'Random Froest'
      }else if(v[1]==2){
        m<-e1071::svm
        ms <- 'SVM'
      }else if(v[1]==3){
        m<-class::knn
        ms <- 'KNN'
      }
      if(v[2]==1 ){
        m1<-randomForest::randomForest
        ms1<-'Random Froest'
      }else if(v[2]==2){
        m1<-e1071::svm
        ms1 <- 'SVM'
      }else if(v[2]==3){
        m1<-class::knn
        ms1 <- 'KNN'
      }
    }
    return(list(m,ms,m1,ms1))
  }
}