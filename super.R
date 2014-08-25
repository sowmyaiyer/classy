#*********************************************************************************************************
# © 2013 jakemdrew.com. All rights reserved. 
# This source code is licensed under The GNU General Public License (GPLv3):  
# http://opensource.org/licenses/gpl-3.0.html
#*********************************************************************************************************

source("/home/si14w/my/performance.R")

#-------------------------------------------------------------------------------
#-----------------------Setup Parallel Processing-------------------------------
#-------------------------------------------------------------------------------

#number of bootstrap samples to create
sampleCount <- 10
library(doMC)
registerDoMC(cores=sampleCount)
# setup libs
# install if needed from http://stackoverflow.com/a/4090208
list.of.packages <- c(
    "e1071",
    "ROCR", # http://cran.r-project.org/web/packages/ROCR/index.html
    "glmnet", # http://cran.r-project.org/web/packages/glmnet/glmnet.pdf
    "doParallel",
    "foreach",
    "gbm",
    "gam",
    "randomForest",
    "mboost"
    )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, contriburl="http://cran.us.r-project.org")

#load libs
lib_packages <- lapply(list.of.packages, function(lib){
 library(lib, character.only=TRUE, quietly=TRUE)
  })

#-------------------------------------------------------------------------------
#-----------------------Create Random Sample Test and Training Data-------------
#-------------------------------------------------------------------------------
args <- commandArgs(TRUE)
training_data <- args[1]   
outtype <- args[2]   
outfile <- args[3]

outputPdf <- NA
auc_out_file <- NA
if (length(args) != 3)
{
	stop("Usage : Rscript super.R <tab-separated training_data master table> <output type (PDF/raw)> <outputFileName>")
}
if (outtype == "PDF")
{
	outputPdf <- outfile
} else if (outtype == "raw")
{
	auc_out_file <- outfile
} else {
	stop("Usage : Rscript super.R <tab-separated training_data master table> <output type (PDF/raw)> <outputFileName>")
}
observations = read.table(training_data, header = TRUE, sep="\t")
observations <- data.frame(id=observations[,1],observations[,-1])

# Take 20% random sample of the data for testing, 80% for training
testIndexes <- sample(1:nrow(observations), size=0.2*nrow(observations))
testData <- observations[testIndexes,]
trainData <- observations[-testIndexes,]

# Create random bootstrap training samples (with replacement) in parallel 
trainSamples <- foreach(i = 1:sampleCount) %dopar% {
		    sample_indices <- sample(1:nrow(trainData), size=0.2*nrow(trainData), replace=TRUE)
                    trainData[sample_indices,] 
                } 
#-------------------------------------------------------------------------------
#-----------------------Create Different Bootstrap Models in Parallel-----------
#-------------------------------------------------------------------------------

#-----------------------svm-----------------------

modelDataGlmNoReg <- foreach(i = 1:sampleCount) %dopar% {
                  glm(label ~ ., data=trainSamples[[i]][,-1],family="binomial")
			}
modelDataGlm <- foreach(i = 1:sampleCount) %dopar% {
                  glmnet(x=as.matrix(trainSamples[[i]][,c(-1,-ncol(trainSamples[[i]]))]), y=as.factor(trainSamples[[i]]$label), family="binomial", alpha=1)
			}

modelDataSvm <- foreach(i = 1:sampleCount) %dopar% {
                  svm(x=trainSamples[[i]][,c(-1,-ncol(trainSamples[[i]]))], y=as.factor(trainSamples[[i]]$label), probability=TRUE, cost=10^-5, gamma=0.1, kernel="linear")
			}
modelDataGbm <- foreach(i = 1:sampleCount) %dopar% {
			#gbm(label ~ ., data=trainSamples[[i]][,-1], n.trees=1000, distribution = "bernoulli")
			gbm(label ~ ., data=trainSamples[[i]][,-1], n.trees=1000, distribution = "adaboost")
			}
modelDataGam <- foreach(i = 1:sampleCount) %dopar% {
			gam(label ~ ., data=trainSamples[[i]][,-1], family=binomial(link = "logit"))
			}
modelDataGamBoost <- foreach(i = 1:sampleCount) %dopar% {
			gamboost(label ~ ., data=trainSamples[[i]][,-1])
			}

modelDataNB <- foreach(i = 1:sampleCount) %dopar% {
			naiveBayes(label ~ ., data=trainSamples[[i]][,-1])
			}
modelDataRandomForest <- foreach(i = 1:sampleCount) %dopar% {
			randomForest(x=as.matrix(trainSamples[[i]][,c(-1,-ncol(trainSamples[[i]]))]), y=as.factor(trainSamples[[i]]$label), ntree=500)
			}
#-------------------------------------------------------------------------------
#-----------------------Predict the Test Data in Parallel Using Each Model------
#-------------------------------------------------------------------------------

predictDataGlmNoReg <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataGlmNoReg[[i]], newdata=as.data.frame(testData[,c(-1 ,-ncol(testData))]), type="response")
                }                

predictDataGlm <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataGlm[[i]], newx=as.matrix(testData[,c(-1 ,-ncol(testData))]), type="response", s=cv.glmnet(x=as.matrix(trainSamples[[i]][,c(-1,-ncol(trainSamples[[i]]))]), y=as.factor(trainSamples[[i]]$label),
                        family="binomial",
                        type.measure="auc",
                        alpha=1
                        )$lambda.min) 
                }                
predictDataSvm <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataSvm[[i]], testData[,c(-1,-ncol(testData))]
                          , probability=TRUE)
			}
predictDataGbm <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataGbm[[i]], testData[,c(-1,-ncol(testData))]
                          , probability=TRUE, n.trees=1000)
		   }


predictDataRandomForest <- foreach(i = 1:sampleCount) %dopar% {
                    	predict(modelDataRandomForest[[i]], testData[,c(-1,-ncol(testData))]
                          , type="prob")
		   }
predictDataNB <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataNB[[i]], testData[,c(-1,-ncol(testData))]
                          , type="raw")
		   }
predictDataGam <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataGam[[i]], testData[,c(-1,-ncol(testData))]
                          , type="response")
			}
predictDataGamBoost <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataGamBoost[[i]], testData[,c(-1,-ncol(testData))]
                          , type="response")
			}

#-------------------------------------------------------------------------------
#-----------------------Rank Each Model's Bootstrap Data-------------------------
#-------------------------------------------------------------------------------
rankPredictData <- function(predictData, getPofG, rankDataObject=NULL,reverseRank=FALSE, addFinalRank=FALSE){
    rankData <- rankDataObject
    #Rank the test data's probability of g for each model
    rankCols <- foreach(i = 1:length(predictData)) %dopar% {
                    if(getPofG == "svm"){
                        Pg <- attr(predictData[[i]], "probabilities")[,c("1")]
#                        Pg <-  unlist(predictData)
                        g  <- ifelse(Pg >= 0.5,1,0)
                    } else if (getPofG == "naiveBayes" | getPofG == "randomForest"){
                        Pg <- predictData[[i]][,2]
                        g  <- ifelse(predictData[[i]][,"1"] >= predictData[[i]][,"0"],1,0)
                    } else if (getPofG == "gbm"){
                        Pg <- predictData[[i]]
                        g  <- ifelse(Pg <=0,0,1)
                    } else if (getPofG == "glmnet" | getPofG == "glm" | getPofG == "gam" | getPofG == "gamboost") {
			Pg <- predictData[[i]]
			g  <- ifelse(Pg >= 0.5,1,0)
		    } else {
			stop("unhandled method in rankPredictData")
		    } 
                    data.frame(gVote=g, rankG=rank(Pg))
                }
    #convert list into one data frame
    colnames(rankCols[[1]])[1] <- "gVote"
    colOffset<-ifelse(is.null(rankData),0,ncol(rankData)-1)
    newRankData <- rankCols[[1]]
    colnames(newRankData)[2] <- paste("rank",colOffset + 1,sep='')
    for(i in 2:length(rankCols)){
        newRankData <- data.frame(newRankData, rankCols[[i]]$rankG)
	colnames(rankCols[[i]])[1] <- "gVote"
        newRankData$gVote <- newRankData$gVote + rankCols[[i]]$gVote
        colnames(newRankData)[i+1] <- paste("rank",i + colOffset,sep="")
    }
    #Combine any previous rankData, if provided
    if(is.null(rankData)){ 
	rankData <- newRankData
    } else {
        rankData <- data.frame(rankData,newRankData[,-1])
        rankData$gVote <- rankData$gVote + newRankData$gVote 
    }
    if(addFinalRank){
        #create final ranking by summing all ranks and then sort DESC
        rankData$finalRank <- apply(rankData[,-1],1,sum)      
        #add ground truth class column to output
        rankData <- data.frame(id=testData$id,rankData,actualClass=testData[,ncol(testData)])
        #Sort by the rank of class g
        rankData <- rankData[with(rankData, order(-finalRank)), ]
        #add predicted class column to output
        rankData$predictedClass <- ifelse(rankData$gVote >= ((ncol(rankData)-1) / 2),"1","0")
    }
    return(rankData)
}

#-----------------------Create the final ranking tables------------------- 
rankGlm <- rankPredictData(predictDataGlm,"glmnet",NULL,FALSE,TRUE)
rankSvm <- rankPredictData(predictDataSvm,"svm",NULL,FALSE,TRUE)
rankGbm <- rankPredictData(predictDataGbm,"gbm",NULL,FALSE,TRUE)
rankGam <- rankPredictData(predictDataGam,"gam",NULL,FALSE,TRUE)
rankGamBoost <- rankPredictData(predictDataGamBoost,"gamboost",NULL,FALSE,TRUE)
rankNaiveBayes <- rankPredictData(predictDataNB,"naiveBayes",NULL,FALSE,TRUE)
rankRandomForest <- rankPredictData(predictDataRandomForest,"randomForest",NULL,FALSE,TRUE)
rankGlmNoReg <- rankPredictData(predictDataGlmNoReg,"glm",NULL,FALSE,TRUE)



pred_glm <- prediction(predictions=rankGlm$finalRank,labels=rankGlm$actualClass)
pred_svm <- prediction(predictions=rankSvm$finalRank,labels=rankSvm$actualClass)
pred_gbm <- prediction(predictions=rankGbm$finalRank,labels=rankGbm$actualClass)
pred_gam <- prediction(predictions=rankGam$finalRank,labels=rankGam$actualClass)
pred_gamboost <- prediction(predictions=rankGamBoost$finalRank,labels=rankGamBoost$actualClass)
pred_nb <- prediction(predictions=rankNaiveBayes$finalRank,labels=rankNaiveBayes$actualClass)
pred_rf <- prediction(predictions=rankRandomForest$finalRank,labels=rankRandomForest$actualClass)
pred_glm_noreg <- prediction(predictions=rankGlmNoReg$finalRank,labels=rankGlmNoReg$actualClass)


perf_auc_glm <- performance(pred_glm, measure="auc", x.measure="fpr")@y.values[[1]][1]
perf_auc_svm <- performance(pred_svm, measure="auc", x.measure="fpr")@y.values[[1]][1]
perf_auc_gbm <- performance(pred_gbm, measure="auc", x.measure="fpr")@y.values[[1]][1]
perf_auc_gam <- performance(pred_gam, measure="auc", x.measure="fpr")@y.values[[1]][1]
perf_auc_gamboost <- performance(pred_gamboost, measure="auc", x.measure="fpr")@y.values[[1]][1]
perf_auc_nb <- performance(pred_nb, measure="auc", x.measure="fpr")@y.values[[1]][1]
perf_auc_rf <- performance(pred_rf, measure="auc", x.measure="fpr")@y.values[[1]][1]
perf_auc_glm_noreg <- performance(pred_glm_noreg, measure="auc", x.measure="fpr")@y.values[[1]][1]


if (!is.na(outputPdf))
{
	pdf(outputPdf)
	pred_glm <- prediction(predictions=rankGlm$finalRank,labels=rankGlm$actualClass)
	pred_svm <- prediction(predictions=rankSvm$finalRank,labels=rankSvm$actualClass)
	pred_gbm <- prediction(predictions=rankGbm$finalRank,labels=rankGbm$actualClass)
	pred_gam <- prediction(predictions=rankGam$finalRank,labels=rankGam$actualClass)
	pred_gamboost <- prediction(predictions=rankGamBoost$finalRank,labels=rankGamBoost$actualClass)
	pred_nb <- prediction(predictions=rankNaiveBayes$finalRank,labels=rankNaiveBayes$actualClass)
	pred_glm_noreg <- prediction(predictions=rankGlmNoReg$finalRank,labels=rankGlmNoReg$actualClass)
	pred_rf <- prediction(predictions=rankRandomForest$finalRank,labels=rankRandomForest$actualClass)
	
	plot(performance(pred_glm, measure="tpr", x.measure="fpr"), main="", cex.main=0.6, bty="n", col="red")
	par(new=TRUE)
	plot(performance(pred_svm, measure="tpr", x.measure="fpr"), main="", cex.main=0.6, bty="n", col="blue", xlab="",ylab="", axes=FALSE)
	par(new=TRUE)
	plot(performance(pred_gbm, measure="tpr", x.measure="fpr"), main="", cex.main=0.6, bty="n", col="green", xlab="",ylab="", axes=FALSE)
	par(new=TRUE)
	plot(performance(pred_gam, measure="tpr", x.measure="fpr"), main="", cex.main=0.6, bty="n", col="hotpink", xlab="",ylab="", axes=FALSE)
	par(new=TRUE)
	plot(performance(pred_gamboost, measure="tpr", x.measure="fpr"), main="", cex.main=0.6, bty="n", col="black", xlab="",ylab="", axes=FALSE)
	par(new=TRUE)
	plot(performance(pred_nb, measure="tpr", x.measure="fpr"), main="", cex.main=0.6, bty="n", col="purple", xlab="",ylab="", axes=FALSE)
	par(new=TRUE)
	plot(performance(pred_rf, measure="tpr", x.measure="fpr"), main="", cex.main=0.6, bty="n", col="orange", xlab="",ylab="", axes=FALSE)
	par(new=TRUE)
	plot(performance(pred_glm_noreg, measure="tpr", x.measure="fpr"), main="", cex.main=0.6, bty="n", col="brown", xlab="",ylab="", axes=FALSE)
	
	perf_auc_glm <- paste("glmnet: ",round(performance(pred_glm, measure="auc", x.measure="fpr")@y.values[[1]][1], digits=3), sep="")
	perf_auc_svm <- paste("svm: ",round(performance(pred_svm, measure="auc", x.measure="fpr")@y.values[[1]][1], digits=3), sep="")
	perf_auc_gbm <- paste("gbm: ",round(performance(pred_gbm, measure="auc", x.measure="fpr")@y.values[[1]][1], digits=3), sep="")
	perf_auc_gam <- paste("gam: ",round(performance(pred_gam, measure="auc", x.measure="fpr")@y.values[[1]][1], digits=3), sep="")
	perf_auc_gamboost <- paste("gamboost: ",round(performance(pred_gamboost, measure="auc", x.measure="fpr")@y.values[[1]][1], digits=3), sep="")
	perf_auc_nb <- paste("naiveBayes: ",round(performance(pred_nb, measure="auc", x.measure="fpr")@y.values[[1]][1], digits=3), sep="")
	perf_auc_rf <- paste("randomForest: ",round(performance(pred_rf, measure="auc", x.measure="fpr")@y.values[[1]][1], digits=3), sep="")
	perf_auc_glm_noreg <- paste("glm: ",round(performance(pred_glm_noreg, measure="auc", x.measure="fpr")@y.values[[1]][1], digits=3), sep="")
	legend("bottomright",c(perf_auc_glm,perf_auc_svm,perf_auc_gbm,perf_auc_gam, perf_auc_gamboost,perf_auc_nb,perf_auc_rf,perf_auc_glm_noreg), cex=0.6, bty="n", lty="solid", col=c("red","blue","green","hotpink","black","purple","orange","brown"))
	dev.off()
} else {
	row <- c(perf_auc_glm,perf_auc_svm,perf_auc_gbm,perf_auc_nb,perf_auc_rf, perf_auc_glm_noreg)
	write(row, file=paste("/home/si14w/my/auc_", auc_out_file,sep=""), append=TRUE, sep="\t")
}

