#*********************************************************************************************************
# © 2013 jakemdrew.com. All rights reserved. 
# This source code is licensed under The GNU General Public License (GPLv3):  
# http://opensource.org/licenses/gpl-3.0.html
#*********************************************************************************************************

source("/home/si14w/TR/classy/performance.R")

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
#set.seed(42)
# Take 20% random sample of the data for testing, 80% for training
testIndexes <- sample(1:nrow(observations), size=0.2*nrow(observations))
testData <- observations[testIndexes,]
trainData <- observations[-testIndexes,]

# Create random bootstrap training samples (with replacement) in parallel 
trainSamples <- foreach(i = 1:sampleCount) %dopar% {
		    #set.seed(i)
		    sample_indices <- sample(1:nrow(trainData), size=0.3*nrow(trainData), replace=TRUE)
			cat(sample_indices[1:10],"\n")
                    trainData[sample_indices,] 
                } 
#-------------------------------------------------------------------------------
#-----------------------Create Different Bootstrap Models in Parallel-----------
#-------------------------------------------------------------------------------

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
			cat(i,"\n")
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
			randomForest(x=as.matrix(trainSamples[[i]][,c(-1,-ncol(trainSamples[[i]]))]), y=as.factor(trainSamples[[i]]$label), ntree=500, importance=TRUE)
			}
#-------------------------------------------------------------------------------
#-----------------------Predict the Test Data in Parallel Using Each Model------
#-------------------------------------------------------------------------------
predictions <- list()

predictDataGlmNoReg <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataGlmNoReg[[i]], newdata=as.data.frame(testData[,c(-1 ,-ncol(testData))]), type="response")
                }                

predictions[["glm"]] <- predictDataGlmNoReg
predictDataGlm <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataGlm[[i]], newx=as.matrix(testData[,c(-1 ,-ncol(testData))]), type="response", s=cv.glmnet(x=as.matrix(trainSamples[[i]][,c(-1,-ncol(trainSamples[[i]]))]), y=as.factor(trainSamples[[i]]$label),
                        family="binomial",
                        type.measure="auc",
                        alpha=1
                        )$lambda.min) 
                }                
predictions[["glmnet"]] <- predictDataGlm
predictDataSvm <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataSvm[[i]], testData[,c(-1,-ncol(testData))]
                          , probability=TRUE)
			}
predictions[["svm"]] <- predictDataSvm
predictDataGbm <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataGbm[[i]], testData[,c(-1,-ncol(testData))]
                          , probability=TRUE, n.trees=1000)
		   }
predictions[["gbm"]] <- predictDataGbm

predictDataRandomForest <- foreach(i = 1:sampleCount) %dopar% {
                    	predict(modelDataRandomForest[[i]], testData[,c(-1,-ncol(testData))]
                          , type="prob")
		   }

predictions[["randomForest"]] <- predictDataRandomForest

predictDataNB <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataNB[[i]], testData[,c(-1,-ncol(testData))]
                          , type="raw")
		   }

predictions[["naiveBayes"]] <- predictDataNB
predictDataGam <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataGam[[i]], testData[,c(-1,-ncol(testData))]
                          , type="response")
			}
predictions[["gam"]] <- predictDataGam

predictDataGamBoost <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataGamBoost[[i]], testData[,c(-1,-ncol(testData))]
                          , type="response")
			}
predictions[["gamboost"]] <- predictDataGamBoost

#-------------------------------------------------------------------------------
#-----------------------Rank Each Model's Bootstrap Data-------------------------
#-------------------------------------------------------------------------------
rankPredictData <- function(predictData, getPofG, rankDataObject=NULL){
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
    rankData <- newRankData
    rankData$gVote <- rankData$gVote + newRankData$gVote
    #create final ranking by summing all ranks and then sort DESC
    rankData$finalRank <- apply(rankData[,-1],1,sum)
    #add ground truth class column to output
    rankData <- data.frame(id=testData$id,rankData,actualClass=testData[,ncol(testData)])
    #Sort by the rank of class g
#    rankData <- rankData[with(rankData, order(-finalRank)), ]
    #add predicted class column to output
    rankData$predictedClass <- ifelse(rankData$gVote >= ((ncol(rankData)-1) / 2 ),"1","0")

    return(rankData)
}

#-----------------------Create the final ranking tables------------------- 

rankTables <- list()
pred <- list()
perf_auc <- list()
#methods <- c("glmnet","gbm","gamboost","randomForest")
methods <- c("glmnet","svm","gbm","gam","gamboost","naiveBayes","randomForest","glm")
rankTables[[methods[1]]] <- rankPredictData(predictions[[methods[1]]], methods[1], NULL)
ensembleRankData <- data.frame(rankTables[[methods[1]]][,c("id","gVote","finalRank","actualClass")])
pred[[methods[1]]] <- prediction(predictions=rankTables[[methods[1]]]$finalRank,labels=rankTables[[methods[1]]]$actualClass)
perf_auc[[methods[1]]] <- performance(pred[[methods[1]]], measure="auc", x.measure="fpr")@y.values[[1]][1]
for (c in 2:length(methods))
{
	method <- methods[c]
	rankTables[[method]] <- rankPredictData(predictions[[method]],method, NULL)
	pred[[method]] <- prediction(predictions=rankTables[[method]]$finalRank,labels=rankTables[[method]]$actualClass)
	perf_auc[[method]] <- performance(pred[[method]], measure="auc", x.measure="fpr")@y.values[[1]][1]
	ensembleRankData$gVote <- ensembleRankData$gVote + rankTables[[method]]$gVote
        ensembleRankData$finalRank <- ensembleRankData$finalRank + rankTables[[method]]$finalRank
}

numberOfRanks  <- sampleCount*length(methods)
ensembleRankData$predictedClass <- ifelse(ensembleRankData$gVote >= (numberOfRanks/2), "1","0")
pred[["ensemble"]] <- prediction(predictions=ensembleRankData$finalRank,labels=ensembleRankData$actualClass)
perf_auc[["ensemble"]] <- performance(pred[["ensemble"]], measure="auc", x.measure="fpr")@y.values[[1]][1]
cat(unlist(perf_auc),"\n")
if (outtype=="PDF" & !is.na(outputPdf))
{
	pdf(outputPdf)
	mycols <- c("red","blue","green","hotpink","black","purple","orange","brown")
	
	legend_auc <- list()
	for (i in 1:length(methods))
	{
		method <- methods[i]
		plot(performance(pred[[method]], measure="tpr", x.measure="fpr"), main="", cex.main=0.6, bty="n", col=mycols[i])
		par(new=TRUE)
		legend_auc[[method]] <- paste(method, round(perf_auc[[method]], digits=3))	
	}
	plot(performance(pred[["ensemble"]],  measure="tpr", x.measure="fpr"), main="", cex.main=0.6, bty="n", col="black", lty="dashed")
	legend("bottomright", c(unlist(legend_auc), paste("ensemble ",round(perf_auc[["ensemble"]],digits=3))),  cex=0.6, bty="n", lty="solid", col=c("red","blue","green","hotpink","black","purple","orange","brown"))
	dev.off()
} else {
	row <- t(as.matrix(unlist(perf_auc)))
	write.table(row, file=auc_out_file, append=TRUE, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE)
}

