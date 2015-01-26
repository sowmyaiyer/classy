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
sampleCount <- 7
library(doMC)
registerDoMC(cores=sampleCount)
# setup libs
# install if needed from http://stackoverflow.com/a/4090208
list.of.packages <- c(
    "e1071",
    "ROCR", # http://cran.r-project.org/web/packages/ROCR/index.html
    "doParallel",
    "foreach",
    "gbm",
    "randomForest"
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
testingData <- args[4]

outputPdf <- NA
auc_out_file <- NA
if (length(args) != 4)
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
observations <- read.table(training_data, header = TRUE, sep="\t")
observations <- data.frame(id=observations[,1],observations[,-1])
rownames(observations) <- observations$id
# set.seed(42)

trainData <- observations
testData <- read.table(testingData,header = TRUE, sep="\t")

# Take 20% random sample of the data for testing, 80% for training
##testIndexes <- sample(1:nrow(observations), size=0.2*nrow(observations))
##testData <- observations[testIndexes,]
##trainData <- observations[-testIndexes,]

# Create random bootstrap training samples (with replacement) in parallel 
trainSamples <- foreach(i = 1:sampleCount) %dopar% {
#		    set.seed(i)
		    sample_indices <- sample(1:nrow(trainData), size=0.3*nrow(trainData), replace=TRUE)
			cat(sample_indices[1:10],"\n")
                    trainData[sample_indices,] 
                } 
#-------------------------------------------------------------------------------
#-----------------------Create Different Bootstrap Models in Parallel-----------
#-------------------------------------------------------------------------------

modelDataRandomForest <- foreach(i = 1:sampleCount) %dopar% {
			randomForest(x=as.matrix(trainSamples[[i]][,c(-1,-ncol(trainSamples[[i]]))]), y=as.factor(trainSamples[[i]]$label), ntree=1000, importance=TRUE)
			}
cat("done randomForest\n")			

modelDataGbm <- foreach(i = 1:sampleCount) %dopar% {
                        gbm(label ~ ., data=trainSamples[[i]][,-1], n.trees=1000, distribution = "bernoulli",interaction.depth=4,cv.folds=0)
                        }
cat("done gbm\n")
#-------------------------------------------------------------------------------
#-----------------------Predict the Test Data in Parallel Using Each Model------
#-------------------------------------------------------------------------------
predictions <- list()

predictDataRandomForest <- foreach(i = 1:sampleCount) %dopar% {
                    	predict(modelDataRandomForest[[i]], testData[,c(-1,-ncol(testData))]
                          , type="prob")
		   }
predictions[["randomForest"]] <- predictDataRandomForest
print(predictions[["randomForest"]])
predictDataGbm <- foreach(i = 1:sampleCount) %dopar% {
                    predict(modelDataGbm[[i]], testData[,c(-1,-ncol(testData))]
                          , probability=TRUE, n.trees=1000,type="response")
                   }
predictions[["gbm"]] <- predictDataGbm
print(predictDataGbm[[1]])
#-------------------------------------------------------------------------------
#-----------------------Rank Each Model's Bootstrap Data-------------------------
#-------------------------------------------------------------------------------
rankPredictData <- function(predictData, getPofG, rankDataObject=NULL) 
{
	rankData <- rankDataObject
	cat(getPofG,"\n")
    #Rank the test data's probability of g for each model
    rankCols <- foreach(i = 1:length(predictData)) %dopar% {
                    if (getPofG == "randomForest") {
                        Pg <- predictData[[i]][,2]
                        g  <- ifelse(predictData[[i]][,"1"] >= predictData[[i]][,"0"],1,0)
                    } else if (getPofG == "gbm") {
                        Pg <- predictData[[i]]
                        #g  <- ifelse(Pg <=0,0,1)
                        g  <- ifelse(Pg <=0.5,0,1)
                    }
                    #data.frame(gVote=g, rankG=rank(Pg))
                    data.frame(gVote=g, scoreG=Pg)
                }
    #convert list into one data frame
    colnames(rankCols[[1]])[1] <- "gVote"
    colOffset<-ifelse(is.null(rankData),0,ncol(rankData)-1)
    newRankData <- rankCols[[1]]
    colnames(newRankData)[2] <- paste("rank",colOffset + 1,sep='')
    for(i in 2:length(rankCols))
    {
        #newRankData <- data.frame(newRankData, rankCols[[i]]$rankG)
        newRankData <- data.frame(newRankData, rankCols[[i]]$scoreG)
        colnames(rankCols[[i]])[1] <- "gVote"
        newRankData$gVote <- newRankData$gVote + rankCols[[i]]$gVote
        colnames(newRankData)[i+1] <- paste("rank",i + colOffset,sep="")
    }
    rankData <- newRankData
    rankData$gVote <- rankData$gVote + newRankData$gVote
    #create final ranking by summing all ranks and then sort DESC
    rankData$finalRank <- apply(rankData[,-1],1,mean)
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
rankTables[["randomForest"]] <- rankPredictData(predictions[["randomForest"]], "randomForest", NULL)
write.table(rankTables[["randomForest"]], file="rankTable_randomForest",,quote=FALSE,row.names=FALSE,sep="\t")
rankTables[["gbm"]] <- rankPredictData(predictions[["gbm"]], "gbm", NULL)
write.table(rankTables[["gbm"]], file="rankTable_gbm",,quote=FALSE,row.names=FALSE,sep="\t")

