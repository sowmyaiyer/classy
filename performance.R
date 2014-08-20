#-------------------------------------------------------------------------------
#-----------------------Create Function To Measure Accuracy---------------------
#-------------------------------------------------------------------------------
accuracy <- function (truth, predicted){
        tTable   <- table(truth,predicted)
	print(tTable)
        tp <- tTable[1,1]
        if(ncol(tTable)>1){ fp <- tTable[1,2] } else { fp <- 0}
        if(nrow(tTable)>1){ fn <- tTable[2,1] } else { fn <- 0}
        if(ncol(tTable)>1 & nrow(tTable)>1){ tn <- tTable[2,2] } else { tn <- 0}

        return((tp + tn) / (tp + tn + fp + fn))
}

#-------------------------------------------------------------------------------
#-----------------------Create Function To Measure TPR = sensitivity = recall---
#-------------------------------------------------------------------------------
tpr <- function (truth, predicted){
        tTable   <- table(truth,predicted)
        tp <- tTable[1,1]
        if(ncol(tTable)>1){ fp <- tTable[1,2] } else { fp <- 0}
        if(nrow(tTable)>1){ fn <- tTable[2,1] } else { fn <- 0}
        if(ncol(tTable)>1 & nrow(tTable)>1){ tn <- tTable[2,2] } else { tn <- 0}

        return(tp / (tp + fn))
}


#-------------------------------------------------------------------------------
#-----------------------Create Function To Measure FPR--------------------------
#-------------------------------------------------------------------------------
fpr <- function (truth, predicted){
        tTable   <- table(truth,predicted)
        tp <- tTable[1,1]
        if(ncol(tTable)>1){ fp <- tTable[1,2] } else { fp <- 0}
        if(nrow(tTable)>1){ fn <- tTable[2,1] } else { fn <- 0}
        if(ncol(tTable)>1 & nrow(tTable)>1){ tn <- tTable[2,2] } else { tn <- 0}

        return(fp / (fp + tn))
}

#-------------------------------------------------------------------------------
#-----------------------Create Function To Measure TNR = specificity------------
#-------------------------------------------------------------------------------
tnr <- function (truth, predicted){
        tTable   <- table(truth,predicted)
        tp <- tTable[1,1]
        if(ncol(tTable)>1){ fp <- tTable[1,2] } else { fp <- 0}
        if(nrow(tTable)>1){ fn <- tTable[2,1] } else { fn <- 0}
        if(ncol(tTable)>1 & nrow(tTable)>1){ tn <- tTable[2,2] } else { tn <- 0}

        return(tn / (tn + fp))
}

#-------------------------------------------------------------------------------
#-----------------------Create Function To Measure PPV = precision--------------
#-------------------------------------------------------------------------------
ppv <- function (truth, predicted){
        tTable   <- table(truth,predicted)
        tp <- tTable[1,1]
        if(ncol(tTable)>1){ fp <- tTable[1,2] } else { fp <- 0}
        if(nrow(tTable)>1){ fn <- tTable[2,1] } else { fn <- 0}
        if(ncol(tTable)>1 & nrow(tTable)>1){ tn <- tTable[2,2] } else { tn <- 0}

        return(tp / (tp + fp))
}


#-------------------------------------------------------------------------------
#-----------------------Create Function To Measure FDR--------------------------
#-------------------------------------------------------------------------------
fdr <- function (truth, predicted){
        ppv1 <- ppv(truth, predicted)
        return (1-ppv1)
}
