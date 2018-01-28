library(matrixcalc) #for function is.positive.definite()

#Add Uncertainty for others
TrainingOtherCorr=read.csv("SixCombinedCorrelation/PD_ CombinedTrainingOther_NoMissing.csv", header=FALSE, sep=",")
TrainingOtherCorr=as.matrix(TrainingOtherCorr) 
TrainingOtherCorr000=cov2cor((matrix(rWishart(n=1,df=3834,TrainingOtherCorr),3834,3834)))
write.table(TrainingOtherCorr000,file="C:/Users/Wanru/Documents/SixCombinedCorrelation/Org000_CombinedTrainingOther_NoMissing.csv",sep=",",col.names=F,row.names=F)


# code by Imran for making wishart generated matrices Positive definite and to be used by next modules

TrainingOtherCorr <- read_csv("C:/Users/Mimran/OneDrive - George Mason University/C4I PC Backup/SCITE/RCPs Sixth Quarter/RCP15/PD corr mats/PD_ CombinedTrainingOther.csv", col_names = F)

TrainingOtherCorr=as.matrix(TrainingOtherCorr) #converting to matrix object type

TrainingOtherCorr000=cov2cor((matrix(rWishart(n=1,df=dim(TrainingOtherCorr)[1],TrainingOtherCorr),dim(TrainingOtherCorr)[1],dim(TrainingOtherCorr)[1])))

TrainingOtherCorr000 <- round(TrainingOtherCorr000, 10) #reduce the number of significant digits because it messes with the checks of is.positive.definite()

if (is.positive.definite(TrainingOtherCorr000) == FALSE) #if the check returns TRUE then do nearPD apporximation else do nothing
{
  TrainingOtherCorr000 <- nearPD(TrainingOtherCorr000) %>% .$mat %>% as.matrix
}

write_csv(TrainingOtherCorr000, "C:/Users/Wanru/Documents/SixCombinedCorrelation/Org000_CombinedTrainingOther_NoMissing.csv", col_names=F)T

# end of code block by Imran


ValidationOtherCorr=read.csv("SixCombinedCorrelation/PD_ CombinedValidationOther_NoMissing.csv", header=FALSE, sep=",")
ValidationOtherCorr=as.matrix(ValidationOtherCorr) 
ValidationOtherCorr000=cov2cor((matrix(rWishart(n=1,df=3834,ValidationOtherCorr),3834,3834)))
write.table(ValidationOtherCorr000,file="C:/Users/Wanru/Documents/SixCombinedCorrelation/Org000_CombinedValidationOther_NoMissing.csv",sep=",",col.names=F,row.names=F)

TestingOtherCorr=read.csv("SixCombinedCorrelation/PD_ CombinedTestingOther_NoMissing.csv", header=FALSE, sep=",")
TestingOtherCorr=as.matrix(TestingOtherCorr) 
TestingOtherCorr000=cov2cor((matrix(rWishart(n=1,df=2414,TestingOtherCorr),2414,2414)))
write.table(TestingOtherCorr000,file="C:/Users/Wanru/Documents/SixCombinedCorrelation/Org006_CombinedTestingOther_NoMissing.csv",sep=",",col.names=F,row.names=F)

#Add uncertainty for linemanagers
TrainingManagerCorr=read.csv("SixCombinedCorrelation/PD_ CombinedTrainingLineManagement_NoMissing.csv", header=FALSE, sep=",")
TrainingManagerCorr=as.matrix(TrainingManagerCorr) 
TrainingManagerCorr000=cov2cor((matrix(rWishart(n=1,df=3834,TrainingManagerCorr),3834,3834)))
write.table(TrainingManagerCorr000,file="C:/Users/Wanru/Documents/SixCombinedCorrelation/Org008_CombinedTrainingManager_NoMissing.csv",sep=",",col.names=F,row.names=F)

ValidationManagerCorr=read.csv("SixCombinedCorrelation/PD_ CombinedValidationLineManagement_NoMissing.csv", header=FALSE, sep=",")
ValidationManagerCorr=as.matrix(ValidationManagerCorr) 
ValidationManagerCorr000=cov2cor((matrix(rWishart(n=1,df=3834,ValidationManagerCorr),3834,3834)))
write.table(ValidationManagerCorr000,file="C:/Users/Wanru/Documents/SixCombinedCorrelation/Org008_CombinedValidationManager_NoMissing.csv",sep=",",col.names=F,row.names=F)

TestingManagerCorr=read.csv("SixCombinedCorrelation/PD_ CombinedTestingLineManagement_NoMissing.csv", header=FALSE, sep=",")
TestingManagerCorr=as.matrix(TestingManagerCorr) 
TestingManagerCorr000=cov2cor((matrix(rWishart(n=1,df=2414,TestingManagerCorr),2414,2414)))
write.table(TestingManagerCorr000,file="C:/Users/Wanru/Documents/SixCombinedCorrelation/Org000_CombinedTestingManager_NoMissing.csv",sep=",",col.names=F,row.names=F)


