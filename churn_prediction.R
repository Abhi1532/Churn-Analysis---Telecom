library(data.table)
data<-fread("churn_data.csv",sep=",",header=T)
valdata<-fread("Validation_data.csv",sep=",",header=T)
data = as.data.frame(data)
summary(data)
valdata = as.data.frame(valdata)
valdata$churnStatus <-replace(valdata$churnStatus,valdata$churnStatus == FALSE,0)
valdata$churnStatus <-replace(valdata$churnStatus,valdata$churnStatus == TRUE,1)
summary(valdata)

pclass<-subset(data,churnStatus==1) 
n1class<-subset(data,churnStatus==0)
r<-nrow(pclass)+25000
nclass<-n1class[1:r,]

trainprow<-trunc(.7 * nrow(pclass))
trainnrow<-trunc(.3 * nrow(nclass))
testprow<-nrow(pclass) - trainprow
testnrow<-nrow(nclass) - trainnrow
idpclass=sample(seq(1:nrow(pclass)),nrow(pclass), replace=F)
idnclass=sample(seq(1:nrow(nclass)),nrow(nclass), replace=F)
trclass=rbind(pclass[idpclass[1:trainprow],],nclass[idnclass[1:trainnrow],])
testclass=rbind(pclass[idpclass[(trainprow +1):(nrow(pclass)) ],],nclass[idnclass[(trainnrow+1):nrow(nclass)],])

train<-as.data.frame(trclass[-1])
test<-as.data.frame(testclass[-1])

#reg<-glm(churnStatus~.,family=binomial(link = "logit"),data=train)
#summary(reg)
#length(reg$coefficients) > reg$rank
reg<-glm(churnStatus~AgeOnNet+mean2G3G+
           meanVOL_2G+
           meanVOL_3G+meanTotal_Bill+
           meanUsag+meanLOC_OG_XYZ2XYZ_MOU+varLOC_OG_XYZ2XYZ_MOU+
           meanSTD_OG_XYZ2XYZ_MOU+varSTD_OG_XYZ2XYZ_MOU+meanLOC_OG_XYZ2M_MOU+
           varLOC_OG_XYZ2M_MOU+meanSTD_OG_XYZ2M_MOU+
           varSTD_OG_MOU+meanISD_OG_MOU+meanTOTAL_OG_MOU+
           varTOTAL_OG_MOU+meanLOC_IC_XYZ2XYZ_MOU+
           meanSTD_IC_XYZ2XYZ_MOU+
           meanISD_IC_MOU+
           meanROAM_IC_MOU+meanTOTAL_IC_MOU+
           varTOTAL_IC_MOU+meannonRentRatio+varnonRentRatio+vartotalSTD+vartotalLocal+
           meanXYZ2XYZ_XYZ2M_RATIO ,family=binomial(link = "logit"),
         data=train)
stglm1<-step(reg, data=train, direction="backward")
summary(stglm1)
stglm1$anova
summary(stglm1)

reg<-glm(formula = churnStatus ~ AgeOnNet + mean2G3G + meanVOL_2G + 
        meanTotal_Bill + meanUsag + meanLOC_OG_XYZ2XYZ_MOU + varLOC_OG_XYZ2XYZ_MOU + 
        meanSTD_OG_XYZ2XYZ_MOU + meanLOC_OG_XYZ2M_MOU + varLOC_OG_XYZ2M_MOU + 
        meanSTD_OG_XYZ2M_MOU + varSTD_OG_MOU + meanISD_OG_MOU + meanTOTAL_OG_MOU + 
        varTOTAL_OG_MOU + meanLOC_IC_XYZ2XYZ_MOU + meanSTD_IC_XYZ2XYZ_MOU + 
        meanISD_IC_MOU + meanROAM_IC_MOU + meanTOTAL_IC_MOU + varTOTAL_IC_MOU + 
        meannonRentRatio + varnonRentRatio + vartotalSTD + meanXYZ2XYZ_XYZ2M_RATIO, 
      family = binomial(link = "logit"), data = train)
summary(reg)
length(reg$coefficients) > reg$rank
res1<-predict(reg,newdata=test,type="response")
cres1<-replace(res1,res1>=0.5,1)
cres2<-replace(cres1,cres1<0.5,0)
ctab1 <- table(factor(cres2),factor(test$churnStatus))
library(pROC)
library("caret")
troc1<-roc(as.numeric(test$churnStatus),as.numeric(cres2))
plot(troc1,xlim=c(1,0),ylim=c(0,1),asp=0,main="ROC Curve for test data")
library(ROCR)
m1.scores <- prediction(cres2, test$churnStatus)
m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit

cm1<-confusionMatrix(ctab1)
cm1
accuracy1<-cm1$overall["Accuracy"]
print(accuracy1)

res2<-predict(reg,newdata=valdata,type="response")
cres3<-replace(res2,res2>=0.5,1)
cres4<-replace(cres3,cres3<0.5,0)
ctab2 <- table(factor(cres4),factor(valdata$churnStatus))
library(pROC)
library("caret")
troc2<-roc(as.numeric(valdata$churnStatus),as.numeric(cres4))
plot(troc2,xlim=c(1,0),ylim=c(0,1),asp=0,main="ROC Curve for validation data")
library(ROCR)
m2.scores <- prediction(cres4, valdata$churnStatus)
m2.perf <- performance(m2.scores, "tpr", "fpr")
ks2.logit <- max(attr(m2.perf, "y.values")[[1]] - (attr(m2.perf, "x.values")[[1]]))
ks2.logit

cm2<-confusionMatrix(ctab2)
cm2
accuracy2<-cm2$overall["Accuracy"]
print(accuracy2)
