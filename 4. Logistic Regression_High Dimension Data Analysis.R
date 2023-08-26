library(foreign)

setwd("C:/Users/adib1/OneDrive - Florida International University/Classes/Large Data Analysis/Project")
data=read.spss("LargeDataProj04142021.sav", use.value.label=FALSE, to.data.frame=TRUE)
data.NY = subset(data, HHSTATE %in% c("NY"))
data.FL = subset(data, HHSTATE=="FL")

# For Florida
n.FL = nrow(data.FL)
t.FL = sample (n.FL,200)
te.FL = data.frame(X2=data.FL[t.FL,3:22], g2=data.FL[t.FL,2])
tr.FL = data.frame (X2=data.FL[-t.FL,3:22], g2=data.FL[-t.FL,2])

# For New York
n.NY = nrow(data.NY)
t.NY = sample (n.NY,3000)
te.NY = data.frame(X1=data.NY[t.NY,3:22], g1=data.NY[t.NY,2])
tr.NY = data.frame (X1=data.NY[-t.NY,3:22], g1=data.NY[-t.NY,2])

########## Logistic Regression for NY
full_1 = glm(as.factor(g1)~.,family = binomial, data=tr.NY)
# Predict on training
logreg.pred.tr1=predict(full_1,type="response")>0.5
table(logreg.pred.tr1,tr.NY$g1)
sum(diag(table(logreg.pred.tr1,tr.NY$g1)))/sum(table(logreg.pred.tr1,tr.NY$g1))
# Predict on test
logreg.pred.te1=predict(full_1,newdata=te.NY,type="response")>0.5
table(logreg.pred.te1,te.NY$g1) 
sum(diag(table(logreg.pred.te1,te.NY$g1)))/sum(table(logreg.pred.te1,te.NY$g1))

########## Logistic Regression for FL
full_2 = glm(as.factor(g2)~.,family = binomial, data=tr.FL)
# Predict on training
logreg.pred.tr2=predict(full_2,type="response")>0.5
table(logreg.pred.tr2,tr.FL$g2)
sum(diag(table(logreg.pred.tr2,tr.FL$g2)))/sum(table(logreg.pred.tr2,tr.FL$g2))
# Predict on test
logreg.pred.te2=predict(full_2,newdata=te.FL,type="response")>0.5
table(logreg.pred.te2,te.FL$g2) 
sum(diag(table(logreg.pred.te2,te.FL$g2)))/sum(table(logreg.pred.te2,te.FL$g2))
