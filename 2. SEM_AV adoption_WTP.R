library(foreign) 
library(lavaan)

setwd("D:/Statistical Model/SEM")

data <- read.csv("full_data_with_interaction.csv")

dim(data)

###Confirmatory factor analysis

cfamodel <- "
    #defining the latent variables
    
    F2=~Tech1+Tech2+Tech3
    F3=~Choice5+Choice6
    F6=~Own2+Own3+Own4+Own5+Own6
    F5=~Mot1+Feat1+Feat6+Feat7
"
fitcfa <- cfa(data = data, model = cfamodel)

summary(fitcfa)

data_2 <- data.frame(data, predict(fitcfa))

dim(data_2)

library(dplyr)

data_3 <- data_2 %>% mutate(F5_older_int = F5 * older_dummy, F2_older_int = F2 * older_dummy, 
                            F3_older_int = F3 * older_dummy, F6_older_int = F6 * older_dummy)
dim(data_3)


###############################################

###Best base model

model_final <- '

WTP ~ F2 + F3 + F5 + AV_ten_years  + Fare1  + Fare1_older_int 
+ Trip_Dist5 + Trip_Dist10 + Inc_50k_75k_older_int
+Male+High_School+Some_college+Associate_degree+Bachelor+OnlineShop.2+OnlineShop.3 +OnlineShop.6
+Trip_Dist5 +Trip_Dist20+Age8+Parking3+Parking5

+ D1C_older_int 
 


AV_ten_years ~ F3 + F5 + F5_older_int + Fare3 + Parking2_older_int + Access3 
+ Trip_Dist30 + TT_30more  + Inc_25k_50k_older_int 
+Male+Student+OnlineShop.2+HHIncome_0_25k+Inc_25k_50k+Inc_200k_more
+DenPop+AvgD2A_EPHHM
 
+ FullTime_older_int 
+ HHSize.6_older_int
+ D1C5_OFF10 + D1C5_IND10 + D1C5_SVC10 
+ AvgD3A + AvgD2A_EPHHM


 
' 
secondorder <- sem(model_final, data=data_3, ordered= c('WTP', 'AV_ten_years')) 
summary(secondorder,fit.measures=TRUE,standardized=TRUE)
fitmeasures(secondorder)

######################################################
#Final optimized model

model_final_2 <- '

WTP ~ F2 + F3 + F5 + AV_ten_years  + Fare1  + Fare1_older_int 
+ Trip_Dist5 + Inc_25k_50k_older_int
+Male+OnlineShop.3 +OnlineShop.6
+Trip_Dist5 +Trip_Dist20+Age8+Parking5

+ D1C_older_int 
 


AV_ten_years ~ F3 + F5 + F5_older_int + Parking2_older_int + Access3 
+ Trip_Dist30 + TT_30more   
+Male+Student+OnlineShop.2+HHIncome_0_25k+Inc_25k_50k+Inc_200k_more
+DenPop+AvgD2A_EPHHM
 
 
+ HHSize.6_older_int
+ D1C5_OFF10 + D1C5_IND10 + D1C5_SVC10 
+ AvgD3A + AvgD2A_EPHHM


 
' 
secondorder <- sem(model_final_2, data=data_3, ordered= c('WTP', 'AV_ten_years')) 
summary(secondorder,fit.measures=TRUE,standardized=TRUE)
fitmeasures(secondorder)