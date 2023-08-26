# install.packages("mlogit")

library(mlogit)
library(readxl)
# library(nnet)

setwd("D:/Statistical Model/MNL_ML")
mydata<- read_excel("Ecomm_Logit_data_final.xlsx", sheet = "Gr")
attach(mydata)
head(mydata)

# Descriptive statistics
table(Choice)

# Reshaping the data from wide to long format
mldata<-mlogit.data(mydata, varying=9:20, choice="Choice", id.var = "ID", shape="wide")
mldata[1:20,]


################ Multinomial Logit Model #####################

#Multinomial logit model coefficients 
mlogit.model1 <- mlogit(Choice ~ Product_price+Shopping_time+Delivery_cost+Delivery_time+Travel_time,
                        data=mldata, reflevel="instore")
summary(mlogit.model1)


################### Mixed Logit Models ##########################

# Main Effects
mlogit.model2 <- mlogit(Choice ~ Product_price+Shopping_time+Delivery_cost+Travel_time|
                          Fem+Hisp+Gen_Z+Millennials+Gen_X+Boomers_II+Black+From_15k_to_25k+
                          From_25k_to_35k+From_35k_to_50k+From_50k_to_75k+Less_than_highsch+
                          Highsch_grad+associate+Full_empl+Part_empl+Stdnt_intn_volnt+hhsize_3+
                          hhsize_4+hhsize_5plus+CB5to18_0+MOT65_0+MWDL_0+NoVeh_1+NoVeh3plus+
                          VehAcc_0prcnt+VehAcc_100prcnt+Secur+Tech_depend+Alt_mob+Env+Local_st+
                          Recreatnl+Deliv+Online_pos+Dist_0to5mi,
                        rpar = c(Product_price="zbt", Shopping_time="zbt", Delivery_cost="zbt",
                                 Travel_time="zbt"), 
                        data=mldata, R = 1000, halton = NA, panel = TRUE, reflevel="instore")
summary(mlogit.model2)

#### Marginal effect
m <- mlogit(Choice ~ Product_price+Shopping_time+Delivery_cost+Travel_time|
              Fem+Hisp+Gen_Z+Millennials+Gen_X+Boomers_II+Black+From_15k_to_25k+
              From_25k_to_35k+From_35k_to_50k+From_50k_to_75k+Less_than_highsch+
              Highsch_grad+associate+Full_empl+Part_empl+Stdnt_intn_volnt+hhsize_3+
              hhsize_4+hhsize_5plus+CB5to18_0+MOT65_0+MWDL_0+NoVeh_1+NoVeh3plus+
              VehAcc_0prcnt+VehAcc_100prcnt+Secur+Tech_depend+Alt_mob+Env+Local_st+
              Recreatnl+Deliv+Online_pos+Dist_0to5mi,
            rpar = c(Product_price="zbt", Shopping_time="zbt", Delivery_cost="zbt",
                     Travel_time="zbt"), 
            data=mldata, R = 1000, halton = NA, panel = TRUE, reflevel="instore")

z <- with(mldata, data.frame(Product_price = tapply(Product_price, index(m)$alt, mean), 
                             Shopping_time = tapply(Shopping_time, index(m)$alt, mean), 
                             Delivery_cost = tapply(Delivery_cost, index(m)$alt, mean), 
                             Travel_time = tapply(Travel_time, index(m)$alt, mean), 
                             Fem = mean(Fem), Gen_Z = mean(Gen_Z)))

effects(mlogit.model2, covariate = "Fem", data = z)


# Main and Interaction Effects
mlogit.model3 <- mlogit(Choice ~ Product_price+Shopping_time+Delivery_cost+Travel_time+Fem_DC+Fem_TT+
                          Hisp_PP+GenZ_DC+GenZ_TT+Millenials_DC+Millenials_TT+GenX_PP+GenX_DC+GenX_TT+
                          BoomersII_PP+BoomersII_DC+BoomersII_TT+White_DC+Asian_PP+Asian_DC+
                          LessThan15k_DC+LessThan15k_TT+Frm35kto50k_PP+Frm100kto150k_DC+Lesshighsch_DC+
                          HighschGrad_PP+SomeCollege_PP+SomeCollege_DC+Associate_PP+Associate_DC+
                          Bachelors_PP+Gradorhigher_PP+PartEmpl_DC+StdIntVol_TT+Homemaker_DC+
                          Retired_DC+DetSngl_DC+Hhsize1_DC+Veh3plus_TT+VehAcc0_PP+
                          VehAcc100_PP+Secur_PP+Secur_DC+TechDepend_TT+AltMob_PP+AltMob_DC+Env_PP+
                          LocalSt_PP+Deliv_PP+OnPos_PP+TechPref_PP+Dist0to5mi_PP+Dist0to5mi_TT+
                          Dist5to10mi_PP+Dist5to10mi_DC+Dist10to15mi_TT+Dist20plus_PP
                        | Fem+Hisp+Gen_Z+Millennials+Gen_X+Boomers_II+Black+From_35k_to_50k+
                          Less_than_highsch+Full_empl+hhsize_3+hhsize_4+hhsize_5plus+MOT65_0+MWDL_0+
                          NoVeh_1+NoVeh3plus+VehAcc_0prcnt+Secur+Tech_depend+Alt_mob+Local_st+
                          Recreatnl+Deliv+Online_pos+Dist_0to5mi,
                        rpar = c(Product_price="zbt", Shopping_time="zbt", Delivery_cost="zbt",
                                 Travel_time="zbt"), 
                        data=mldata, R = 1000, halton = NA, panel = TRUE, reflevel="instore")
summary(mlogit.model3)

# Main and Interaction Effects (II)
mlogit.model4 <- mlogit(Choice ~ Product_price+Shopping_time+Delivery_cost+Travel_time+Fem_DC+Fem_TT+
                          Hisp_PP+GenZ_DC+GenZ_TT+Millenials_DC+Millenials_TT+GenX_PP+GenX_DC+GenX_TT+
                          BoomersII_PP+BoomersII_DC+BoomersII_TT+White_DC+Asian_DC+
                          LessThan15k_DC+LessThan15k_TT+Frm35kto50k_PP+Frm100kto150k_DC+Lesshighsch_DC+
                          HighschGrad_PP+SomeCollege_PP+SomeCollege_DC+Associate_PP+Associate_DC+
                          Bachelors_PP+Gradorhigher_PP+PartEmpl_DC+StdIntVol_TT+Homemaker_DC+
                          Retired_DC+DetSngl_DC+Hhsize1_DC+Veh3plus_TT+VehAcc0_PP+VehAcc100_PP+
                          Secur_PP+Secur_DC+TechDepend_TT+AltMob_PP+AltMob_DC+Env_PP+Rec_PP+
                          LocalSt_PP+Deliv_PP+OnPos_PP+CostConsc_PP+Dist0to5mi_PP+Dist0to5mi_TT+
                          Dist5to10mi_PP+Dist5to10mi_DC+Dist10to15mi_TT+Dist20plus_PP
                        | Fem+Hisp+Gen_Z+Millennials+Gen_X+Boomers_II+Black+From_35k_to_50k+
                          Less_than_highsch+Full_empl+hhsize_3+hhsize_4+hhsize_5plus+MOT65_0+MWDL_0+
                          NoVeh_1+NoVeh3plus+VehAcc_0prcnt+Secur+Tech_depend+Alt_mob+Local_st+
                          Recreatnl+Deliv+Online_pos+Dist_0to5mi,
                        rpar = c(Product_price="zbt", Shopping_time="zbt", Delivery_cost="zbt",
                                 Travel_time="zbt"), 
                        data=mldata, R = 500, halton = NA, panel = TRUE, reflevel="instore")
summary(mlogit.model4)

# Statistic(s) of the random parameter(s)
stdev(rpar(mlogit.model4, "Product_price", norm = NULL))
stdev(rpar(mlogit.model4, "Shopping_time", norm = NULL))
stdev(rpar(mlogit.model4, "Delivery_cost", norm = NULL))
stdev(rpar(mlogit.model4, "Travel_time", norm = NULL))

# Multinomial logit model odds ratios 
# exp(coef(mlogit.model2))
