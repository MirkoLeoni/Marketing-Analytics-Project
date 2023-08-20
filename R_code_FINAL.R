# Libraries
library(readxl)
library(lattice)
library(readr)
library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(corrplot)
library(rlist)
library(lme4)
library(emmeans)
library(multcomp)
library(lmerTest)
library(stackoverflow)
library(survival)
library(ggfortify)
library(survminer)
library(stargazer)


## DATASET UPLOADING & MANAGEMENT ----
#Import the databases
ds_transaction_raw = fread("~/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Analytics for Business Lab/LAB_prjct/Dataset/transaction_dataset.txt", sep=";", na.strings = "NULL")
ds_customer = read_delim("~/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Analytics for Business Lab/LAB_prjct/Dataset/customer_dataset.txt",delim = ";", escape_double = FALSE, col_types = cols(gender=col_factor(levels = c("M","F"))), trim_ws = TRUE)
ds_customer = data.table(ds_customer)

#Data Cleaning
ds_transaction_raw[is.na(ds_transaction_raw)] = 0 # NA means zero expenses

ds_customer$gender = as.factor(ds_customer$gender)
ds_customer$loyalty_store = as.factor(ds_customer$loyalty_store)
ds_customer[ds_customer$birth_year == "1658", "birth_year"] = 1958 #typo corrected 
ds_customer[, `:=` (age = 2022 - birth_year, age_first_purchase = first_purchase_year - birth_year) ] # added age and age_first_purchase

# Rearrange the transaction database - Added the promo% and the PL%
ds_transaction = data.table(
  id_customer = ds_transaction_raw$id_customer,
  ticket_id = ds_transaction_raw$ticket_id,
  date = ds_transaction_raw$date,
  store_id = ds_transaction_raw$store_id,
  store_type = ds_transaction_raw$store_type,
  gross_sales = ds_transaction_raw$gross_sales,
  gross_sales_PL = ds_transaction_raw$PL_gross_sales,
  net_sales = ds_transaction_raw$net_sales,
  promo_perc = (1-(ds_transaction_raw$net_sales/ds_transaction_raw$gross_sales)),
  PL_perc = (ds_transaction_raw$number_item_PL)/(ds_transaction_raw$number_item_PL+ds_transaction_raw$number_items_other),
  num_item_other = ds_transaction_raw$number_items_other,
  num_item_PL = ds_transaction_raw$number_item_PL,
  num_item_TOT = ds_transaction_raw$number_item_PL + ds_transaction_raw$number_items_other 
)

ds_transaction$date = as.Date.character(ds_transaction$date)

#Extract the NEW customer id (subscription in 2021/22)
id_new_subscription = ds_customer[ds_customer$first_purchase_year %in% c(2021,2022), "id_customer"] # 1063 new subscriptions

# Create a new database without the NEW customers
ds_transaction_cleaned = ds_transaction[!(ds_transaction$id_customer %in% id_new_subscription$id_customer),]

ds_transaction_2122 = ds_transaction[(ds_transaction$id_customer %in% id_new_subscription$id_customer),]

# EXPLORATORY ANALYSIS ----

ds_transaction[, `:=`(weekday=as.factor(weekdays(date)), week = as.integer(format(ds_transaction_raw$date, "%W")), month = as.integer(format(ds_transaction_raw$date, "%m")))]
ds_transaction = setorder(ds_transaction, date)

by_month = ds_transaction[, .(frequency=.N, sumsales = sum(gross_sales), sumnet = sum(net_sales),sumPL = sum(gross_sales_PL), avgpromo = mean(promo_perc), avgPL = mean(PL_perc)), by= month]
by_month = setorder(by_month, month)

by_weekday = ds_transaction[, .(frequency=.N, sumgross = sum(gross_sales), sumnet = sum(net_sales), avgpromo = mean(promo_perc), avgPL = mean(PL_perc)), by=weekday]
by_weekday = setorder(by_weekday, weekday)

by_store_type = ds_transaction[, .(frequency=.N, sumsales = sum(gross_sales), sumnet = sum(net_sales), avgpromo = mean(promo_perc), avgPL = mean(PL_perc)), by = store_type]
# In superstore and e-commerce more PLshare, in iperstore and e-commerce higher average promo

#Anova on store type
anova = aov(PL_perc ~ store_type , data= ds_transaction)
summary(anova) # significant difference in the PLperc across store types
tukey = TukeyHSD(anova)
tukey
anova2 = aov(promo_perc ~ store_type , data= ds_transaction)
summary(anova2) # significant difference in the Promo across store types
tukey2 = TukeyHSD(anova2)
tukey2

# Anomaly detection (remove hothot customers) ----
# excluding those customers that at least once in every RFM period has been an "hot hot" customer

ds_1 = ds_transaction_cleaned[format.Date(date, "%m")=="01" | format.Date(date, "%m")=="02",]
ds_1 = setorder(ds_1, date)

ds_2 = ds_transaction_cleaned[format.Date(date, "%m")=="03" | format.Date(date, "%m")=="04",]
ds_2 = setorder(ds_2, date)

ds_3 = ds_transaction_cleaned[format.Date(date, "%m")=="05" | format.Date(date, "%m")=="06",]
ds_3 = setorder(ds_3, date)

ds_4 = ds_transaction_cleaned[format.Date(date, "%m")=="07" | format.Date(date, "%m")=="08",]
ds_4 = setorder(ds_4, date)

ds_5 = ds_transaction_cleaned[format.Date(date, "%m")=="09" | format.Date(date, "%m")=="10",]
ds_5 = setorder(ds_5, date)

ds_6 = ds_transaction_cleaned[format.Date(date, "%m")=="11" | format.Date(date, "%m")=="12",]
ds_6 = setorder(ds_6, date)

d1 = ds_1[, .(frequency=.N, monetary=sum(net_sales), promomean=mean(promo_perc), plmean=mean(PL_perc)), by=id_customer]
anoMon1 = quantile(d1$monetary, probs = 0.75) + (quantile(d1$monetary, probs = 0.75)-quantile(d1$monetary, probs = 0.25))*3
anoFre1 = quantile(d1$frequency, probs = 0.75) + (quantile(d1$frequency, probs = 0.75)-quantile(d1$frequency, probs = 0.25))*1.5
id_hothot1 = as.numeric(d1[frequency > anoFre1  & monetary > anoMon1, id_customer] )

d2 = ds_2[, .(frequency=.N, monetary=sum(net_sales), promomean=mean(promo_perc), plmean=mean(PL_perc)), by=id_customer]
anoMon2 = quantile(d2$monetary, probs = 0.75) + (quantile(d2$monetary, probs = 0.75)-quantile(d2$monetary, probs = 0.25))*3
anoFre2 = quantile(d2$frequency, probs = 0.75) + (quantile(d2$frequency, probs = 0.75)-quantile(d2$frequency, probs = 0.25))*1.5
id_hothot2 = as.numeric(d2[frequency > anoFre2  & monetary > anoMon2, id_customer] )

d3 = ds_3[, .(frequency=.N, monetary=sum(net_sales), promomean=mean(promo_perc), plmean=mean(PL_perc)), by=id_customer]
anoMon3 = quantile(d3$monetary, probs = 0.75) + (quantile(d3$monetary, probs = 0.75)-quantile(d3$monetary, probs = 0.25))*3
anoFre3 = quantile(d3$frequency, probs = 0.75) + (quantile(d3$frequency, probs = 0.75)-quantile(d3$frequency, probs = 0.25))*1.5
id_hothot3 = as.numeric(d3[frequency > anoFre3  & monetary > anoMon3, id_customer] )

d4 = ds_4[, .(frequency=.N, monetary=sum(net_sales), promomean=mean(promo_perc), plmean=mean(PL_perc)), by=id_customer]
anoMon4 = quantile(d4$monetary, probs = 0.75) + (quantile(d4$monetary, probs = 0.75)-quantile(d4$monetary, probs = 0.25))*3
anoFre4 = quantile(d4$frequency, probs = 0.75) + (quantile(d4$frequency, probs = 0.75)-quantile(d4$frequency, probs = 0.25))*1.5
id_hothot4 = as.numeric(d4[frequency > anoFre4  & monetary > anoMon4, id_customer] )

d5 = ds_5[, .(frequency=.N, monetary=sum(net_sales), promomean=mean(promo_perc), plmean=mean(PL_perc)), by=id_customer]
anoMon5 = quantile(d5$monetary, probs = 0.75) + (quantile(d5$monetary, probs = 0.75)-quantile(d5$monetary, probs = 0.25))*3
anoFre5 = quantile(d5$frequency, probs = 0.75) + (quantile(d5$frequency, probs = 0.75)-quantile(d5$frequency, probs = 0.25))*1.5
id_hothot5 = as.numeric(d5[frequency > anoFre5  & monetary > anoMon5, id_customer] )

d6 = ds_6[, .(frequency=.N, monetary=sum(net_sales), promomean=mean(promo_perc), plmean=mean(PL_perc)), by=id_customer]
anoMon6 = quantile(d6$monetary, probs = 0.75) + (quantile(d6$monetary, probs = 0.75)-quantile(d6$monetary, probs = 0.25))*3
anoFre6 = quantile(d6$frequency, probs = 0.75) + (quantile(d6$frequency, probs = 0.75)-quantile(d6$frequency, probs = 0.25))*1.5
id_hothot6 = as.numeric(d6[frequency > anoFre6  & monetary > anoMon6, id_customer] )

boxplot(d6$frequency, range=1.5,ylim= c(0,140), ylab="Frequency")
boxplot(d6$monetary, range=3, ylab= "Monetary", ylim= c(0,20000))

id_hothot_YEAR = unique(sort(c(id_hothot1, id_hothot2, id_hothot3, id_hothot4, id_hothot5, id_hothot6)))

rm(list = c("anoFre1","anoFre2","anoFre3","anoFre4","anoFre5","anoFre6","anoMon1","anoMon2","anoMon3","anoMon4","anoMon5","anoMon6",
            "d1","d2","d3","d4","d5","d6","ds_1","ds_2","ds_3","ds_4","ds_5","ds_6"))
rm(list = c("id_hothot1","id_hothot2","id_hothot3","id_hothot4","id_hothot5","id_hothot6"))

ds_transaction_cleaned = ds_transaction_cleaned[!(ds_transaction_cleaned$id_customer %in% id_hothot_YEAR),]

## RegFM ANALYSIS ----

# Creating the 2 month subsets 
ds_1 = ds_transaction_cleaned[format.Date(date, "%m")=="01" | format.Date(date, "%m")=="02",]
ds_1 = setorder(ds_1, date)

ds_2 = ds_transaction_cleaned[format.Date(date, "%m")=="03" | format.Date(date, "%m")=="04",]
ds_2 = setorder(ds_2, date)

ds_3 = ds_transaction_cleaned[format.Date(date, "%m")=="05" | format.Date(date, "%m")=="06",]
ds_3 = setorder(ds_3, date)

ds_4 = ds_transaction_cleaned[format.Date(date, "%m")=="07" | format.Date(date, "%m")=="08",]
ds_4 = setorder(ds_4, date)

ds_5 = ds_transaction_cleaned[format.Date(date, "%m")=="09" | format.Date(date, "%m")=="10",]
ds_5 = setorder(ds_5, date)

ds_6 = ds_transaction_cleaned[format.Date(date, "%m")=="11" | format.Date(date, "%m")=="12",]
ds_6 = setorder(ds_6, date)

# RegFM_6  November December -----

d6 = ds_6[, .(frequency=.N, monetary=sum(net_sales), promomean=mean(promo_perc), plmean=mean(PL_perc)), by=id_customer]

coldcold6 = d6[frequency<3|id_customer==14998]
# extracting those for which the regularity is impossible to calculate (frequency<3), and those for which is NA (only purchases in one day in the given time frame)
RFM_ds_6 = anti_join(d6, coldcold6)

coldcold6 = coldcold6[,.(id_customer, promomean6=promomean, plmean6=plmean, clusters6 = "coldcold")]

id_active_6 = RFM_ds_6[,id_customer]
id_active_6 = sort(id_active_6)
Reg_ds_6 = data.table(id_customer = id_active_6, MeanInt = 0, StdInt = 0)

#Interpurchase time in ds_1t
for (i in id_active_6) {
  aux = data.frame(date = ds_6[ds_6$id_customer == i, "date"])
  aux = aux %>% arrange(date)
  aux_reg = mean(diff(aux$date))
  aux_std = sd(diff(aux$date))
  Reg_ds_6[Reg_ds_6$id_customer == i, "MeanInt"] = aux_reg
  Reg_ds_6[Reg_ds_6$id_customer == i, "StdInt"] = aux_std
}

RFM_ds_6 = RFM_ds_6[Reg_ds_6, on =.(id_customer=id_customer)] 
RFM_ds_6 = RFM_ds_6[,CV := StdInt/MeanInt]

# Setting the thresholds for the scores 

plot(quantile(RFM_ds_6$CV, probs= seq(0,1, by = 0.01)), type = 'l', xlab = "Percentile", ylab = "Regularity value", main = "Regularity percentile distribution" )
points(x = 10, y = quantile(RFM_ds_6$CV, probs= 0.1), pch = 19, col = "red")
points(x = 85, y = quantile(RFM_ds_6$CV, probs= 0.85), pch = 19, col = "red")

plot(quantile(RFM_ds_6$frequency, probs= seq(0,1, 0.01:0.99)), type = 'l', xlab = "Percentile", ylab = "Frequency value", main = "Frequency percentile distribution" )
points(x = 30, y = quantile(RFM_ds_6$frequency, probs= 0.30), pch = 19, col = "red")
points(x = 80, y = quantile(RFM_ds_6$frequency, probs= 0.80), pch = 19, col = "red")

plot(quantile(RFM_ds_6$monetary, probs= seq(0,1, 0.01:0.99)), type = 'l', xlab = "Percentile", ylab = "Monetary value", main = "Monetary percentile distribution" )
points(x = 33, y = quantile(RFM_ds_6$monetary, probs= 0.33), pch = 19, col = "red")
points(x = 66, y = quantile(RFM_ds_6$monetary, probs= 0.66), pch = 19, col = "red")

# Upper means higher value of the variables distribution
UpperReg = quantile(RFM_ds_6$CV, probs= 0.1)
LowerReg =  quantile(RFM_ds_6$CV, probs= 0.85)
UpperMon = quantile(RFM_ds_6$monetary, probs = 0.66)
LowerMon = quantile(RFM_ds_6$monetary, probs = 0.33)
UpperFreq = quantile(RFM_ds_6$frequency, probs = 0.80)
LowerFreq = quantile(RFM_ds_6$frequency, probs = 0.30)

# Setting the scores fof Reg - Freq - Mon
RFM_ds_6$scoreReg = as.numeric(ifelse(RFM_ds_6$CV <= UpperReg ,3 ,
                                      ifelse(RFM_ds_6$CV > UpperReg & RFM_ds_6$CV < LowerReg,2,1)))


RFM_ds_6$scoreFre = as.numeric (ifelse(RFM_ds_6$frequency <= LowerFreq , 1 ,
                                       ifelse(RFM_ds_6$frequency > LowerFreq & RFM_ds_6$frequency < UpperFreq, 2 , 3)))


RFM_ds_6$scoreMon = as.numeric(ifelse(RFM_ds_6$monetary <= LowerMon, 1 ,
                                      ifelse(RFM_ds_6$monetary > LowerMon & RFM_ds_6$monetary < UpperMon, 2, 3)))


RFM_ds_6$Segments = as.factor(ifelse(RFM_ds_6$scoreFre == 3  & RFM_ds_6$scoreMon == 3 & (RFM_ds_6$scoreReg == 3|RFM_ds_6$scoreReg == 2), "champion",
                                     ifelse((RFM_ds_6$scoreFre == 3|RFM_ds_6$scoreFre == 2) & RFM_ds_6$scoreMon == 3 & RFM_ds_6$scoreReg == 1, "sensitive",
                                            ifelse(RFM_ds_6$scoreFre == 2  & RFM_ds_6$scoreMon == 3 & (RFM_ds_6$scoreReg == 2|RFM_ds_6$scoreReg == 3), "star",
                                                   ifelse(RFM_ds_6$scoreFre == 1 & RFM_ds_6$scoreMon == 1 & RFM_ds_6$scoreReg == 1, "frozen",
                                                          ifelse(RFM_ds_6$scoreFre == 1 & RFM_ds_6$scoreMon == 1 & (RFM_ds_6$scoreReg == 3|RFM_ds_6$scoreReg == 2), "precise",
                                                                 ifelse(RFM_ds_6$scoreFre == 1 & (RFM_ds_6$scoreMon == 3|RFM_ds_6$scoreMon == 2) & RFM_ds_6$scoreReg ==1, "bomb",     
                                                                        ifelse(RFM_ds_6$scoreFre == 1 & (RFM_ds_6$scoreMon == 2|RFM_ds_6$scoreMon == 3) & (RFM_ds_6$scoreReg == 3|RFM_ds_6$scoreReg == 2), "stock buyer", 
                                                                               ifelse(RFM_ds_6$scoreFre == 2 & RFM_ds_6$scoreMon == 2 & (RFM_ds_6$scoreReg == 2|RFM_ds_6$scoreReg == 3) ,"average",     
                                                                                      ifelse((RFM_ds_6$scoreFre == 2 |RFM_ds_6$scoreFre == 3) & (RFM_ds_6$scoreMon == 2|RFM_ds_6$scoreMon == 1) & RFM_ds_6$scoreReg == 1, "needed_attention", 
                                                                                             ifelse(RFM_ds_6$scoreFre == 2 & RFM_ds_6$scoreMon == 1 & (RFM_ds_6$scoreReg == 2 | RFM_ds_6$scoreReg == 3), "single",     
                                                                                                           ifelse(RFM_ds_6$scoreFre == 3 & (RFM_ds_6$scoreMon == 1|RFM_ds_6$scoreMon == 2) &  (RFM_ds_6$scoreReg == 2|RFM_ds_6$scoreReg == 3), "promising", "outsider"))))))))))))
table(RFM_ds_6$Segments)


# RegFM_1 January February ----

d1 = ds_1[, .(frequency=.N, monetary=sum(net_sales), promomean=mean(promo_perc), plmean=mean(PL_perc)), by=id_customer]

coldcold1 = d1[(frequency<3)|(id_customer == 809)]
RFM_ds_1 = anti_join(d1, coldcold1)
coldcold1 = coldcold1[,.(id_customer, promomean1=promomean, plmean1=plmean, clusters1 = "coldcold")]

id_active_1 = RFM_ds_1[,id_customer]
id_active_1 = sort(id_active_1)
Reg_ds_1 = data.table(id_customer = id_active_1, MeanInt = 0, StdInt = 0)

#Interpurchase time in ds_1
for (i in id_active_1) {
  aux = data.frame(date = ds_1[ds_1$id_customer == i, "date"])
  aux = aux %>% arrange(date)
  aux_reg = mean(diff(aux$date))
  aux_std = sd(diff(aux$date))
  Reg_ds_1[Reg_ds_1$id_customer == i, "MeanInt"] = aux_reg
  Reg_ds_1[Reg_ds_1$id_customer == i, "StdInt"] = aux_std
}

RFM_ds_1 = RFM_ds_1[Reg_ds_1, on =.(id_customer=id_customer)] 
RFM_ds_1 = RFM_ds_1[,CV := StdInt/MeanInt]

#same thresholds as the last period (6)

RFM_ds_1$scoreReg = as.numeric(ifelse(RFM_ds_1$CV <= UpperReg ,3 ,
                                      ifelse(RFM_ds_1$CV > UpperReg & RFM_ds_1$CV < LowerReg,2,1)))


RFM_ds_1$scoreFre = as.numeric (ifelse(RFM_ds_1$frequency <= LowerFreq , 1 ,
                                       ifelse(RFM_ds_1$frequency > LowerFreq & RFM_ds_1$frequency < UpperFreq, 2 , 3)))


RFM_ds_1$scoreMon = as.numeric(ifelse(RFM_ds_1$monetary <= LowerMon, 1 ,
                                      ifelse(RFM_ds_1$monetary > LowerMon & RFM_ds_1$monetary < UpperMon, 2, 3)))


RFM_ds_1$Segments = as.factor(ifelse(RFM_ds_1$scoreFre == 3  & RFM_ds_1$scoreMon == 3 & (RFM_ds_1$scoreReg == 3|RFM_ds_1$scoreReg == 2), "champion",
                                     ifelse((RFM_ds_1$scoreFre == 3|RFM_ds_1$scoreFre == 2) & RFM_ds_1$scoreMon == 3 & RFM_ds_1$scoreReg == 1, "sensitive",
                                            ifelse(RFM_ds_1$scoreFre == 2  & RFM_ds_1$scoreMon == 3 & (RFM_ds_1$scoreReg == 2|RFM_ds_1$scoreReg == 3), "star",
                                                   ifelse(RFM_ds_1$scoreFre == 1 & RFM_ds_1$scoreMon == 1 & RFM_ds_1$scoreReg == 1, "frozen",
                                                          ifelse(RFM_ds_1$scoreFre == 1 & RFM_ds_1$scoreMon == 1 & (RFM_ds_1$scoreReg == 3|RFM_ds_1$scoreReg == 2), "precise",
                                                                 ifelse(RFM_ds_1$scoreFre == 1 & (RFM_ds_1$scoreMon == 3|RFM_ds_1$scoreMon == 2) & RFM_ds_1$scoreReg ==1, "bomb",     
                                                                        ifelse(RFM_ds_1$scoreFre == 1 & (RFM_ds_1$scoreMon == 2|RFM_ds_1$scoreMon == 3) & (RFM_ds_1$scoreReg == 3|RFM_ds_1$scoreReg == 2), "stock buyer", 
                                                                               ifelse(RFM_ds_1$scoreFre == 2 & RFM_ds_1$scoreMon == 2 & (RFM_ds_1$scoreReg == 2|RFM_ds_1$scoreReg == 3) ,"average",     
                                                                                      ifelse((RFM_ds_1$scoreFre == 2 |RFM_ds_1$scoreFre == 3) & (RFM_ds_1$scoreMon == 2|RFM_ds_1$scoreMon == 1) & RFM_ds_1$scoreReg == 1, "needed_attention", 
                                                                                             ifelse(RFM_ds_1$scoreFre == 2 & RFM_ds_1$scoreMon == 1 & (RFM_ds_1$scoreReg == 2 | RFM_ds_1$scoreReg == 3), "single",     
                                                                                                           ifelse(RFM_ds_1$scoreFre == 3 & (RFM_ds_1$scoreMon == 1|RFM_ds_1$scoreMon == 2) &  (RFM_ds_1$scoreReg == 2|RFM_ds_1$scoreReg == 3), "promising", "outsider"))))))))))))
table(RFM_ds_1$Segments)


# RegFM_2 March April ----

d2 = ds_2[, .(frequency=.N, monetary=sum(net_sales), promomean=mean(promo_perc), plmean=mean(PL_perc)), by=id_customer]

coldcold2 = d2[(frequency<3)|(id_customer == 4120)|(id_customer == 10892)]
RFM_ds_2 = anti_join(d2, coldcold2)
coldcold2 = coldcold2[,.(id_customer, promomean2=promomean, plmean2=plmean, clusters2 = "coldcold")]

id_active_2 = RFM_ds_2[,id_customer]
id_active_2 = sort(id_active_2)
Reg_ds_2 = data.table(id_customer = id_active_2, MeanInt = 0, StdInt = 0)

#Interpurchase time 
for (i in id_active_2) {
  aux = data.frame(date = ds_2[ds_2$id_customer == i, "date"])
  aux = aux %>% arrange(date)
  aux_reg = mean(diff(aux$date))
  aux_std = sd(diff(aux$date))
  Reg_ds_2[Reg_ds_2$id_customer == i, "MeanInt"] = aux_reg
  Reg_ds_2[Reg_ds_2$id_customer == i, "StdInt"] = aux_std
}

RFM_ds_2 = RFM_ds_2[Reg_ds_2, on =.(id_customer=id_customer)] 
RFM_ds_2 = RFM_ds_2[,CV := StdInt/MeanInt]

#same thresholds as the last period (6)

RFM_ds_2$scoreReg = as.numeric(ifelse(RFM_ds_2$CV < UpperReg ,3 ,
                                      ifelse(RFM_ds_2$CV > UpperReg & RFM_ds_2$CV < LowerReg,2,1)))


RFM_ds_2$scoreFre = as.numeric (ifelse(RFM_ds_2$frequency <= LowerFreq, 1 ,
                                       ifelse(RFM_ds_2$frequency > LowerFreq & RFM_ds_2$frequency < UpperFreq, 2 , 3)))


RFM_ds_2$scoreMon = as.numeric(ifelse(RFM_ds_2$monetary < LowerMon, 1 ,
                                      ifelse(RFM_ds_2$monetary > LowerMon & RFM_ds_2$monetary < UpperMon, 2, 3)))


RFM_ds_2$Segments = as.factor(ifelse(RFM_ds_2$scoreFre == 3  & RFM_ds_2$scoreMon == 3 & (RFM_ds_2$scoreReg == 3|RFM_ds_2$scoreReg == 2), "champion",
                                     ifelse((RFM_ds_2$scoreFre == 3|RFM_ds_2$scoreFre == 2) & RFM_ds_2$scoreMon == 3 & RFM_ds_2$scoreReg == 1, "sensitive",
                                            ifelse(RFM_ds_2$scoreFre == 2  & RFM_ds_2$scoreMon == 3 & (RFM_ds_2$scoreReg == 2|RFM_ds_2$scoreReg == 3), "star",
                                                   ifelse(RFM_ds_2$scoreFre == 1 & RFM_ds_2$scoreMon == 1 & RFM_ds_2$scoreReg == 1, "frozen",
                                                          ifelse(RFM_ds_2$scoreFre == 1 & RFM_ds_2$scoreMon == 1 & (RFM_ds_2$scoreReg == 3|RFM_ds_2$scoreReg == 2), "precise",
                                                                 ifelse(RFM_ds_2$scoreFre == 1 & (RFM_ds_2$scoreMon == 3|RFM_ds_2$scoreMon == 2) & RFM_ds_2$scoreReg ==1, "bomb",     
                                                                        ifelse(RFM_ds_2$scoreFre == 1 & (RFM_ds_2$scoreMon == 2|RFM_ds_2$scoreMon == 3) & (RFM_ds_2$scoreReg == 3|RFM_ds_2$scoreReg == 2), "stock buyer", 
                                                                               ifelse(RFM_ds_2$scoreFre == 2 & RFM_ds_2$scoreMon == 2 & (RFM_ds_2$scoreReg == 2|RFM_ds_2$scoreReg == 3) ,"average",     
                                                                                      ifelse((RFM_ds_2$scoreFre == 2 |RFM_ds_2$scoreFre == 3) & (RFM_ds_2$scoreMon == 2|RFM_ds_2$scoreMon == 1) & RFM_ds_2$scoreReg == 1, "needed_attention", 
                                                                                             ifelse(RFM_ds_2$scoreFre == 2 & RFM_ds_2$scoreMon == 1 & (RFM_ds_2$scoreReg == 2 | RFM_ds_2$scoreReg == 3), "single",     
                                                                                                           ifelse(RFM_ds_2$scoreFre == 3 & (RFM_ds_2$scoreMon == 1|RFM_ds_2$scoreMon == 2) &  (RFM_ds_2$scoreReg == 2|RFM_ds_2$scoreReg == 3), "promising", "outsider"))))))))))))



table(RFM_ds_2$Segments) 


# RegFM_3 May june ----

d3 = ds_3[, .(frequency=.N, monetary=sum(net_sales), promomean=mean(promo_perc), plmean=mean(PL_perc)), by=id_customer]

coldcold3 = d3[frequency<3|(id_customer == 204)|(id_customer == 2272)|(id_customer == 7751)]
RFM_ds_3 = anti_join(d3, coldcold3)
coldcold3 = coldcold3[,.(id_customer, promomean3=promomean, plmean3=plmean, clusters3 = "coldcold")]

id_active_3 = RFM_ds_3[,id_customer]
id_active_3 = sort(id_active_3)
Reg_ds_3 = data.table(id_customer = id_active_3, MeanInt = 0, StdInt = 0)

#Interpurchase time in ds_1t
for (i in id_active_3) {
  aux = data.frame(date = ds_3[ds_3$id_customer == i, "date"])
  aux = aux %>% arrange(date)
  aux_reg = mean(diff(aux$date))
  aux_std = sd(diff(aux$date))
  Reg_ds_3[Reg_ds_3$id_customer == i, "MeanInt"] = aux_reg
  Reg_ds_3[Reg_ds_3$id_customer == i, "StdInt"] = aux_std
}

RFM_ds_3 = RFM_ds_3[Reg_ds_3, on =.(id_customer=id_customer)] 
RFM_ds_3 = RFM_ds_3[,CV := StdInt/MeanInt]

#same thresholds as the last period (6)

RFM_ds_3$scoreReg = as.numeric(ifelse(RFM_ds_3$CV <= UpperReg ,3 ,
                                      ifelse(RFM_ds_3$CV > UpperReg & RFM_ds_3$CV < LowerReg,2,1)))


RFM_ds_3$scoreFre = as.numeric (ifelse(RFM_ds_3$frequency <= LowerFreq , 1 ,
                                       ifelse(RFM_ds_3$frequency > LowerFreq & RFM_ds_3$frequency < UpperFreq, 2 , 3)))


RFM_ds_3$scoreMon = as.numeric(ifelse(RFM_ds_3$monetary <= LowerMon, 1 ,
                                      ifelse(RFM_ds_3$monetary > LowerMon & RFM_ds_3$monetary < UpperMon, 2, 3)))


RFM_ds_3$Segments = as.factor(ifelse(RFM_ds_3$scoreFre == 3  & RFM_ds_3$scoreMon == 3 & (RFM_ds_3$scoreReg == 3|RFM_ds_3$scoreReg == 2), "champion",
                                     ifelse((RFM_ds_3$scoreFre == 3|RFM_ds_3$scoreFre == 2) & RFM_ds_3$scoreMon == 3 & RFM_ds_3$scoreReg == 1, "sensitive",
                                            ifelse(RFM_ds_3$scoreFre == 2  & RFM_ds_3$scoreMon == 3 & (RFM_ds_3$scoreReg == 2|RFM_ds_3$scoreReg == 3), "star",
                                                   ifelse(RFM_ds_3$scoreFre == 1 & RFM_ds_3$scoreMon == 1 & RFM_ds_3$scoreReg == 1, "frozen",
                                                          ifelse(RFM_ds_3$scoreFre == 1 & RFM_ds_3$scoreMon == 1 & (RFM_ds_3$scoreReg == 3|RFM_ds_3$scoreReg == 2), "precise",
                                                                 ifelse(RFM_ds_3$scoreFre == 1 & (RFM_ds_3$scoreMon == 3|RFM_ds_3$scoreMon == 2) & RFM_ds_3$scoreReg ==1, "bomb",     
                                                                        ifelse(RFM_ds_3$scoreFre == 1 & (RFM_ds_3$scoreMon == 2|RFM_ds_3$scoreMon == 3) & (RFM_ds_3$scoreReg == 3|RFM_ds_3$scoreReg == 2), "stock buyer", 
                                                                               ifelse(RFM_ds_3$scoreFre == 2 & RFM_ds_3$scoreMon == 2 & (RFM_ds_3$scoreReg == 2|RFM_ds_3$scoreReg == 3) ,"average",     
                                                                                      ifelse((RFM_ds_3$scoreFre == 2 |RFM_ds_3$scoreFre == 3) & (RFM_ds_3$scoreMon == 2|RFM_ds_3$scoreMon == 1) & RFM_ds_3$scoreReg == 1, "needed_attention", 
                                                                                             ifelse(RFM_ds_3$scoreFre == 2 & RFM_ds_3$scoreMon == 1 & (RFM_ds_3$scoreReg == 2 | RFM_ds_3$scoreReg == 3), "single",     
                                                                                                           ifelse(RFM_ds_3$scoreFre == 3 & (RFM_ds_3$scoreMon == 1|RFM_ds_3$scoreMon == 2) &  (RFM_ds_3$scoreReg == 2|RFM_ds_3$scoreReg == 3), "promising", "outsider"))))))))))))



table(RFM_ds_3$Segments) 


# RegFM_4 July August ----

d4 = ds_4[, .(frequency=.N, monetary=sum(net_sales), promomean=mean(promo_perc), plmean=mean(PL_perc)), by=id_customer]

coldcold4 = d4[frequency<3|(id_customer == 3914)|(id_customer == 19332)]
RFM_ds_4 = anti_join(d4, coldcold4)
coldcold4 = coldcold4[,.(id_customer, promomean4=promomean, plmean4=plmean, clusters4 = "coldcold")]

id_active_4 = RFM_ds_4[,id_customer]
id_active_4 = sort(id_active_4)
Reg_ds_4 = data.table(id_customer = id_active_4, MeanInt = 0, StdInt = 0)

#Interpurchase time 
for (i in id_active_4) {
  aux = data.frame(date = ds_4[ds_4$id_customer == i, "date"])
  aux = aux %>% arrange(date)
  aux_reg = mean(diff(aux$date))
  aux_std = sd(diff(aux$date))
  Reg_ds_4[Reg_ds_4$id_customer == i, "MeanInt"] = aux_reg
  Reg_ds_4[Reg_ds_4$id_customer == i, "StdInt"] = aux_std
}

RFM_ds_4 = RFM_ds_4[Reg_ds_4, on =.(id_customer=id_customer)] 
RFM_ds_4 = RFM_ds_4[,CV := StdInt/MeanInt]

#same thresholds as the last period (6)

RFM_ds_4$scoreReg = as.numeric(ifelse(RFM_ds_4$CV <= UpperReg ,3 ,
                                      ifelse(RFM_ds_4$CV > UpperReg & RFM_ds_4$CV < LowerReg,2,1)))


RFM_ds_4$scoreFre = as.numeric (ifelse(RFM_ds_4$frequency <= LowerFreq , 1 ,
                                       ifelse(RFM_ds_4$frequency > LowerFreq & RFM_ds_4$frequency < UpperFreq, 2 , 3)))


RFM_ds_4$scoreMon = as.numeric(ifelse(RFM_ds_4$monetary <= LowerMon, 1 ,
                                      ifelse(RFM_ds_4$monetary > LowerMon & RFM_ds_4$monetary < UpperMon, 2, 3)))


RFM_ds_4$Segments = as.factor(ifelse(RFM_ds_4$scoreFre == 3  & RFM_ds_4$scoreMon == 3 & (RFM_ds_4$scoreReg == 3|RFM_ds_4$scoreReg == 2), "champion",
                                     ifelse((RFM_ds_4$scoreFre == 3|RFM_ds_4$scoreFre == 2) & RFM_ds_4$scoreMon == 3 & RFM_ds_4$scoreReg == 1, "sensitive",
                                            ifelse(RFM_ds_4$scoreFre == 2  & RFM_ds_4$scoreMon == 3 & (RFM_ds_4$scoreReg == 2|RFM_ds_4$scoreReg == 3), "star",
                                                   ifelse(RFM_ds_4$scoreFre == 1 & RFM_ds_4$scoreMon == 1 & RFM_ds_4$scoreReg == 1, "frozen",
                                                          ifelse(RFM_ds_4$scoreFre == 1 & RFM_ds_4$scoreMon == 1 & (RFM_ds_4$scoreReg == 3|RFM_ds_4$scoreReg == 2), "precise",
                                                                 ifelse(RFM_ds_4$scoreFre == 1 & (RFM_ds_4$scoreMon == 3|RFM_ds_4$scoreMon == 2) & RFM_ds_4$scoreReg ==1, "bomb",     
                                                                        ifelse(RFM_ds_4$scoreFre == 1 & (RFM_ds_4$scoreMon == 2|RFM_ds_4$scoreMon == 3) & (RFM_ds_4$scoreReg == 3|RFM_ds_4$scoreReg == 2), "stock buyer", 
                                                                               ifelse(RFM_ds_4$scoreFre == 2 & RFM_ds_4$scoreMon == 2 & (RFM_ds_4$scoreReg == 2|RFM_ds_4$scoreReg == 3) ,"average",     
                                                                                      ifelse((RFM_ds_4$scoreFre == 2 |RFM_ds_4$scoreFre == 3) & (RFM_ds_4$scoreMon == 2|RFM_ds_4$scoreMon == 1) & RFM_ds_4$scoreReg == 1, "needed_attention", 
                                                                                             ifelse(RFM_ds_4$scoreFre == 2 & RFM_ds_4$scoreMon == 1 & (RFM_ds_4$scoreReg == 2 | RFM_ds_4$scoreReg == 3), "single",     
                                                                                                           ifelse(RFM_ds_4$scoreFre == 3 & (RFM_ds_4$scoreMon == 1|RFM_ds_4$scoreMon == 2) &  (RFM_ds_4$scoreReg == 2|RFM_ds_4$scoreReg == 3), "promising", "outsider"))))))))))))



table(RFM_ds_4$Segments) 


# RegFM_5 September October ----

d5 = ds_5[, .(frequency=.N, monetary=sum(net_sales), promomean=mean(promo_perc), plmean=mean(PL_perc)), by=id_customer]

coldcold5 = d5[frequency<3|id_customer==3705|id_customer==14998]
RFM_ds_5 = anti_join(d5, coldcold5)
coldcold5 = coldcold5[,.(id_customer, promomean5=promomean, plmean5=plmean, clusters5 = "coldcold")]

id_active_5 = RFM_ds_5[,id_customer]
id_active_5 = sort(id_active_5)
Reg_ds_5 = data.table(id_customer = id_active_5, MeanInt = 0, StdInt = 0)

#Interpurchase time in ds_1t
for (i in id_active_5) {
  aux = data.frame(date = ds_5[ds_5$id_customer == i, "date"])
  aux = aux %>% arrange(date)
  aux_reg = mean(diff(aux$date))
  aux_std = sd(diff(aux$date))
  Reg_ds_5[Reg_ds_5$id_customer == i, "MeanInt"] = aux_reg
  Reg_ds_5[Reg_ds_5$id_customer == i, "StdInt"] = aux_std
}

RFM_ds_5 = RFM_ds_5[Reg_ds_5, on =.(id_customer=id_customer)] 
RFM_ds_5 = RFM_ds_5[,CV := StdInt/MeanInt]

#same thresholds as the last period (6)

RFM_ds_5$scoreReg = as.numeric(ifelse(RFM_ds_5$CV <= UpperReg ,3 ,
                                      ifelse(RFM_ds_5$CV > UpperReg & RFM_ds_5$CV < LowerReg,2,1)))


RFM_ds_5$scoreFre = as.numeric (ifelse(RFM_ds_5$frequency <= LowerFreq , 1 ,
                                       ifelse(RFM_ds_5$frequency > LowerFreq & RFM_ds_5$frequency < UpperFreq, 2 , 3)))


RFM_ds_5$scoreMon = as.numeric(ifelse(RFM_ds_5$monetary <= LowerMon, 1 ,
                                      ifelse(RFM_ds_5$monetary > LowerMon & RFM_ds_5$monetary < UpperMon, 2, 3)))


RFM_ds_5$Segments = as.factor(ifelse(RFM_ds_5$scoreFre == 3  & RFM_ds_5$scoreMon == 3 & (RFM_ds_5$scoreReg == 3|RFM_ds_5$scoreReg == 2), "champion",
                                     ifelse((RFM_ds_5$scoreFre == 3|RFM_ds_5$scoreFre == 2) & RFM_ds_5$scoreMon == 3 & RFM_ds_5$scoreReg == 1, "sensitive",
                                            ifelse(RFM_ds_5$scoreFre == 2  & RFM_ds_5$scoreMon == 3 & (RFM_ds_5$scoreReg == 2|RFM_ds_5$scoreReg == 3), "star",
                                                   ifelse(RFM_ds_5$scoreFre == 1 & RFM_ds_5$scoreMon == 1 & RFM_ds_5$scoreReg == 1, "frozen",
                                                          ifelse(RFM_ds_5$scoreFre == 1 & RFM_ds_5$scoreMon == 1 & (RFM_ds_5$scoreReg == 3|RFM_ds_5$scoreReg == 2), "precise",
                                                                 ifelse(RFM_ds_5$scoreFre == 1 & (RFM_ds_5$scoreMon == 3|RFM_ds_5$scoreMon == 2) & RFM_ds_5$scoreReg ==1, "bomb",     
                                                                        ifelse(RFM_ds_5$scoreFre == 1 & (RFM_ds_5$scoreMon == 2|RFM_ds_5$scoreMon == 3) & (RFM_ds_5$scoreReg == 3|RFM_ds_5$scoreReg == 2), "stock buyer", 
                                                                               ifelse(RFM_ds_5$scoreFre == 2 & RFM_ds_5$scoreMon == 2 & (RFM_ds_5$scoreReg == 2|RFM_ds_5$scoreReg == 3) ,"average",     
                                                                                      ifelse((RFM_ds_5$scoreFre == 2 |RFM_ds_5$scoreFre == 3) & (RFM_ds_5$scoreMon == 2|RFM_ds_5$scoreMon == 1) & RFM_ds_5$scoreReg == 1, "needed_attention", 
                                                                                             ifelse(RFM_ds_5$scoreFre == 2 & RFM_ds_5$scoreMon == 1 & (RFM_ds_5$scoreReg == 2 | RFM_ds_5$scoreReg == 3), "single",     
                                                                                                           ifelse(RFM_ds_5$scoreFre == 3 & (RFM_ds_5$scoreMon == 1|RFM_ds_5$scoreMon == 2) &  (RFM_ds_5$scoreReg == 2|RFM_ds_5$scoreReg == 3), "promising", "outsider"))))))))))))


table(RFM_ds_5$Segments) 


# Final database ----

fwrite(RFM_ds_6, "RFM_ds_6.csv")
fwrite(RFM_ds_1, "RFM_ds_1.csv")
fwrite(RFM_ds_2, "RFM_ds_2.csv")
fwrite(RFM_ds_3, "RFM_ds_3.csv")
fwrite(RFM_ds_4, "RFM_ds_4.csv")
fwrite(RFM_ds_5, "RFM_ds_5.csv")
fwrite(ds_transaction_cleaned, "ds_transaction_cleaned.csv")
fwrite(ds_transaction_2122, "ds_transaction_2122.csv")
fwrite(ds_6, "ds_6.csv")

# Updating RFM_ds_i (considering just useful features: id_customer, promomean, plmean, segments)
RFM_ds_1 = RFM_ds_1[,.(id_customer=as.integer(id_customer), promomean1=promomean, plmean1=plmean, clusters1 = Segments)]
RFM_ds_2 = RFM_ds_2[,.(id_customer=as.integer(id_customer), promomean2=promomean, plmean2=plmean, clusters2 = Segments)]
RFM_ds_3 = RFM_ds_3[,.(id_customer=as.integer(id_customer), promomean3=promomean, plmean3=plmean, clusters3 = Segments)]
RFM_ds_4 = RFM_ds_4[,.(id_customer=as.integer(id_customer), promomean4=promomean, plmean4=plmean, clusters4 = Segments)]
RFM_ds_5 = RFM_ds_5[,.(id_customer=as.integer(id_customer), promomean5=promomean, plmean5=plmean, clusters5 = Segments)]
RFM_ds_6 = RFM_ds_6[,.(id_customer=as.integer(id_customer), promomean6=promomean, plmean6=plmean, clusters6 = Segments)]

RFM_ds_1 = rbind(RFM_ds_1, coldcold1)
RFM_ds_2 = rbind(RFM_ds_2, coldcold2)
RFM_ds_3 = rbind(RFM_ds_3, coldcold3)
RFM_ds_4 = rbind(RFM_ds_4, coldcold4)
RFM_ds_5 = rbind(RFM_ds_5, coldcold5)
RFM_ds_6 = rbind(RFM_ds_6, coldcold6)

# subset of the customers (excluding those NEW 2021/22 and hothot )
final_customer = ds_transaction_cleaned[, .(frequency_annual=.N, monetary_annual=sum(net_sales), promomean_annual=mean(promo_perc), plmean_annual=mean(PL_perc)), by=id_customer]
final_customer = ds_customer[final_customer, on = .(id_customer) ]
final_customer = RFM_ds_6[final_customer, on = .(id_customer), nomatch=NA]
final_customer = RFM_ds_5[final_customer, on = .(id_customer), nomatch=NA]
final_customer = RFM_ds_4[final_customer, on = .(id_customer), nomatch=NA]
final_customer = RFM_ds_3[final_customer, on = .(id_customer), nomatch=NA]
final_customer = RFM_ds_2[final_customer, on = .(id_customer), nomatch=NA]
final_customer = RFM_ds_1[final_customer, on = .(id_customer), nomatch=NA]

setorder(final_customer, id_customer)

final_customer[is.na(final_customer$promomean1),2] = 0
final_customer[is.na(final_customer$promomean2),5] = 0
final_customer[is.na(final_customer$promomean3),8] = 0
final_customer[is.na(final_customer$promomean4),11] = 0
final_customer[is.na(final_customer$promomean5),14] = 0
final_customer[is.na(final_customer$promomean6),17] = 0

final_customer[is.na(final_customer$plmean1),3] = 0
final_customer[is.na(final_customer$plmean2),6] = 0
final_customer[is.na(final_customer$plmean3),9] = 0
final_customer[is.na(final_customer$plmean4),12] = 0
final_customer[is.na(final_customer$plmean5),15] = 0
final_customer[is.na(final_customer$plmean6),18] = 0

final_customer[is.na(final_customer$clusters1),4] = "nonCustomer"
final_customer[is.na(final_customer$clusters2),7] = "nonCustomer"
final_customer[is.na(final_customer$clusters3),10] = "nonCustomer"
final_customer[is.na(final_customer$clusters4),13] = "nonCustomer"
final_customer[is.na(final_customer$clusters5),16] = "nonCustomer"
final_customer[is.na(final_customer$clusters6),19] = "nonCustomer"

fwrite(final_customer, "final_customer.csv")

## Longitudinal Analysis ----

#dataset with RegFM, taking cluster size averages of frequency, monetary and regularity
REGFM6_repr = RFM_ds_6[,.(frequency= mean(frequency), monetary = mean(monetary), Regularity= mean(CV), size = .N), by= Segments]

clust1 = final_customer[,.(number= .N, plmean = mean(plmean1), promomean=mean(promomean1), avgAge=mean(age)), by=clusters1]
clust2 = final_customer[,.(number= .N, plmean = mean(plmean2), promomean=mean(promomean2), avgAge=mean(age)), by=clusters2]
clust3 = final_customer[,.(number= .N, plmean = mean(plmean3), promomean=mean(promomean3), avgAge=mean(age)), by=clusters3]
clust4 = final_customer[,.(number= .N, plmean = mean(plmean4), promomean=mean(promomean4), avgAge=mean(age)), by=clusters4]
clust5 = final_customer[,.(number= .N, plmean = mean(plmean5), promomean=mean(promomean5), avgAge=mean(age)), by=clusters5]
clust6 = final_customer[,.(number= .N, plmean = mean(plmean6), promomean=mean(promomean6), avgAge=mean(age)), by=clusters6]
setorder(clust1, clusters1)
setorder(clust2, clusters2)
setorder(clust3, clusters3)
setorder(clust4, clusters4)
setorder(clust5, clusters5)
setorder(clust6, clusters6)

#Asses cluster size of each cluster for each period over the whole year
population = data.table()
population = population[,.(cluster=clust1$clusters1, number1=clust1$number,number2=clust2$number,number3=clust3$number,
                           number4=clust4$number,number5=clust5$number,number6=clust6$number)]


# For each customer over the year it is possible to verify how his behaviour changed and how this translates in clusters belonging

Longitudinal = data.table()
Longitudinal= Longitudinal[,.(id_customer= final_customer$id_customer, P1= final_customer$clusters1, P2= final_customer$clusters2,
                              P3= final_customer$clusters3,P4= final_customer$clusters4,P5= final_customer$clusters5, P6= final_customer$clusters6)]



#Segments profitability along the year, and for each period. Profitability has been assessed
#both in terms of average monetary value per customer, and as revenue stream

MON_segments1 = RFM_ds_1[,.(sumMon1 = sum(monetary)), by=Segments]
MON_segments2 = RFM_ds_2[,.(sumMon2 = sum(monetary)), by=Segments]
MON_segments3 = RFM_ds_3[,.(sumMon3 = sum(monetary)), by=Segments]
MON_segments4 = RFM_ds_4[,.(sumMon4 = sum(monetary)), by=Segments]
MON_segments5 = RFM_ds_5[,.(sumMon5 = sum(monetary)), by=Segments]
MON_segments6 = RFM_ds_6[,.(sumMon6 = sum(monetary)), by=Segments]
setorder(MON_segments1, Segments)
setorder(MON_segments2, Segments)
setorder(MON_segments3, Segments)
setorder(MON_segments4, Segments)
setorder(MON_segments5, Segments)
setorder(MON_segments6, Segments)

Profitability = data.table()
Profitability = Profitability[,.(cluster = MON_segments1$Segments,Mon1=MON_segments1$sumMon1,Mon2=MON_segments2$sumMon2,Mon3=MON_segments3$sumMon3,Mon4=MON_segments4$sumMon4
                                 ,Mon5=MON_segments5$sumMon5,Mon6=MON_segments6$sumMon6)]


Profitability = population[Profitability, on = .(cluster)]
Profitability$Year = rowSums(Profitability[,8:13])
Profitability$totalN = rowSums(Profitability[,2:7])

Profitability$unitaryY = Profitability$Year/Profitability$totalN
total_turnover = sum(Profitability$Year)
Profitability$turnoverperc = Profitability$Year / total_turnover

Profitability$unitary1= Profitability$Mon1/Profitability$number1
Profitability$unitary2=Profitability$Mon2/Profitability$number2
Profitability$unitary3=Profitability$Mon3/Profitability$number3
Profitability$unitary4=Profitability$Mon4/Profitability$number4
Profitability$unitary5=Profitability$Mon5/Profitability$number5
Profitability$unitary6=Profitability$Mon6/Profitability$number6

# average Private label and Promo  over the year for each cluster

long_plmean= data.table()
long_plmean = long_plmean[,.(cluster=clust1$clusters1, Pl1= clust1$plmean,Pl2= clust2$plmean,Pl3= clust3$plmean,
                             Pl4= clust4$plmean,Pl5= clust5$plmean,Pl6=clust6$plmean)]
long_plmean = long_plmean[-7,]



long_Promomean=  data.table()
long_Promomean = long_Promomean[,.(cluster=clust1$clusters1, PM1= clust1$promomean,PM2= clust2$promomean,PM3= clust3$promomean,
                                   PM4= clust4$promomean,PM5= clust5$promomean,PM6=clust6$promomean)]
long_Promomean = long_Promomean[-7,]


#Champions longitudinal analysis to build the intracluster perspective

Champions_long = data.table(id_customer = Longitudinal$id_customer)
Champions_long = cbind(Champions_long, data.table(ifelse(Longitudinal[,2:7] == "champion", 1, 0)))
Champions_long$aux = rowSums(Champions_long[,2:7])
Champions_long = Champions_long[Champions_long$aux != 0, 1:7]

#Stars longitudinal analysis to build the intracluster perspective

stars_long = data.table(id_customer = Longitudinal$id_customer)
stars_long = cbind(stars_long, data.table(ifelse(Longitudinal[,2:7] == "star", 1, 0)))
stars_long$aux = rowSums(stars_long[,2:7])
stars_long = stars_long[stars_long$aux != 0, 1:7]

#Sensitive longitudinal analysis to build the intracluster perspective

sensitive_long = data.table(id_customer = Longitudinal$id_customer)
sensitive_long = cbind(sensitive_long, data.table(ifelse(Longitudinal[,2:7] == "sensitive", 1, 0)))
sensitive_long$aux = rowSums(sensitive_long[,2:7])
sensitive_long = sensitive_long[sensitive_long$aux != 0, 1:7]


## Anova Analysis on Promo and Private Label ----

#Private label and promo analysis on the last period , associating score for each customer belonging to a specific cluster
#to assess tendency of the customer to purchase private label products and discounted product

PL_PM6 = data.table()
PL_PM6 = final_customer[,. ( PM6 = promomean6,PL6=plmean6, cluster = clusters6), by = id_customer]


quantile(PL_PM6$PM6, probs= seq(0,1, 0.01:0.99)) # 0.20 H , 0.08 L
quantile(PL_PM6$PL6, probs= seq(0,1, 0.01:0.99)) #0.2L, 0.40 H

UpperPM6 = 0.2
LowerPM6 =  0.08
UpperPL6 =  0.4
LowerPL6 = 0.2

PL_PM6$scorePM6 = as.numeric(ifelse(PL_PM6$PM6 <= LowerPM6 ,1 ,
                                    ifelse(PL_PM6$PM6 > LowerPM6 & PL_PM6$PM6 < UpperPM6,2,3)))

PL_PM6$scorePL6 = as.numeric(ifelse(PL_PM6$PL6 <= LowerPL6 ,1 ,
                                    ifelse(PL_PM6$PL6 > LowerPL6 & PL_PM6$PL6 < UpperPL6,2,3)))


table(PL_PM6$scorePM6)
table(PL_PM6$scorePL6)


#Double two way anova and slicing procedure on the 3 most profitable ones

twoway = PL_PM6[RFM_ds_6, on = .(id_customer), nomatch=NA]

anovaPL = aov(monetary~ scorePL6*Segments, data=twoway)
anovaPM = aov(monetary~ scorePM6*Segments, data=twoway)

summary(anovaPL)
summary(anovaPM)


twoway1 = subset(twoway, Segments == "star" | Segments == "sensitive" | Segments == "champion")
twoway1$scorePL6 = as.factor(twoway1$scorePL6)
twoway1$scorePM6 = as.factor(twoway1$scorePM6)
twoway1$cluster =as.factor(twoway1$cluster)

interaction.plot(twoway1$scorePL6,twoway1$Segments,twoway1$monetary, trace.label = deparse(substitute(Segments)), type = "l", ylim = c(700,1200),
                 xlab = deparse(substitute(ScorePL)), ylab = deparse(substitute(Monetary)), col = c("cadetblue","cadetblue1","cadetblue2","cadetblue3", "cadetblue4",
                                                                                                    "deepskyblue","deepskyblue1","deepskyblue2","deepskyblue3","deepskyblue4", "dodgerblue","dodgerblue1"))
interaction.plot(twoway1$scorePM6,twoway1$Segments,twoway1$monetary, trace.label = deparse(substitute(Segments)), type = "l", ylim = c(700,1200),
                 xlab = deparse(substitute(ScorePM)), ylab = deparse(substitute(Monetary)), col = c("cadetblue","cadetblue1","cadetblue2","cadetblue3", "cadetblue4",
                                                                                                    "deepskyblue","deepskyblue1","deepskyblue2","deepskyblue3","deepskyblue4", "dodgerblue","dodgerblue1"))
interaction.plot(way$Segments,way$scorePL6,way$monetary)

ds_anovaPM = twoway1[,.(monetary=mean(monetary)), by=.(cluster, scorePM6)]
ds_anovaPL = twoway1[,.(monetary = mean(monetary)), by=.(cluster, scorePL6)]

emmPL= emmeans(anovaPL, specs = "scorePL6", by="Segments")
pairs(emmPL)
emmPM= emmeans(anovaPM, specs = "scorePM6", by="Segments")
pairs(emmPM)

## Analysis on NEW Customers ----
customer_info = ds_transaction_2122[,.(frequency=.N, monetary=sum(net_sales), first = min(date), last = max(date)), by= id_customer]
customer_info = customer_info[frequency > quantile(customer_info$frequency, prob=0.10), ] #eliminated those new customers with less than 5 transactions in their activity period
id_new = unique(customer_info$id_customer)

ds_transaction = ds_transaction_2122[ds_transaction_2122$id_customer %in% id_new]
ds_new_customer = ds_transaction[,c(1:3,8:10)]
setorder(ds_new_customer, date)

#Initializing: preparing the dataset for the for cycle
input_surv = data.table(id_customer = id_new) 

for (j in 1:length(id_new)) {
  id = id_new[j]
  prova=ds_new_customer[id_customer == id,]
  setorder(prova, date)
  prova[, `:=` (days= as.integer(date-min(date)))]
  prova = prova[,.(net_sales=sum(net_sales), PL_perc = mean(PL_perc), promo_perc=mean(promo_perc)), by=.(id_customer, days)]
  for (i in prova$days) {
    index2 = which(prova$days==i)
    aux = prova$days[index2] - 30
    index1 = ifelse(aux>0, max(which(prova$days<aux)),0)
    x = sum(prova[(index1+1):index2]$net_sales)
    if (x>311){ # threshold in monetary for being high spending (over the 3rd RFM score )
      input_surv[input_surv$id_customer == id, "highspending"] = T
      input_surv[input_surv$id_customer == id, "time"] = prova[index2]$days
      input_surv[input_surv$id_customer == id, "promomean"] = mean(prova[(index1+1):index2]$promo_perc)
      input_surv[input_surv$id_customer == id, "plmean"] = mean(prova[(index1+1):index2]$PL_perc)
      break
    }
    else{
      input_surv[input_surv$id_customer == id, "highspending"] = F
      input_surv[input_surv$id_customer == id, "time"] = prova[index2]$days
      input_surv[input_surv$id_customer == id, "promomean"] = mean(prova[(index1+1):index2]$promo_perc)
      input_surv[input_surv$id_customer == id, "plmean"] = mean(prova[(index1+1):index2]$PL_perc)
    }
  }
}


input_surv$highspending = as.factor(input_surv$highspending)
survival = ds_customer[input_surv, on=.(id_customer)]
survival = survival[,.(time, highspending, age,gender, promomean, plmean)] #complete dataset for the survival with the possible covariates.
survival$gender = as.factor(survival$gender)

survival_clean = survival[time>0,] # eliminating customers that became highspending at time 0 

quantile(survival_clean$age, prob=seq(0,1,0.01:0.99))
quantile(survival_clean$plmean, prob=seq(0,1,0.01:0.99))
quantile(survival_clean$promomean, prob=seq(0,1,0.01:0.99))

#Trying to fit Kaplan Meier, with no covariates.
kaplanmeier = survfit(Surv(time, highspending == T)~1, data=survival_clean)
autoplot(kaplanmeier)
quantile(kaplanmeier) # 50% of new customers become highspending before 167 days
mean(kaplanmeier[[2]]) # on average it takes 180 days to become highspending


survival_clean$scorePL = as.factor(ifelse(survival_clean$plmean<=0.25, "L", "H"))

kaplanmeier2 = survfit(Surv(time, highspending == T)~scorePL, data=survival_clean)
quantile(kaplanmeier2)
ggsurvplot(kaplanmeier2, size=0.5, legend.labs=c("High PL","Low PL"), conf.int = F, xlab="Time in days", xlim=c(0,350),
           ylim=c(.25,1), break.time.by= 50, ggtheme = theme_light(), censor.size=1.75,
           palette = c("#df0000", "#ff7b5a"),surv.median.line = "hv")


#Trying weibull
ggsurvplot(kaplanmeier, data=survival_clean, fun="cloglog") # can test visually whether Weibull model makes sense: plotting ln(-ln(S(t)))
# since it is not a straight line it does not make sense.
survreg = survreg(Surv(time, highspending == T)~1, data=survival_clean,dist="weibull")
summary(survreg) #intercept= 5.71 , scale= 1.5
AIC(survreg)#6410
BIC(survreg)#6420.26
mrlw = exp(survreg$coefficients[[1]])*gamma(1+survreg$scale)

#Trying lognormal
survreg2 = survreg(Surv(time, highspending == T)~1, data=survival_clean,dist="lognormal")
summary(survreg2) #intercept= 5.04 , scale= 1.89
AIC(survreg2)#6291.063
BIC(survreg2)#6300.767
mrll = exp(survreg2$coefficients[[1]]+0.5*survreg2$scale^2)

## Value-Risk Matrix ----

# Risk assessment
analysis_date = as.Date.character(x = "2021-12-31", format = "%Y-%m-%d")
RFM6 = ds_6[,.(Rec = as.numeric(analysis_date - max(date))), by = id_customer] # calculating the recency

RFM6 = RFM6[RFM_ds_6, on =.(id_customer=id_customer)] # to eliminate coldcold customer (not present in RFM_ds_6)
na_index= which(is.na(RFM6$Segments == T)) # indeed, no NA

RFM6$MeanInt = as.numeric(RFM6$MeanInt)
RFM6$StdInt = as.numeric(RFM6$StdInt)
RFM6$Rec = as.numeric(RFM6$Rec)

#Computing the risk coefficient
RFM6[RFM6$StdInt==0,"StdInt"] = 0.000001

RFM6$Risk = (RFM6$Rec - RFM6$MeanInt)/RFM6$StdInt
RFM6[is.na(RFM6)] = 0

# the risk factor indicates how many standard deviations the recency is away from the mean interpurchase time. 
# PROBLEM: 157 customer with more than 10 of risk, 109 with less than -10
# Solution: put all the >10 at 10, all the <-10 at -10 

RFM6[RFM6$Risk>10,"Risk"] = 10
RFM6[RFM6$Risk<(-10),"Risk"] = -10
hist(RFM6$Risk)
quantile(RFM6$Risk, probs= seq(0,1, 0.01:0.99))
# 87% of customers are totally normal: they have a recency that is lower than MeanInt + 1*sigma
# the remaining 13% is at risk 

#VALUE ASSESMENT
RFM6$MeanMonetary = RFM6$monetary
hist(RFM6$MeanMonetary)
quantile(RFM6$MeanMonetary, probs= seq(0,1, 0.01:0.99))

for (i in 1:21344){
  RFM6$m[i] = ecdf(RFM6$MeanMonetary)(RFM6$MeanMonetary[i])
  RFM6$f[i] = ecdf(RFM6$frequency)(RFM6$frequency[i])
  RFM6$value[i]=RFM6$m[i]*RFM6$f[i]
}

hist(RFM6$value)
quantile(RFM6$value, probs= seq(0,1, 0.01:0.99))
#75% of customers is considered low value, the remaining 25% have an high value coefficient

# ANALYSIS
# Divest: value < 0.5, risk > 1
# Aggressively retain: value > 0.5, risk > 1
# Maintain: value > 0.5, risk < 1
# Grow: value < 0.5, risk < 1

RFM6$Action = as.factor(ifelse(RFM6$Risk <= 1 & RFM6$value <= 0.5, "Grow",
                               ifelse(RFM6$Risk<=1 & RFM6$value > 0.5, "Maintain",
                                      ifelse(RFM6$Risk>1 & RFM6$value<=0.5, "Divest", "Aggresively retain"))))

export = RFM6[,.(id_customer, clusters=Segments, Action, Risk, value)]

install.packages('circlize')
library(circlize)
plot(export$Risk, export$value, col = rand_color(10))
abline(v=1)
abline(h=0.5)

aggresivelyretain=RFM6[Action=='Aggresively retain',.(number=.N), by=Segments]
divest=RFM6[Action=='Divest',.(number=.N), by=Segments]
maintain=RFM6[Action=='Maintain',.(number=.N), by=Segments]
grow=RFM6[Action=='Grow',.(number=.N), by=Segments]


## CHURN ANALYSIS ----

# ds_transaction_cleaned = fread("/Users/Jacopo/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Analytics for Business Lab/FINAL FOLDER/Datasets_final/ds_transaction_cleaned.csv")
# final_customer = fread("/Users/Jacopo/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Analytics for Business Lab/FINAL FOLDER/Datasets_final/final_customer.csv")

ds_YEAR = ds_transaction_cleaned[, .(frequency = .N), by = id_customer]
quantile(ds_YEAR$frequency, probs = seq(0,1, 0.01:0.99))

to_drop = quantile(ds_YEAR$frequency, probs = 0.25) # churn analysis is significant only for customer with a certain regularity
ds_YEAR = ds_YEAR[ds_YEAR$frequency >= to_drop,] #rm customer with too few transactions

id_churn = sort(as.numeric(ds_YEAR$id_customer))
rm(ds_YEAR)

ds_churn = ds_transaction_cleaned[ds_transaction_cleaned$id_customer %in% id_churn, c(1,3,8)]
end_year = as.Date("2021-12-31")

transaction_list = list()
for (i in 1:length(id_churn)) {
  
  ds_aux = ds_churn[ds_churn$id_customer == id_churn[i],]
  ds_aux = ds_aux[order(ds_aux$date),]
  
  aux_date = c(ds_aux$date, end_year)
  aux_mon = ds_aux$net_sales
  aux_interpurchase = as.numeric(diff(aux_date))
  
  list_aux = list(id_customer = id_churn[i], date = aux_date, interpurchase = aux_interpurchase, monetary = aux_mon)
  transaction_list[[i]] = list_aux
}

# list.save(transaction_list, "transaction_list_new.yaml")

# transaction_list = list.load("/Users/Jacopo/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Analytics for Business Lab/FINAL FOLDER/Datasets_final/transaction_list_new.yaml")
# library(zoo)
# for (i in 1:length(transaction_list)) {
#   transaction_list[[i]]$date = as.Date(transaction_list[[i]]$date)
# }

#algorithm for churn analysis

timeframe = c(seq(30,120,5))
results = data.frame(time = timeframe, Accuracy = NA, Precision = NA)
confusionMatrix_list = list()
for (z in 1:length(timeframe)){
  t = timeframe[z]
  training_set = data.frame(id_customer = id_churn, Pred_churn = NA, Actual_churn = NA)
  
  for(i in 1:length(id_churn)){
    id = id_churn[i]
    if (sum(transaction_list[[i]]$interpurchase >= t)!=0){
      
      training_set[training_set$id_customer == id, "Pred_churn"] = T
      index = max(which((transaction_list[[i]]$interpurchase >= t) == T))
      
      if (index == length(transaction_list[[i]]$interpurchase)){ # -> checlk on recency value
        training_set[training_set$id_customer == id, "Actual_churn"] = T
      }
      else{
        purchase_lenght = (length(transaction_list[[i]]$date) - 1) #togliere l'ultima data fittizia 31/12
        purchase_pre = length(transaction_list[[i]]$date[1:index])
        purchase_post = length(transaction_list[[i]]$date[(index+1):purchase_lenght])
        mon_pre = sum(transaction_list[[i]]$monetary[1:index])
        mon_post = sum(transaction_list[[i]]$monetary[(index+1):purchase_lenght])
        
        interp_length = length(transaction_list[[i]]$interpurchase)-1
        days_pre = sum(transaction_list[[i]]$interpurchase[1:(index-1)])
        days_post = sum(transaction_list[[i]]$interpurchase[(index+1):interp_length])
        
        grocery_day_defection = (purchase_post/days_post <= 0.5*purchase_pre/days_pre)
        monetary_day_defection = (mon_post/days_post <= 0.5*mon_pre/days_pre)
        
        if (monetary_day_defection == T){
          training_set[training_set$id_customer == id, "Actual_churn"] = T
        }
        else{
          training_set[training_set$id_customer == id, "Actual_churn"] = F
        }
      }
    }
    else {
      training_set[training_set$id_customer == id, "Pred_churn"] = F
      training_set[training_set$id_customer == id, "Actual_churn"] = F
    }
  }
  
  classifier = table(training_set$Pred_churn,training_set$Actual_churn)
  
  list_aux = list(timeframe = t, ConfMatrix = classifier)
  confusionMatrix_list[[z]] = list_aux
  
  TP = classifier[2,2]
  FP = classifier[2,1]
  FN = classifier[1,2]
  TN = classifier[1,1]
  
  results[results$time == t, 'Accuracy'] <-(TP+TN)/(TP+TN+FP+FN)
  results[results$time == t, 'Precision'] <- TP/(TP+FP)
}

# fwrite(results, "algorithm_performance.csv")

#Timeframe selected
t_best = 60
training_def = data.frame(id_customer = id_churn, Pred_churn = NA, Actual_churn = NA, is_beginning = NA)
for(i in 1:length(id_churn)){
  id = id_churn[i]
  if (sum(transaction_list[[i]]$interpurchase >= t_best)!=0){
    
    training_def[training_def$id_customer == id, "Pred_churn"] = T
    index = max(which((transaction_list[[i]]$interpurchase >= t_best) == T))
    
    training_def[training_def$id_customer == id, "is_beginning"] = ifelse(index <= 5, T, F)
    
    training_def[training_def$id_customer == id, "Churn_date"] = transaction_list[[i]]$date[[index]]
    training_def[training_def$id_customer == id, "Return_date"] = transaction_list[[i]]$date[[index+1]]
    
    if (index == length(transaction_list[[i]]$interpurchase)){
      training_def[training_def$id_customer == id, "Actual_churn"] = T
    }
    else{
      purchase_lenght = (length(transaction_list[[i]]$date) - 1) #togliere l'ultima data fittizia 31/12
      purchase_pre = length(transaction_list[[i]]$date[1:index])
      purchase_post = length(transaction_list[[i]]$date[(index+1):purchase_lenght])
      mon_pre = sum(transaction_list[[i]]$monetary[1:index])
      mon_post = sum(transaction_list[[i]]$monetary[(index+1):purchase_lenght])
      
      interp_length = length(transaction_list[[i]]$interpurchase)-1
      days_pre = sum(transaction_list[[i]]$interpurchase[1:(index-1)])
      days_post = sum(transaction_list[[i]]$interpurchase[(index+1):interp_length])
      
      grocery_day_defection = (purchase_post/days_post <= 0.5*purchase_pre/days_pre) #-> 50% drop
      monetary_day_defection = (mon_post/days_post <= 0.5*mon_pre/days_pre)
      
      if (monetary_day_defection == T){
        training_def[training_def$id_customer == id, "Actual_churn"] = T
      }
      else{
        training_def[training_def$id_customer == id, "Actual_churn"] = F
      }
    }
  }
  else {
    training_def[training_def$id_customer == id, "Pred_churn"] = F
    training_def[training_def$id_customer == id, "Actual_churn"] = F
  }
}

# Removing critical behaviours ("reacquisitions", "holidays", "beginning")
ds_first_purchase = data.table(id_customer = id_churn)
for (i in 1:length(id_churn)) {
  ds_first_purchase[ds_first_purchase$id_customer == transaction_list[[i]]$id_customer, "First_purchase"] = min(transaction_list[[i]]$date)
}

begin_year = as.Date("2021-01-01")
id_reacquisition = (ds_first_purchase[ds_first_purchase$First_purchase >= (begin_year + t_best),]$id_customer)

training_def = training_def[!(training_def$id_customer %in% id_reacquisition), ]

id_isBeginning = na.omit(training_def[training_def$is_beginning == T,]$id_customer)
training_def = training_def[!(training_def$id_customer %in% id_isBeginning), ]

Summer_start = as.Date("2021-06-05")
Summer_end = as.Date("2021-09-18")
Summer = c(Summer_start)
for (i in 1:as.numeric(Summer_end - Summer_start)) {
  Summer = c(Summer, (Summer[i] + 1))
}

id_holydays = na.omit(training_def[training_def$Churn_date %in% Summer & training_def$Return_date %in% Summer,]$id_customer)
training_def = training_def[!(training_def$id_customer %in% id_holydays),]

classifier = table(training_def$Pred_churn,training_def$Actual_churn)
TP = classifier[2,2]
FP = classifier[2,1]
FN = classifier[1,2]
TN = classifier[1,1]
Precision_final = (TP+TN)/(TP+TN+FP+FN)
Accuracy_final = TP/(TP+FP)

id_customer_logistic = training_def[,]$id_customer

transaction_list_final = list()
j = 1
for (i in 1:length(transaction_list)) {
  if (transaction_list[[i]]$id_customer %in% id_customer_logistic) {
    id_aux = transaction_list[[i]]$id_customer
    ds_aux = ds_transaction_cleaned[ds_transaction_cleaned$id_customer == id_aux, ]
    ds_aux = ds_aux[order(date), ]
    list_aux = list(id_customer = transaction_list[[i]]$id_customer, date = transaction_list[[i]]$date,
                    interpurchase = transaction_list[[i]]$interpurchase, monetary = transaction_list[[i]]$monetary,
                    promo_perc = ds_aux$promo_perc, num_item_TOT = ds_aux$num_item_TOT, num_item_PL = ds_aux$num_item_PL)
    transaction_list_final[[j]] = list_aux
    j = j+1
  }
}

length(transaction_list_final) == length(id_customer_logistic)

# write.table(training_def, "Lgr_reg_ds.txt", sep = ",", row.names = F)

ds_customer[, `:=` (membership_years = 2022 - first_purchase_year) ] # added membership years
ds_customer_churn = ds_customer[, .(id_customer, membership_years)]
ds_customer_churn = ds_customer_churn[ds_customer_churn$id_customer %in% id_customer_logistic,]
ds_customer_churn = ds_customer_churn[order(id_customer),]
ds_customer_logistic = data.table(id_customer = id_customer_logistic)
ds_customer_logistic[,"membership_years"] = ds_customer_churn$membership_years

ds_customer_logistic = ds_customer_logistic[training_def[training_def$id_customer %in% id_customer_logistic, c(1,3)], on = .(id_customer), nomatch=NA]

end_year = as.Date("2021-12-31")
for (i in 1:length(transaction_list_final)) {
  
  id_aux = transaction_list_final[[i]]$id_customer
  
  if (ds_customer_logistic$Actual_churn[i] == T) {
    
    interp_aux = transaction_list_final[[i]]$interpurchase[-length(transaction_list_final[[i]]$interpurchase)]
    recency_aux = transaction_list_final[[i]]$interpurchase[length(transaction_list_final[[i]]$interpurchase)]
    
    index = max(which((transaction_list_final[[i]]$interpurchase >= t_best) == T))
    
    lm_aux_interp = lm(interp_aux[1:(index-1)] ~ c(1:(index-1)))
    lm_aux_monetary = lm(transaction_list_final[[i]]$monetary[1:index] ~ c(1:index))
    
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "interp_coeff"] = lm_aux_interp$coefficients[2]
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "monetary_coeff"] = lm_aux_monetary$coefficients[2]
    
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "max_interp"] = max(interp_aux[1:(index-1)])
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "frequency"] = index
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "meanTicketSales"] = mean(transaction_list_final[[i]]$monetary[1:index])
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "meanTotItem"] = mean(transaction_list_final[[i]]$num_item_TOT[1:index])
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "meanPlItem"] = mean(transaction_list_final[[i]]$num_item_PL[1:index])
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "meanPromo"] = mean(transaction_list_final[[i]]$promo_perc[1:index])
  } 
  else {
    interp_aux = transaction_list_final[[i]]$interpurchase[-length(transaction_list_final[[i]]$interpurchase)]
    recency_aux = transaction_list_final[[i]]$interpurchase[length(transaction_list_final[[i]]$interpurchase)]
    
    aux_length = length(transaction_list_final[[i]]$interpurchase)
    
    lm_aux_interp = lm(transaction_list_final[[i]]$interpurchase[1:aux_length] ~ c(1:aux_length))
    lm_aux_monetary = lm(transaction_list_final[[i]]$monetary[1:(aux_length+1)] ~ c(1:(aux_length+1)))
    
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "interp_coeff"] = lm_aux_interp$coefficients[2]
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "monetary_coeff"] = lm_aux_monetary$coefficients[2]
    
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "max_interp"] = max(interp_aux)
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "frequency"] = length(transaction_list_final[[i]]$monetary)
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "meanTicketSales"] = mean(transaction_list_final[[i]]$monetary)
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "meanTotItem"] = mean(transaction_list_final[[i]]$num_item_TOT)
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "meanPlItem"] = mean(transaction_list_final[[i]]$num_item_PL)
    ds_customer_logistic[ds_customer_logistic$id_customer == id_aux, "meanPromo"] = mean(transaction_list_final[[i]]$promo_perc)
  }
}

ds_customer_logistic$Actual_churn = ifelse(ds_customer_logistic$Actual_churn == T, 1, 0)

# fwrite(ds_customer_logistic, "ds_customer_logistic.csv")

## Logistic regression ----

index_isChurn = which(ds_customer_logistic$Actual_churn == 1)
index_isnotChurn = which(ds_customer_logistic$Actual_churn == 0)

# over sapling and undersampling
set.seed(123) 
index_oversampling = sample(index_isChurn, replace = T, size = 1500)
index_undersampling = sample(index_isnotChurn, replace = F, size = 2500)
index_balanced =  c(index_oversampling, index_undersampling)

table(index_balanced)

ds_balanced = ds_customer_logistic[index_balanced,]

lgr_churn = glm(Actual_churn ~ ., data = ds_balanced[,-1], family = binomial(link = "logit"))
summary(lgr_churn)

#test presence of autocorrelation
res = lgr_churn$residuals
acf(res)    #high presence of autocorrelation
bgtest(lgr_churn, order = 1) 
#Given that the p-value is lower than ?? we reject the null hypothesis and thus there is autocorrelation in the model

#robust standard errors approach
Nwcov = vcovHAC(lgr_churn)
coeftest(lgr_churn, vcov = Nwcov)

#bonferroni approach
p_value = summary(lgr_churn)$coefficients[,"Pr(>|z|)"]
p.adjust(p_value)

#fitting the model with significant variable
lgr_churn = glm(Actual_churn ~ ., data = ds_balanced[,-c(1,2,5,6,8,9)], family = binomial(link = "logit"))
coeftest(lgr_churn, vcov = Nwcov)

summary(lgr_churn)

#calcolo il training error
classification = factor(ifelse(predict(lgr_churn, newdata = ds_balanced[,-c(1,5)], type='response') > 0.5,0,1), levels=c(0,1))  
(tab = table(classification, ds_balanced$Actual_churn))
(tab[2]+tab[3])/sum(tab)

## Decision tree method -----

ds_balanced$Actual_churn = as.factor(ds_balanced$Actual_churn)

library(rpart)
library(rpart.plot)
library(caret)

tr_fit = rpart(Actual_churn ~., data = ds_balanced[,-c(1:2, 8:9)], method="class")
rpart.plot(tr_fit)
 
# calculating the training error
tr_prob = predict(tr_fit, ds_balanced[,-c(1:2, 8:9)])
tr_pred = ifelse(tr_prob[,2] > 0.5,"1","0")
table(Predicted = tr_pred, Actual = ds_balanced$Actual_churn)

confusionMatrix( as.factor(tr_pred), as.factor(ds_balanced$Actual_churn), positive = "1" )

