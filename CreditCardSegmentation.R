rm(list=ls(all=T))
getwd()
setwd("C:/Users/ganes/Desktop/CreditCardSegmentationPro")


### Import File
credit_data <-read.csv("Credit-card-data.csv",stringsAsFactors = FALSE)

View(credit_data)

#                  ********OUTLIERS************

### Identifying Outliers
mystats <- function(x) {
  nummiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+2*s
  LC <- m-2*s
  outlierflag<- max>UC | min<LC
  return(c(n=n, nummiss=nummiss, outlierflag=outlierflag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}



### Let's create new variables

credit_data$Monthly_Avg_PURCHASES <- credit_data$PURCHASES/(credit_data$PURCHASES_FREQUENCY*credit_data$TENURE)
credit_data$Monthly_CASH_ADVANCE <- credit_data$CASH_ADVANCE/(credit_data$CASH_ADVANCE_FREQUENCY*credit_data$TENURE)

credit_data$LIMIT_USAGE <- credit_data$BALANCE/credit_data$CREDIT_LIMIT
credit_data$MIN_PAYMENTS_RATIO <- credit_data$PAYMENTS/credit_data$MINIMUM_PAYMENTS

write.csv(credit_data,"NewVariables.csv")



NumeVar <- c(
  "BALANCE",
  "BALANCE_FREQUENCY",
  "PURCHASES",
  "Monthly_Avg_PURCHASES",
  "ONEOFF_PURCHASES",
  "INSTALLMENTS_PURCHASES",
  "CASH_ADVANCE",
  "Monthly_CASH_ADVANCE",
  "PURCHASES_FREQUENCY",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "CASH_ADVANCE_FREQUENCY",
  "CASH_ADVANCE_TRX",
  "PURCHASES_TRX",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PAYMENTS",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "PRC_FULL_PAYMENT",
  "TENURE")

Outliers<-t(data.frame(apply(credit_data[NumeVar], 2, mystats)))
View(Outliers)

write.csv(Outliers,"Outliers.csv")


#    *********Outlier Treatment**********

credit_data$BALANCE[credit_data$BALANCE>5727.53]<-5727.53
credit_data$BALANCE_FREQUENCY[credit_data$BALANCE_FREQUENCY>1.3510787]<-1.3510787
credit_data$PURCHASES[credit_data$PURCHASES>5276.46]<-5276.46
credit_data$Monthly_Avg_PURCHASES[credit_data$Monthly_Avg_PURCHASES>800.03] <- 800.03
credit_data$ONEOFF_PURCHASES[credit_data$ONEOFF_PURCHASES>3912.2173709]<-3912.2173709
credit_data$INSTALLMENTS_PURCHASES[credit_data$INSTALLMENTS_PURCHASES>2219.7438751]<-2219.7438751
credit_data$CASH_ADVANCE[credit_data$CASH_ADVANCE>5173.1911125]<-5173.1911125
credit_data$Monthly_CASH_ADVANCE[credit_data$Monthly_CASH_ADVANCE>2558.53] <- 2558.53
credit_data$PURCHASES_FREQUENCY[credit_data$PURCHASES_FREQUENCY>1.2930919]<-1.2930919
credit_data$ONEOFF_PURCHASES_FREQUENCY[credit_data$ONEOFF_PURCHASES_FREQUENCY>0.7991299]<-0.7991299

credit_data$PURCHASES_INSTALLMENTS_FREQUENCY[credit_data$PURCHASES_INSTALLMENTS_FREQUENCY>1.1593329]<-1.1593329
credit_data$CASH_ADVANCE_FREQUENCY[credit_data$CASH_ADVANCE_FREQUENCY>0.535387]<-0.535387
credit_data$CASH_ADVANCE_TRX[credit_data$CASH_ADVANCE_TRX>16.8981202]<-16.8981202
credit_data$PURCHASES_TRX[credit_data$PURCHASES_TRX>64.4251306]<-64.4251306
credit_data$CREDIT_LIMIT[credit_data$CREDIT_LIMIT>11772.09]<-11772.09
credit_data$LIMIT_USAGE[credit_data$LIMIT_USAGE>1.1683] <- 1.1683
credit_data$PAYMENTS[credit_data$PAYMENTS>7523.26]<-7523.26
credit_data$MINIMUM_PAYMENTS[credit_data$MINIMUM_PAYMENTS>5609.1065423]<-5609.1065423
credit_data$MIN_PAYMENTS_RATIO[credit_data$MIN_PAYMENTS_RATIO>249.9239] <- 249.9239
credit_data$PRC_FULL_PAYMENT[credit_data$PRC_FULL_PAYMENT>0.738713]<-0.738713
credit_data$TENURE[credit_data$TENURE>14.19398]<-14.19398


#             *********MISSING VALUES**********

###### Missing Value Imputation with mean 
credit_data$MINIMUM_PAYMENTS[which(is.na(credit_data$MINIMUM_PAYMENTS))] <- 721.9256368
credit_data$CREDIT_LIMIT[which(is.na(credit_data$CREDIT_LIMIT))] <- 4343.62
credit_data$Monthly_Avg_PURCHASES[which(is.na(credit_data$Monthly_Avg_PURCHASES))] <-184.8991609
credit_data$Monthly_CASH_ADVANCE[which(is.na(credit_data$Monthly_CASH_ADVANCE))] <- 717.7235629
credit_data$LIMIT_USAGE[which(is.na(credit_data$LIMIT_USAGE))] <-0.3889264
credit_data$MIN_PAYMENTS_RATIO[which(is.na(credit_data$MIN_PAYMENTS_RATIO))]  <- 9.3500701


###### Checking Missing Value
checkMissingValues<-t(data.frame(apply(credit_data[NumeVar], 2, mystats)))
View(checkMissingValues)

write.csv(credit_data,"Missingvaluetreatment.csv")


###### Variable Reduction (Factor Analysis)

StepNums <- credit_data[NumeVar]
corrm<- cor(StepNums)    
View(corrm)

write.csv(corrm, "CorrelationMatrix.csv")

#scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE)   ### SCREE PLOT

eigen(corrm)$values

require(dplyr)

eigenvalues <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))

write.csv(eigenvalues, "EigenValuess.csv")


require(psych)
FA<-fa(r=corrm, 7, rotate="varmax", fm="ml")  


####sort loadings
FA_SORT<-fa.sort(FA)      
FA_SORT$loadings


Loading<-data.frame(FA_SORT$loadings[1:ncol(StepNums),])
write.csv(Loading, "loading2.csv")


########### standardizing the data
segmentPrepared <-credit_data[NumeVar]

segmentPrepared = scale(segmentPrepared)

write.csv(segmentPrepared, "StandardizedData.csv")


######building clusters using k-means clustering 

cluster3 <- kmeans(segmentPrepared,3)
cluster4 <- kmeans(segmentPrepared,4)
cluster5 <- kmeans(segmentPrepared,5)
cluster6 <- kmeans(segmentPrepared,6)

credit_New<-cbind(credit_data,km_clust_3=cluster3$cluster,km_clust_4=cluster4$cluster,km_clust_5=cluster5$cluster ,km_clust_6=cluster6$cluster)
View(credit_New)


####### Profiling

NumeVar2<- c(
  "Monthly_Avg_PURCHASES",
  "Monthly_CASH_ADVANCE",
  "CASH_ADVANCE",
  "CASH_ADVANCE_TRX",
  "CASH_ADVANCE_FREQUENCY",
  "ONEOFF_PURCHASES",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PAYMENTS",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "PURCHASES_FREQUENCY",
  "INSTALLMENTS_PURCHASES",
  "PURCHASES_TRX",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "BALANCE",
  "TENURE"
)

require(tables)
tt <-cbind(tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+
              factor(km_clust_6)~Heading()*length*All(credit_data[1]),
            data=credit_New),tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+
          factor(km_clust_6)~Heading()*mean*All(credit_data[NumeVar2]),
        data=credit_New))

tt_2 <- as.data.frame.matrix(tt)
View(tt_2)

rownames(tt_2)<-c(
  "ALL",
  "KM3_1",
  "KM3_2",
  "KM3_3",
  "KM4_1",
  "KM4_2",
  "KM4_3",
  "KM4_4",
  "KM5_1",
  "KM5_2",
  "KM5_3",
  "KM5_4",
  "KM5_5",
  "KM6_1",
  "KM6_2",
  "KM6_3",
  "KM6_4",
  "KM6_5",
  "KM6_6")


colnames(tt_2)<-c(
  "SEGMENT_SIZE",
  "Monthly_Avg_PURCHASES",
  "Monthly_CASH_ADVANCE",
  "CASH_ADVANCE",
  "CASH_ADVANCE_TRX",
  "CASH_ADVANCE_FREQUENCY",
  "ONEOFF_PURCHASES",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PAYMENTS",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "PURCHASES_FREQUENCY",
  "INSTALLMENTS_PURCHASES",
  "PURCHASES_TRX",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "BALANCE",
  "TENURE"
)

clusterProfilings <- t(tt_2)

write.csv(clusterProfilings,'clusterProfilings.csv')





