#importing the dataset
xdata <- read.csv("F:/projects/dbms/2014Q1-capitalbikeshare-tripdata.csv", na.strings=".")

#summary set for all variables in the dataset
summary(xdata)

#summary stats for selected variables
summary(xdata[c("Start.station.number","End.station.number")])

stat.freq<-table(xdata$Start.station.number)
barplot(stat.freq)
barplot(stat.freq[order(stat.freq,decreasing = T)])
stat.freq
max(stat.freq)     #frequncy of the station which is maximum used 

#using subset of dataset
xdata.sub1 <- subset(xdata, Duration > 3600, select = c("Duration","Member.type","Start.station","Start.station.number"))

#summary stats of subset where duration is greater than 1 hour
summary(xdata.sub1)

startstation.freq<-table(xdata.sub1$Start.station.number)
startstation.freq
barplot(startstation.freq)
max(startstation.freq)

duration.freq<-table(xdata.sub1$Duration)
duration.freq
barplot(xdata$Duration)
max(duration.freq)



#representing the data graphycally

#representing the data in histogram
#par(mfrow=c(4,2))
#par(mar = rep(2, 4))

hist(xdata.sub1$Duration)
hist(xdata.sub1$Start.station.number)
#hist(xdata.sub1$Member.type)

#representing as boxplot
boxplot(xdata.sub1$Duration)
boxplot(xdata.sub1$Start.station.number)

#REPRESENTING THE CATEGORIAL VARIABLE AS BARPLOT(Type of customer & bike number)

bike.freq<-table(xdata$Bike.number)
#bike.freq<-(bike.freq>"300")
barplot.default(bike.freq,bike.freq>250)
bike.freq
max(bike.freq)

member.freq<-table(xdata$Member.type)
barplot(member.freq[order(member.freq,decreasing = T)],
        col = "blue",
        border = NA,
        main = "preferred customer type",
        xlab = "type of customer",
        ylab = "number of passes taken")
#hist(member.freq)


#xdata.sub3 <- subset(bike.freq, Duration > 3600, select = c("Duration","Member.type","Start.station","Start.station.number"))


#PREDECTION OF CLASS OF USER USING LINEAR DISCRIMINANT ANALYSIS




#predicting the class of user(member type)
library(MASS)
ldao<-lda(Member.type~Duration,xdata.sub1)
ldapre<-predict(ldao,xdata.sub1)
ldacls<-ldapre$class
ldatbl<-table(ldacls,xdata.sub1$Member.type)
accuracy<-sum(diag(ldatbl))/sum(ldatbl)*100

#output display
ldao
#ldapre
#ldacls
ldatbl
accuracy

#predicting for userd who have duration >5hours

#using subset of dataset
#xdata.sub2 <- subset(xdata, Duration > 18000, select = c("Duration","Member.type","Start.station","Start.station.number"))

#predicting the class of user(member type)
#library(MASS)
#ldaoo<-lda(Member.type~Duration+Start.station.number,xdata.sub1)
#ldapree<-predict(ldao,xdata.sub1)
#ldaclss<-ldapre$class
#ldatbll<-table(ldacls,xdata.sub1$Member.type)
#accuracyy<-sum(diag(ldatbll))/sum(ldatbll)*100

#output display
#ldaoo
#ldapree
#ldaclss
#ldatbll
#accuracyy
