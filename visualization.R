#load required packages
library(ggplot2)
library(data.table)
#load data
acled=read.csv("acled.csv",header = TRUE,stringsAsFactors = FALSE)

#data cleaning
acled=acled[,-c(4,6,7,8,9,10,11,12,13,15,19,20)]
acled$EVENT_ID_NO_CNTY=factor(acled$EVENT_ID_NO_CNTY)
acled$EVENT_DATE=as.Date(acled$EVENT_DATE,format = "%d/%m/%y")
acled$YEAR=factor(acled$YEAR)
acled$EVENT_TYPE=factor(acled$EVENT_TYPE)
acled$ADMIN2=factor(acled$ADMIN2)
acled$LOCATION=factor(acled$LOCATION)
#data manipulation
#get the days that the violence occured
acled$VIOLENCE_DAY=factor(weekdays(acled$EVENT_DATE))
violence_per_day=as.data.frame(table(acled$VIOLENCE_DAY))
names(violence_per_day)=c("day","count")
#no of fatalities per day
fatalities_per_day=as.data.frame(aggregate(acled$FATALITIES,by =list(Category=acled$VIOLENCE_DAY),FUN=sum))
names(fatalities_per_day)=c("day","fatalities")
#get no of violence ocurance per year
violence_by_year=as.data.frame(table(acled$YEAR))
names(violence_by_year)=c("year","count")
#get no of fatalities per year
fatalities_per_year=as.data.frame(aggregate(acled$FATALITIES, by=list(Category=acled$YEAR), FUN=sum))
names(fatalities_per_year)=c("year","fatalities")
#violence per region
violence_per_region=as.data.frame(table(acled$ADMIN2))
names(violence_per_region)=c("region","count")
region_per_year=as.data.frame(table(acled$YEAR,acled$ADMIN2))
names(region_per_year)=c("year","region","count")
#fatalities_per_region
fatalities_per_region=as.data.frame(aggregate(acled$FATALITIES,by=list(Category=acled$ADMIN2),FUN=sum))
names(fatalities_per_region)=c("region","fatalities")

#visualise the data
ggplot(data=violence_by_year, aes(x=violence_by_year$year, y=violence_by_year$count,group=2)) +geom_line()+ggtitle("violence trend since 1997") +
    labs(x="Year",y="instances")
qplot(x=year, y=fatalities, fill=fatalities,
      data=fatalities_per_year, geom="bar", stat="identity")
ggplot(data=fatalities_per_year, aes(x=year, y=fatalities,group=1)) +geom_line()+ggtitle("fatality since 1997") +
    labs(x="Year",y="fatalities")
#create shiny app

