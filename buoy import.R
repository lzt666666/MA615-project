library(tidyverse)
library(stringr)
library(lubridate)
library(dplyr)
### make URLs

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(2000:2019)

urls <- str_c(url1, years, url2, sep = "")

filenames <- str_c("mr", years, sep = "")

###  Read the data from the website

N <- length(urls)
colname_1=colnames(read.table(urls[1],fill=TRUE, header = TRUE))
colname_2=colnames(read.table(urls[7],fill=TRUE, header = TRUE))
for (i in 1:N){
  suppressMessages(  ###  This stops the annoying messages on your screen.  Do this last.
    assign(filenames[i], read.table(urls[i],fill=TRUE ,header = TRUE))
  )
    
  file <- get(filenames[i])
  if(ncol(file)==17){
    colnames(file)=colname_1
  }else{
    colnames(file)=colname_2
  }
  
  if(i == 1){
    MR <- file
  }else{
    MR <- dplyr::bind_rows(MR, file)
  }
}
#transform time data
MR=data.frame(MR)
time=MR%>% select(YYYY,MM,DD,hh)
time=make_datetime(MR$YYYY,time$MM,time$DD,time$hh)
time=data.frame(time)
MR=dplyr::mutate(time,MR)
MR=MR[,-c(2,3,4,5)]

MR$time=as.POSIXct(MR$time)
view(MR)
#found that:WVHT,DPD,APD,MWD,DEWP,VIS TIDE,MM are all useless, delete them from the dataset
MR=MR[,-c(5,6,7,8,12,13,14,15)]
#delete outliers
MR=filter(MR,MR$WD<999&MR$WSPD<99&MR$GST<99&MR$BAR<9999&MR$ATMP<999&MR$WTMP<999)
summary(MR)
#sampling the data, reduce data size by using the mean value for each day.
MR2=MR%>%group_by(date(time))%>%summarize(mean(WD),mean(WSPD),mean(GST),mean(BAR),mean(ATMP),mean(WTMP))
view(MR2)

#time series plot to ignore seasonal fluctuation
par(mfrow=c(1,2))
ATMP=ts(MR2$`mean(ATMP)`,frequency =365,start=c(2000,1))
TS1=decompose(ATMP)
plot(TS1$trend,main="Air temprature")
WTMP=ts(MR2$`mean(WTMP)`,frequency =365,start=c(2000,1))
TS2=decompose(WTMP)
plot(TS2$trend,main="sea surface temprature")
fit1=lm(TS1$trend~MR2$`date(time)`)
fit2=lm(TS2$trend~MR2$`date(time)`)
summary(fit1)
summary(fit2)
#years average tempreture
MR4=MR%>%group_by(year(time))%>%summarize(mean(ATMP),mean(WTMP))
ggplot(MR4,aes(`year(time)`,`mean(ATMP)`))+geom_point(color='red')+
  geom_point(data=MR4,aes(y=`mean(WTMP)`),color="blue")+ggtitle("2000-2019yearly temperature trend")+labs(y="temperature",x="year")
            

        
