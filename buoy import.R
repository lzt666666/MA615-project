library(tidyverse)
library(stringr)


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
view(MR)
