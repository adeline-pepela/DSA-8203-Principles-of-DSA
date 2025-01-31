library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(randomForest)
library(plyr)
library(readr)
library(janitor)
library(purrr)

#set working directory (where your files are)
list.files("~/Documents")

setwd("Users/PC/DownloadsADEH-MSC/Module 2/DSA 8203-Principles of DSA")


# 1. Import sector data
#2020 sector file is added to this folder
nsesectors <- read.csv(file = 'NSE_data_stock_market_sectors_2020.csv')
View(nsesectors)

print(NSEfolder)
view(NSEfolder)


# 2. - Import NSE Data
#Folder with NSE Data is added to this folder
NSEfolder <- "NSE_Data"
nsefiles <- list.files(path = NSEfolder, pattern = "*.csv", full.names=TRUE)
print(nsefiles)
nsefilesv2 <- nsefiles %>%
  map(read_csv) %>%
  
  reduce(rbind , .init = data.frame())



View(nsefilesv2)
write_csv(nsefilesv2,"nsefilesv2.csv")

list.files(path = NSEfolder)

#Change Date format
nsefilesv2$Date2 = format(as.Date(nsefilesv2$DATE, "%d-%b-%y"), "%d-%m-%Y")
nsefilesv2$Date3 <- lubridate::dmy(nsefilesv2$Date2)
View(nsefilesv2)

#Step 2.1 - Join Sector Data to NSE Stock Price Data
nsefilesv3 <- merge(x=nsefilesv2, y=nsesectors, by = c("CODE" ), all = TRUE)
View(nsefilesv3)

#Omit Null Values
nsefilesv4 <- na.omit(nsefilesv3)
View(nsefilesv4)

#Write file to working Directory
write_csv(nsefilesv4,"nsefilesv4.csv")


#Clean Up column names if necessary
#names(nsefilesv4) <- gsub(" ", "_", names(nsefilesv4))

#subset to include only the company(s) of interest e.g. Kakuzi
nsefilesv5 <- subset(nsefilesv4, subset = nsefilesv4$CODE == "KUKZ" )
View(nsefilesv5)
write_csv(nsefilesv5,"nsefilesv5.csv")

#subset to include only relevant dates
nsefilesv6 <- subset(nsefilesv5, subset = nsefilesv5$Date3 >= "2020-01-01" )
View(nsefilesv6)
write_csv(nsefilesv6,"KakuziAnnual.csv")

nsefilesv6$month = format(nsefilesv6$Date3,"%m")
nsefilesv6$year = format(nsefilesv6$Date3,"%Y")
nsefilesv6$monthyear = paste(nsefilesv6$year,nsefilesv6$month )

#plot some data
qplot(nsefilesv6$Previous, geom = "histogram")

nsefilesv7 <- ggplot(nsefilesv6,aes(monthyear ,y=Previous, group=1)) + geom_jitter() + geom_smooth(model = "lm")

qplot(nsefilesv9$Previous, geom = "histogram", bins = 30) + 
  ggtitle("Kakuzi Distribution") +
  xlab("Previous") + 
  ylab("Frequency")


#plot some more data
qplot(nsefilesv7$Previous, geom = "histogram")
nsefilesv8 <- ggplot(nsefilesv7,aes(monthyear ,y=Previous, group=1)) + geom_jitter() + geom_smooth(model = "lm")








#subset to include only the company(s) of interest e.g. UNGA
nsefilesv8 <- subset(nsefilesv4, subset = nsefilesv4$CODE == "UNGA" )
View(nsefilesv8)
write_csv(nsefilesv8,"nsefilesv8.csv")

#subset to include only relevant dates
nsefilesv9 <- subset(nsefilesv8, subset = nsefilesv8$Date3 >= "2019-01-01" )
View(nsefilesv9)
write_csv(nsefilesv9,"UngaAnnual.csv")

# Add month, year, and monthyear columns
nsefilesv9$month <- format(nsefilesv9$Date3, "%m")
nsefilesv9$year <- format(nsefilesv9$Date3, "%Y")
nsefilesv9$monthyear <- paste(nsefilesv9$year, nsefilesv9$month, sep = "-")


# Convert monthyear to a factor or date for plotting
nsefilesv9$monthyear <- as.factor(nsefilesv9$monthyear)

# Plot with ggplot
ggplot(nsefilesv9, aes(x = monthyear, y = Previous, group = 1)) +
  geom_jitter(width = 0.2, color = "blue") +  # Adds jitter to reduce overplotting
  geom_smooth(method = "lm", color = "red") +  # Linear model trendline
  ggtitle("UNGA Limited Stock Exchange: Previous Value Over Time") +
  xlab("Month-Year") +
  ylab("Previous Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability



#plot some more data
qplot(nsefilesv9$Previous, geom = "histogram")
nsefilesv9 <- ggplot(nsefilesv7,aes(monthyear ,y=Previous, group=1)) + geom_jitter() + geom_smooth(model = "lm")

qplot(nsefilesv9$Previous, geom = "histogram", bins = 30) + 
  ggtitle("UNGA Limited Stock Exchange: Previous Value Distribution") +
  xlab("Previous") + 
  ylab("Frequency")

















#subset to include only relevant dates
nsefilesv6 <- subset(nsefilesv5, subset = nsefilesv5$Date3 >= "2019-01-01" )
View(nsefilesv6)
write_csv(nsefilesv6,"KakuziAnnual.csv")

nsefilesv6$month = format(nsefilesv6$Date3,"%m")
nsefilesv6$year = format(nsefilesv6$Date3,"%Y")
nsefilesv6$monthyear = paste(nsefilesv6$year,nsefilesv6$month )

#plot some data
qplot(nsefilesv6$Previous, geom = "histogram")

nsefilesv7 <- ggplot(nsefilesv6,aes(monthyear ,y=Previous, group=1)) + geom_jitter() + geom_smooth(model = "lm")
