# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

#system("ls ../input")
getwd()
getwd()

allgames = read.csv("..//input//vgsales.csv")
allgames$Year <- as.numeric(as.character(allgames$Year))
# A look at the top publishers

TopPublishers <- aggregate(allgames$Global_Sales,list(allgames$Publisher),sum)
TopPublishers <- TopPublishers[with(TopPublishers, order(-x)), ]

Top25publishers <- head(TopPublishers, 25)
Top25publishers

# A look at Gernres
TopGenre <- aggregate(allgames$Global_Sales,list(allgames$Genre),sum)
TopGenre <- TopGenre[with(TopGenre, order(-x)),]

# Genre sales through the years
GenreByYear <- aggregate(allgames$Global_Sales,list(allgames$Genre, allgames$Year),sum)
colnames(GenreByYear) <- c("Genre", "Year", "TotalSales")

ggplot(GenreByYear) + geom_line(aes(x=Year, y=TotalSales, colour=Genre)) +
  facet_wrap(~Genre) 

# A look at recent platforms (No furture releases)
LimitedYears <- subset(allgames, Year > 2000 & Year < 2017)
PlatformByYear <- aggregate(LimitedYears$Global_Sales, list(LimitedYears$Platform, LimitedYears$Year), sum)
colnames(PlatformByYear)<- c("Platform", "Year", "TotalSales")

ggplot(PlatformByYear) + geom_line(aes(x=Year,y=TotalSales, colour = Platform)) +
  facet_wrap(~Platform)


# Platform Game Sales since the millenium
recentGames <- subset(allgames, Year > 2011 & Year < 2017)

PlatformSales <- aggregate(recentGames$Global_Sales, list(recentGames$Platform), sum)
colnames(PlatformSales)<- c("Platform", "TotalSales")
PlatformSales <- PlatformSales[with(PlatformSales, order(-TotalSales)),]
TopPlatform <- head(PlatformSales, 10)
slices <- TopPlatform$TotalSales
lbls <- TopPlatform$Platform
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Sales by Platform - Last Five Years") 