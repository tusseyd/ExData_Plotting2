## David Tussey davidtussey@gmail.com
## Exploratory Data Analysis
## Project 2, Plot 1 

##  Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
##  Using the base plotting system, make a plot showing the total PM2.5 emission
##  from all sources for each of the years 1999, 2002, 2005, and 2008

##  Record program start time
start.time <- Sys.time()
print(paste("Program Start time is: ", start.time))

## Get data file from provided URL if not available in working directory.
setwd("./")
if(!file.exists("summarySCC_PM25.rds") | !file.exists("Source_Classification_Code.rds")) {
	print("Downloading raw data from web site.")
    download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
	destfile = "SCCrawdata.zip")
	print("Unzipping data file.")
	unzip("SCCrawdata.zip")
}

# Read in the data. 
print("Reading data file.")
if (!exists("NEI")) NEI <- readRDS("./summarySCC_PM25.rds")
if (!exists("SCC")) SCC <- readRDS("./Source_Classification_Code.rds")

## Determine the total polution by year
plot.data <- data.frame()
all.years <- unique(NEI$year)
total <- 0

##  Build the plot.data dataframe for ease in plotting.
for (yr in all.years){
	total <- sum(NEI$Emissions[NEI$year == yr])
    plot.data <- rbind(plot.data, data.frame(yr, total))
}

##Plot 1
##  Plot to the screen for visual verification.
print("Preparing plots for the screen device.")

names(plot.data) <- c("year", "Emissions")
print("Building linear model.")
model <- lm(Emissions ~ year, plot.data)

with(plot.data, plot(year, Emissions,  col = "steelblue4", type = "b", lwd = 3,
	ylim = c(0,10000000), ylab = expression("Total Emissions  " * PM[25]),
	xaxp = c(1998,2009,11), xlab = "Year", main = "Total Emissions by Year"))
with(plot.data, abline(model, lwd = 3, col = "gold2"))

## Plot to the PNG device to create a file.
print("Copying plot to the PNG file.")
dev.copy(png, file = "plot1.png", width=480, height=480)
dev.off()

##  Record program end time
end.time <- Sys.time()
print(paste("Program End time is: ", end.time))
elapsed.time <- end.time - start.time
print(paste("Elapsed time: ", round(elapsed.time, digits = 2)))

## End