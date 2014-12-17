## David Tussey davidtussey@gmail.com
## Exploratory Data Analysis
## Project 2, Plot 3 

##  Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
##  Using the base plotting system, make a plot showing the total PM2.5 emission
##  from all sources for each of the years 1999, 2002, 2005, and 2008

##  Record program start time
start.time <- Sys.time()
print(paste("Program Start time is: ", start.time))

## Ensure ggplot2 is available for use
library(ggplot2)

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
print( "Reading data file." )
if (!exists("NEI")) NEI <- readRDS("./summarySCC_PM25.rds")
if (!exists("SCC")) SCC <- readRDS("./Source_Classification_Code.rds")

##Plot 3
##  Plot to the screen for visual verification.
print("Preparing plots for the screen device.")

if (!exists("baltimore.city")) baltimore.city <- subset(NEI, fips == "24510")

all.years <- unique(baltimore.city$year)
all.types <- unique(baltimore.city$type)
plot.data <- data.frame(Year = integer(), Type = character(), Emissions = numeric())

for (yr in all.years){
	for (type in all.types){
		total <- sum(baltimore.city$Emissions[baltimore.city$year == yr & baltimore.city$type == type])
    plot.data <- rbind(plot.data, data.frame(Year = yr, Type = type, Emissions = total))
	}
}

co.plot <- ggplot(plot.data, aes(Year, Emissions)) + 
    geom_point(colour = "black", shape = 21, size = 4, fill="black") +
    geom_line( colour = "steelblue", size = 1.2) + 
	facet_grid(. ~ Type) + 
    geom_smooth(method = "lm", se = FALSE, colour="gold2", size = 1.5, linetype = "dashed" ) + 
	theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
	xlab("Year") + ylab(expression("Total Emissions  " * PM[25])) + 
	labs(title = "Emissions by Source for Baltimore, MD")
print(co.plot)

## Plot to the PNG device to create a file.
print("Copying plot to the PNG file.")
ggsave(filename="Project2-Plot3.png", dpi=72)

##  Record program end time
end.time <- Sys.time()
print(paste("Program End time is: ", end.time))
elapsed.time <- end.time - start.time
print(paste("Elapsed time : ", round(elapsed.time, digits = 2)))

## End