## David Tussey davidtussey@gmail.com
## Exploratory Data Analysis
## Project 2, Plot 3 

##  Of the four types of sources indicated by the 
##  type (point, nonpoint, onroad, nonroad) variable, 
##  which of these four sources have seen decreases in 
##  emissions from 1999-2008 for Baltimore City? 
##  Which have seen increases in emissions from 1999-2008? 

##  Use the ggplot2 plotting system to make a plot answer this question.

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

##  Build the plot.data dataframe for ease in plotting.
plot.data <- data.frame(Year = integer(), Type = character(), Emissions = numeric())
for (yr in all.years){
	for (type in all.types){
		total <- sum(baltimore.city$Emissions[baltimore.city$year == yr & 
                        baltimore.city$type == type])
    plot.data <- rbind(plot.data, data.frame(Year = yr, Type = type, Emissions = total))
	}
}

co.plot <- ggplot(plot.data, aes(Year, Emissions, color = Type)) + 
    geom_point(shape = 21, size = 6, col="black", bg = "white") +
    geom_line(size = 1.2) + 
	facet_grid(. ~ Type) + 
    geom_smooth(method = "lm", se = FALSE, colour="black", size = 1.5, linetype = "dotted" ) + 
	theme(axis.text.x = element_text(angle = 75, hjust = 1, face = "bold"), 
          legend.position = "none", 
          plot.title = element_text(face="bold", size = 22), 
          axis.text.y = element_text(face = "bold")) +
	xlab("Year") + ylab(expression("Total Emissions  " * PM[25])) + 
	labs(title = "Emissions by Source for Baltimore, MD")
print(co.plot)

## Plot to the PNG device to create a file.
print("Copying plot to the PNG file.")
ggsave(filename="plot3.png", dpi=72)

##  Record program end time
end.time <- Sys.time()
print(paste("Program End time is: ", end.time))
elapsed.time <- end.time - start.time
print(paste("Elapsed time : ", round(elapsed.time, digits = 2)))

## End