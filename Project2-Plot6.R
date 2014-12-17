## David Tussey davidtussey@gmail.com
## Exploratory Data Analysis
## Project 2, Plot 6 

##  Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
##  Using the base plotting system, make a plot showing the total PM2.5 emission
##  from all sources for each of the years 1999, 2002, 2005, and 2008

##  Record program start time
start.time <- Sys.time()
print(paste("Project 2 Plot #6 program Start time is: ", start.time))

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


## Determine "coal combustion-related" sources
motor.SCC <- SCC$SCC[ grepl("motor | vehicle",   SCC$SCC.Level.Four,  ignore.case=TRUE) | 
                      grepl("motor | vehicle",   SCC$SCC.Level.Three, ignore.case=TRUE) |
                      grepl("motor | vehicle",   SCC$EI.Sector,       ignore.case=TRUE) |
                      grepl("motor | vehicle",   SCC$Short.Name,      ignore.case=TRUE) |
                      grepl("motor | vehicle",   SCC$SCC.Level.Two,   ignore.case=TRUE) ] 

##  Set up dataframe for plot
plot.data <- data.frame(Year = integer(), FIPS = character(), Emissions = numeric())

print("Extracting city-related data from NEI file.")
city.data <- subset(NEI, fips == "24510" | fips == "06037")
all.fips <- unique(city.data$fips)
all.years <- unique(city.data$year)

print("Computing emissions data by year by city (FIPS).")

new.total <- 0
for (yr in all.years) {
    for (fip in all.fips) {
        new.total <- sum(city.data$Emissions[city.data$SCC %in% motor.SCC & 
                                        city.data$year == yr & city.data == fip])
        plot.data <- rbind(plot.data, data.frame(Year = yr, FIPS = fip, Emissions = new.total))
    }
 }

plot.data$FIPS <- as.character(plot.data$FIPS)
plot.data$FIPS[plot.data$FIPS == "24510"] <- "Baltimore"
plot.data$FIPS[plot.data$FIPS == "06037"] <- "Los Angeles"


##Plot 5
##  Plot to the screen for visual verification.
print("Preparing plots for the screen device.")

print("Building linear model.")
model <- lm(Emissions ~ Year, plot.data)

co.plot <- ggplot(plot.data, aes(Year, Emissions)) + 
    geom_point(colour = "black", shape = 21, size = 4, fill="black") +
    geom_line( colour = "steelblue", size = 1.2) + 
    facet_grid(. ~ FIPS) + 
    geom_smooth(method = "lm", se = FALSE, colour="gold2", size = 1.5, linetype = "dashed" ) + 
    theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
    xlab("Year") + ylab(expression("Total Emissions  " * PM[25])) + 
    labs(title = "Emissions by Source for Baltimore, MD and Los Angeles, CA")
print(co.plot)

## Plot to the PNG device to create a file.
print("Copying plot to the PNG file.")
ggsave(filename="Project2-Plot6.png", dpi = 72)

##  Record program end time
end.time <- Sys.time()
print(paste("Project 2 Plot #6 program End time is: ", end.time))
elapsed.time <- end.time - start.time
print(paste("Elapsed time: ", round(elapsed.time, digits = 2)))

## End