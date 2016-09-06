###################
# New Orleans, LA #
###################

nola <- read.table("nola.txt",
                   header=FALSE,
                   sep=",",
                   skip=0,
                   stringsAsFactors=FALSE,
                   strip.white=TRUE,
                   quote = NULL)
colnames(nola) <- c("year", "Months", "pop.coverage", "violent.crime", "murders", "rape", "robbery", "assault")

nola2 <- read.table("nola2.txt",
                    header=FALSE,
                    sep=",",
                    skip=0,
                    stringsAsFactors=FALSE,
                    strip.white=TRUE,
                    quote = NULL)
colnames(nola2) <- c("year", "Months", "pop.coverage", "property.crime", "burglary", "larceny", "vehicle")
nola <- merge(nola, nola2, by=c("year", "Months", "pop.coverage"), all=TRUE)
remove(nola2)

# Remove 2005 b/c only have 6 months of data
nola <- subset(nola, year != 2005)

# https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/
# Only preliminary data is available for 2015
year.2013 <- c(2013, NA, 377022, 2965, 156, 176, 1138, 1495, 14525, 3203, 9179, 2143)
year.2014 <- c(2014, NA, 387113, 3770, 150, 244, 1470, 1906, 16382, 3458, 10309, 2615)
nola <- rbind(nola, year.2013, year.2014)
remove(year.2013, year.2014)

nola$nola.violent.crime.rate <- (nola$violent.crime*100000)/nola$pop.coverage
nola$nola.property.crime.rate <- (nola$property.crime*100000)/nola$pop.coverage


##########################
# United States - totals #
##########################

us <- read.table("us.txt",
                 header=FALSE,
                 sep=",",
                 skip=0,
                 stringsAsFactors=FALSE,
                 strip.white=TRUE,
                 quote = NULL)
colnames(us) <- c("year", "us.pop.coverage", "us.violent.crime", "us.murders", "us.rape", "us.robbery", "us.assault")

us2 <- read.table("us2.txt",
                  header=FALSE,
                  sep=",",
                  skip=0,
                  stringsAsFactors=FALSE,
                  strip.white=TRUE,
                  quote = NULL)
colnames(us2) <- c("year", "us.pop.coverage", "us.property.crime", "us.burglary", "us.larceny", "us.vehicle")
us <- merge(us, us2, by=c("year","us.pop.coverage"), all=TRUE)
remove(us2)

# https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/
# Only preliminary data is available for 2015
year.2013 <- c(2013, 316128839, 1163146, 14196, 82109, 345031, 724149, 8632512, 1928465, 6004453, 699594)
year.2014 <- c(2014, 318857056, 1165383, 14249, 84041, 325802, 741291, 8277829, 1729806, 5858496, 689527)
us <- rbind(us, year.2013, year.2014)
remove(year.2013, year.2014)

crime <- merge(nola, us, by="year", all=TRUE)

# Need to subtract out NOLA numbers
crime$us.pop.coverage <- crime$us.pop.coverage - crime$pop.coverage
crime$us.violent.crime <- crime$us.violent.crime - crime$violent.crime
crime$us.property.crime <- crime$us.property.crime - crime$property.crime

# Now calculate US crime rates
crime$us.violent.crime.rate <- (crime$us.violent.crime*100000)/crime$us.pop.coverage
crime$us.property.crime.rate <- (crime$us.property.crime*100000)/crime$us.pop.coverage


##########################
# Police employment data #
##########################

year <- seq(1995, 2015, 1)
police <- c(1372,1302,1480,1635,1633,1658,1627,1613,1612,1661,1671,1424,1416, 1448,1450,1452,1349,1271,1210,1132,1163)
police <- as.data.frame(cbind(year, police))
remove(year)

crime <- merge(police, crime, by="year", all=TRUE)
remove(police)

crime <- subset(crime, year >= 1995)


##########
# Models #
##########

violent.crime.model <- lm(nola.violent.crime.rate ~ police + us.violent.crime.rate,
                          data = crime)
summary(violent.crime.model)

nationwide.lm <- lm(nola.violent.crime.rate ~ us.violent.crime.rate, data=crime) 
summary(nationwide.lm)
nola.violent.crime.z <- rstandard(nationwide.lm)

graph.data <- subset(crime, year != 2005 & year != 2015)
graph.data <- cbind(graph.data, nola.violent.crime.z)

library(ggplot2)
ggplot(graph.data, aes(x=police, y=nola.violent.crime.z)) +
    geom_point() +
    geom_text(aes(label=year), size=4) + 
    geom_smooth(method=lm)  

property.crime.model <- lm(nola.property.crime.rate ~ police + us.property.crime.rate,
                           data = crime)
summary(property.crime.model)

