#--- install needed packages ----
install.packages(c("httr", "DT", "countrycode", "ggplot2" , "plotly", "maps", "mapdata"))


#---- load data from csv -----

# from local file
#RawData <- read.csv(file = "weather_data.csv")

library(httr)
link <- url("https://raw.githubusercontent.com/msyk88/workshopPG/master/weather_data.csv")

RawData <- read.csv(link)


#---- view data -----
#View(Data)
print(RawData)

#---- view data - top elements -----
#head(Data)
head(RawData)


#---- view in a nice way -----
library(DT)

sample_DT <- datatable(RawData)
sample_DT



#---- Data wrangling -----
library(dplyr)
library(countrycode)

Results <- RawData %>%
  group_by(Country) %>%
  summarise(Mean = mean(Temperature), Count = length(Temperature))  %>% 
  mutate(Country1 = countrycode(Country, "iso3c", "country.name"))

print(Results)

#---- Plot map PLOTLY -----
library(plotly)
library(DT)


Data_map <- Results %>%
  mutate(code=Country)

## settings for plot (optional)
g <- list(
  scope = 'europe',
  showframe = FALSE,
          showcoastlines = TRUE,
          showland = TRUE,
          landcolor = "white",
          coastlinewidth = 0.5,
          coastlinecolor = toRGB("grey"),
          projection = list(type = "Mercator"))



## create plot
p <- plot_geo(Data_map) %>%
  add_trace(z = ~Mean, color = ~Mean, colors = "Blues",
            text = ~Country, locations = ~code) %>%
  colorbar(title = "Scale:") %>%
  layout(title = "Average temperature in EU countries between 1991 and 2015."
         ,geo = g
  )

p


#---- Plot map GGPLOT2 -----
library(ggplot2)
library(maps)
library(mapdata)

# get world data for map
world <- map_data("world")

# filter out europe
eu_countries <- c("Albania",
                  "Andorra",
                  "Armenia",
                  "Austria",
                  #"Azerbaijan",
                  "Belarus",
                  "Belgium",
                  "Bosnia and Herzegovina",
                  "Bulgaria",
                  "Croatia",
                  "Cyprus",
                  "Czechia",
                  "Denmark",
                  "Estonia",
                  "Finland",
                  "France",
                  "Georgia",
                  "Germany",
                  "Greece",
                  "Hungary",
                  "Iceland",
                  "Ireland",
                  "Italy",
                  #"Kazakhstan",
                  "Kosovo",
                  "Latvia",
                  "Liechtenstein",
                  "Lithuania",
                  "Luxembourg",
                  "Malta",
                  "Moldova",
                  "Monaco",
                  "Montenegro",
                  "Netherlands",
                  "Norway",
                  "Poland",
                  "Portugal",
                  "Romania",
                  "San Marino",
                  "Serbia",
                  "Slovakia",
                  "Slovenia",
                  "Spain",
                  "Sweden",
                  "Switzerland",
                  "Turkey",
                  "Ukraine",
                  "UK")

europe <- world %>% 
  filter(region %in% eu_countries)


#plotMap <- ggplot(data = world)
plotMap <- ggplot(data = europe)


# MAKE MAP - STEP BY STEP

plotMap1 <- plotMap + 
  geom_map(data=europe, map=europe,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="#7f7f7f", size=0.5)


# world map
# plotMap1 <- plotMap + 
#   geom_map(data=world, map=world,
#            aes(x=long, y=lat, group=group, map_id=region),
#            fill="white", colour="#7f7f7f", size=0.5)


# add data frame to map
plotMap2 <- plotMap1 + geom_map(data=Results, map=world,
                                aes(fill=Mean, map_id= as.character(Country1)),
                                colour="#7f7f7f", size=0.5)


# set scale colors
plotMap3 <- plotMap2 + scale_fill_continuous(low="thistle2", high="darkred", 
                                             guide="colorbar")

# set labels
plotMap4 <- plotMap3 + labs(fill="legend", title="Average temperature in EU countries between 1991 and 2015.", x="", y="")

# change theme
plotMap5 <- plotMap4 + theme_bw()
plotMap6 <- plotMap5 + theme(panel.border = element_blank())

# remove lines on y and x axis
plotMap7 <- plotMap6 + scale_y_continuous(breaks=c())
plotMap8 <- plotMap7 + scale_x_continuous(breaks=c())

plotMap8


# MAKE MAP - AT ONCE

plotMap_ALL <- plotMap + geom_map(data=europe, map=europe,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="#7f7f7f", size=0.5) +
  geom_map(data=Results, map=world,
           aes(fill=Mean, map_id= as.character(Country1)),
           colour="#7f7f7f", size=0.5) +
  scale_fill_continuous(low="thistle2", high="darkred", 
                        guide="colorbar") +
  labs(fill="legend", title="Average temperature in EU countries between 1991 and 2015.", x="", y="") +
  labs(fill="legend", title="Average temperature in EU countries between 1991 and 2015.", x="", y="") + 
  theme_bw() + 
  theme(panel.border = element_blank()) + 
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c())

plotMap_ALL
