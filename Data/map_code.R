library(sf)
library(sp)
ut_crs <- "+proj=utm +zone=12 +datum=NAD83"
ut_shape <- st_read("C:/Users/Kelsey/Box/Utah VOC Project/R/Counties.shp")
ut_shape <- st_transform(ut_shape, ut_crs)
ut_data <- read.csv("C:/Users/Kelsey/Box/Utah VOC Project/Crime_by_county.csv")
ut_data <- merge(ut_shape, ut_data, by.x = "FIPS_STR", by.y = "GEOID")
colnames(ut_data)[which(colnames(ut_data) == "ï..County"):which(colnames(ut_data)=="Domestic.Violence")] <- 
  c("County_Name", "Population","Simple_Assault","Kidnapping","Sexual_Assault","Statutory_Rape",
    "Rape_Ind","Agg_Assault_Ind","Child_Abuse","Dom_Viol")

agency_data <- read.csv("C:/Users/Kelsey/OneDrive/Documents/UOVC Project/Organizations.csv")
agency_sf <- st_as_sf(agency_data, coords = c("Longitude", "Latitude"), crs = 4326)
agency_sf <- st_transform(agency_sf, crs=26912)

ut_data$SA_Rate <- 1000*ut_data$Sexual_Assault/ut_data$Population
agency_sf$awards_2019 <- agency_sf$SASP2019 + agency_sf$VAWA2019 + agency_sf$VOCA2019
agency_sf$pct_SA <- agency_sf$AdultSexAssault/agency_sf$TotalIndviduals
agency_sf$pct_DV <- agency_sf$DomesticViolence/agency_sf$TotalIndviduals
agency_sf$DVSA <- 0
agency_sf$DVSA[agency_sf$OrgType %in% c("Domestic and Family Violence Organization", 
                                        "Sexual Assault Services organization (e.g., rape crisis center)",
                                        "Organization Provides Domestic and Family Violence and Sexual Assault Services", 
                                        "Campus-based victims services" )] <- 1
  


library(tmap)
tmap_mode("view")
tm_shape(ut_data) +
  tm_polygons("Sexual_Assault", 
              style = "kmeans", 
              id = "County_Name",
              popup.vars = c("Number of Sexual Assaults" = "Sexual_Assault", 
                             "Population" = "Population",
                             "Rate per 100,000" = "SA_Rate"),
              title = "Num. of Sex. Assaults") +
tm_shape(agency_sf[agency_sf$DVSA == 1,]) + 
  tm_symbols(size = 0.1,
             col="midnightblue",
             popup.vars = c("Total Individuals Served in 2019" = "TotalIndviduals",
                            "Total Awards in 2019"= "awards_2019",
                            "Percent Sexual Assault" = "pct_SA",
                            "Percent Domestic Violence" = "pct_DV"
                            ))

funding_data <- read.csv("C:/Users/Kelsey/Box/Utah VOC Project/Funding_by_county.csv")
funding_data <- merge(ut_shape, funding_data, by.x = "FIPS_STR", by.y = "GEOID")
colnames(funding_data)[colnames(funding_data)=="ï..County"] <- "County_name"

tmap_mode("view")
tm_shape(funding_data) +
  tm_polygons("Distributed_Awards_per_pop", 
              style = "kmeans", 
              id = "County",
              popup.vars = c("County" = "County_name",
                             "Award" = "Distributed_Awards",
                             "Award per Capita" = "Distributed_Awards_per_pop"),
              title = "Award")

tm_shape(funding_data) +
  tm_polygons(c("Distributed_Awards", "POP_CURRES"), 
              style = "kmeans", 
              n = 8,
              id = "County",
              popup.vars = c("Award" = "Distributed_Awards",
                             "Award per Capita" = "Distributed_Awards_per_pop",
                             "Population" = "POP_CURRES"),
              title = c("Award (Dollars per Capita)", "Population")) +
  tm_facets(sync = TRUE, nrow = 2)

funding_history <- read.csv("C:/Users/Kelsey/OneDrive/Documents/UOVC Project/VOCA Funding History.csv")
colnames(funding_history)[1] <- "Category"
library(ggplot2)
library(plotly)
p <- ggplot(funding_history, aes(x= Year, y=Amount, color = Category)) +
  geom_line()+
  scale_color_hue(name="Category",l=55) +
  geom_point()


ggplotly(p)

#############################################################
library(sf)
library(sp)
ut_crs <- "+proj=utm +zone=12 +datum=NAD83"
ut_shape <- st_read("C:/Users/Kelsey/Box/Utah VOC Project/R/Counties.shp")
ut_shape <- st_transform(ut_shape, ut_crs)
ut_data <- read.csv("C:/Users/Kelsey/Box/Utah VOC Project/Crime_by_county.csv")
ut_data <- merge(ut_shape, ut_data, by.x = "FIPS_STR", by.y = "GEOID")
colnames(ut_data)[which(colnames(ut_data) == "ï..County"):which(colnames(ut_data)=="Domestic.Violence")] <- 
  c("County_Name", "Population","Simple_Assault","Kidnapping","Sexual_Assault","Statutory_Rape",
    "Rape_Ind","Agg_Assault_Ind","Child_Abuse","Dom_Viol")

agency_data <- read.csv("C:/Users/Kelsey/OneDrive/Documents/UOVC Project/Organizations.csv")
agency_sf <- st_as_sf(agency_data, coords = c("Longitude", "Latitude"), crs = 4326)
agency_sf <- st_transform(agency_sf, crs=26912)

ut_data$SA_Rate <- 1000*ut_data$Sexual_Assault/ut_data$Population
agency_sf$pct_SA <- agency_sf$AdultSexAssault/agency_sf$TotalIndviduals
agency_sf$pct_DV <- agency_sf$DomesticViolence/agency_sf$TotalIndviduals
agency_sf$DVSA <- 0
agency_sf$DVSA[agency_sf$OrgType %in% c("Domestic and Family Violence Organization", 
                                        "Sexual Assault Services organization (e.g., rape crisis center)",
                                        "Organization Provides Domestic and Family Violence and Sexual Assault Services", 
                                        "Campus-based victims services" )] <- 1

agency_sf$award_per_served <- agency_sf$Total2019/agency_sf$TotalIndviduals

library(ggplot2)
ut_data <- cbind(ut_data, st_coordinates(st_centroid(ut_data)))
ggplot(data = ut_data) +
  geom_text(data = ut_data, aes(X, Y, label = County_Name), size = 2) +
  geom_sf(aes(fill = Population)) + 
  scale_fill_viridis_c(trans = "log", alpha = .4, option = "cividis") +
  geom_sf(data = agency_sf[agency_sf$OrgDescription=="DV/SA",]) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

library(tmap)
tmap_mode("view")
tm_shape(ut_data) +
  tm_polygons("Population", 
              style = "log10_pretty", 
              id = "County_Name",
              popup.vars = c("Population" = "Population"),
              title = "Population") +
  tm_shape(agency_sf[agency_sf$OrgDescription == "Police Department",]) + 
  tm_symbols(size = 0.1,
             col = "midnightblue",
             popup.vars = c("Total Individuals Served in 2019" = "TotalIndviduals",
                            "Total Awards in 2019"= "awards_2019",
                            "Percent Sexual Assault" = "pct_SA",
                            "Percent Domestic Violence" = "pct_DV",
                            "Funding Dollars Per Victim Served" = "award_per_served"
             ))



tract_shape <- st_read("C:/Users/Kelsey/OneDrive/Documents/UOVC Project/GIS/US_tract_2018.shp")
tract_shape <- tract_shape[tract_shape$STATEFP == "49",]
tract_shape <- st_transform(tract_shape, ut_crs)

demographics <- read.csv("C:/Users/Kelsey/OneDrive/Documents/UOVC Project/Demographics.csv")
demographics <- merge(tract_shape, demographics, by = "GISJOIN")
demographics$PovertyRate1 <- as.numeric(demographics$PovertyRate)

ggplot(data = demographics) +
  geom_sf(aes(fill = Poverty), color = NA) + 
  scale_fill_viridis_c(option = "cividis", trans = "sqrt") +
  geom_sf(data = agency_sf, size = .5, color = "white") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

ggplot(data = demographics[demographics$COUNTYFP %in% c("035","049"),]) +
  geom_sf(aes(fill = Poverty), color = NA) + 
  scale_fill_viridis_c(option = "cividis", trans = "sqrt") +
  geom_sf(data = agency_sf, size = .5, color = "white", xlim = c(393643.6,512172.9), ylim=c(4403451, 4530569)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

###########################################################

library(sf)
library(sp)
ut_crs <- "+proj=utm +zone=12 +datum=NAD83"
ut_shape <- st_read("C:/Users/Kelsey/Box/Utah VOC Project/R/Counties.shp")
ut_shape <- st_transform(ut_shape, ut_crs)
ut_data <- read.csv("C:/Users/Kelsey/Box/Utah VOC Project/Crime_by_county.csv")
ut_data <- merge(ut_shape, ut_data, by.x = "FIPS_STR", by.y = "GEOID")
colnames(ut_data)[which(colnames(ut_data) == "ï..County"):which(colnames(ut_data)=="Domestic.Violence")] <- 
  c("County_Name", "Population","Simple_Assault","Kidnapping","Sexual_Assault","Statutory_Rape",
    "Rape_Ind","Agg_Assault_Ind","Child_Abuse","Dom_Viol")

agency_data <- read.csv("C:/Users/Kelsey/OneDrive/Documents/UOVC Project/Organizations.csv")
agency_sf <- st_as_sf(agency_data, coords = c("Longitude", "Latitude"), crs = 4326)
agency_sf <- st_transform(agency_sf, crs=26912)

ut_data$SA_Rate <- 1000*ut_data$Sexual_Assault/ut_data$Population
agency_sf$awards_2019 <- agency_sf$SASP2019 + 
  agency_sf$VAWA2019 + 
  agency_sf$VOCA2019 + 
  agency_sf$VOCAHousing2019
agency_sf$pct_SA <- agency_sf$AdultSexAssault/agency_sf$TotalIndviduals
agency_sf$pct_DV <- agency_sf$DomesticViolence/agency_sf$TotalIndviduals
agency_sf$DVSA <- 0
agency_sf$DVSA[agency_sf$OrgType %in% c("Domestic and Family Violence Organization", 
                                        "Sexual Assault Services organization (e.g., rape crisis center)",
                                        "Organization Provides Domestic and Family Violence and Sexual Assault Services", 
                                        "Campus-based victims services" )] <- 1

agency_sf$award_per_served <- agency_sf$awards_2019/agency_sf$TotalIndviduals

library(tmap)
tmap_mode("view")
tm_shape(ut_data) +
  tm_polygons("Sexual_Assault", 
              style = "kmeans", 
              id = "County_Name",
              popup.vars = c("Number of Sexual Assaults" = "Sexual_Assault", 
                             "Population" = "Population",
                             "Rate per 100,000" = "SA_Rate"),
              title = "Num. of Sex. Assaults") +
  tm_shape(agency_sf[agency_sf$DVSA == 1,]) + 
  tm_symbols(size = 0.1,
             col = "midnightblue",
             popup.vars = c("Total Individuals Served in 2019" = "TotalIndviduals",
                            "Total Awards in 2019"= "awards_2019",
                            "Percent Sexual Assault" = "pct_SA",
                            "Percent Domestic Violence" = "pct_DV",
                            "Funding Dollars Per Victim Served" = "award_per_served"
             ))

funding_data <- read.csv("C:/Users/Kelsey/Box/Utah VOC Project/Funding_by_county.csv")
funding_data <- merge(ut_shape, funding_data, by.x = "FIPS_STR", by.y = "GEOID")
colnames(funding_data)[colnames(funding_data)=="ï..County"] <- "County_name"

tm_shape(funding_data) +
  tm_polygons(c("Distributed_Awards", "POP_CURRES"), 
              style = "kmeans", 
              n = 8,
              id = "County",
              popup.vars = c("County" = "County_name",
                             "Award" = "Distributed_Awards",
                             "Award per Capita" = "Distributed_Awards_per_pop",
                             "Population" = "POP_CURRES"),
              title = c("Total Award", "Population")) +
  tm_facets(sync = TRUE, ncol = 2)
