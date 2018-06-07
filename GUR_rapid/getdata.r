#-------------------------------------------------------------------------------
# import data from SQL

library(RPostgreSQL, quietly = T, warn.conflicts = F)
s   <- Sys.getenv()
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host = s[['KAHAWAI_DBHOST']], dbname = s[['KAHAWAI_DBNAME']],
        password = s[["KAHAWAI_DBPASS"]], user = s[["KAHAWAI_DBUSER"]])

tcer  <- dbGetQuery(con, "SELECT * FROM n_fma2_fi")
effort  <- dbGetQuery(con, "SELECT * FROM fma2_fi")
landings  <- dbGetQuery(con, "SELECT * FROM fma2_la")
# alloc <- dbGetQuery(con, "SELECT * FROM alloc")
allocated <- dbGetQuery(con, "SELECT * FROM allocated")
prop_land <- dbGetQuery(con, "SELECT * FROM prop_land")
mhr <- dbGetQuery(con, "SELECT * FROM fma2_mq")
ug_landings <- dbGetQuery(con, "SELECT * FROM fma2_la_ug")
est <- dbGetQuery(con, "SELECT * FROM fma2_ca")
d_landings  <- dbGetQuery(con, "SELECT * FROM dropped_landings")
historic  <- dbGetQuery(con, "SELECT * FROM historic_method")
vessel_char  <- dbGetQuery(con, "SELECT * FROM vessel_char")
# effort_a  <- dbGetQuery(con, "SELECT * FROM fma2_fi_a")

#-------------------------------------------------------------------------------
# load libraries for analysis
library(dplyr)
library(tidyr)
library(MASS)
library(ggplot2)
library(influ)
library(rgdal)
library(scales)
library(gridExtra)
library(cowplot)
library(survival)
library(PBSmapping)
library(analyser)

# stats <- readOGR('Shapes', 'STATAREA_SNA2')
# nz <- readOGR('Shapes', 'bigislands')

library(rgeos)
library(rgdal)
s   <- Sys.getenv()

EPSG = make_EPSG()
read_layer <- function(con, query, srid, t) {
    p4s = EPSG[which(EPSG$code == srid), "prj4"]
    dft <- dbGetQuery(con, query)
    row.names(dft) <- dft$gid
    for (i in seq(nrow(dft))) {
        if (i == 1) {
            spt = readWKT(dft$wkt_geometry[i], dft$gid[i], p4s)
        } else {
            spt = rbind(
                spt, readWKT(dft$wkt_geometry[i], dft$gid[i], p4s))
        }
    }
    return(t(spt, dft[-2]))
}
query <- "SELECT gid, ST_AsText(ST_Transform(ST_SetSRID(geom, 2193), 4326)) AS wkt_geometry
          FROM layers_shapes.nz_coastlines_and_islands_polygons_topo_1500k"
nz <- read_layer(con, query, 4326, SpatialPolygonsDataFrame)

query <- "SELECT gid, AREA_CODE, ST_AsText(ST_Transform(ST_SetSRID(geom, 2193), 4326)) AS wkt_geometry
          FROM layers_shapes.STATAREA_General_region"
stats <- read_layer(con, query, 4326, SpatialPolygonsDataFrame)


# try(data(NZbathy,package='analyser'))

data(worldLLhigh)
coast = clipPolys(
  worldLLhigh,
  ylim=c(-60,-30),
  xlim=c(160,190)
)

# old_indices <- read.csv('./2014_indices.csv', header=TRUE)
source('influ.r')

#-------------------------------------------------------------------------------
mhr$month <- as.numeric(mhr$month)
mhr$year <- as.numeric(mhr$year)
mhr$quantity <- as.numeric(mhr$quantity)
mhr$fyear <- ifelse(mhr$month <= 9, mhr$year, mhr$year + 1)
mhr$fyear <- as.factor(as.character(mhr$fyear))

landings <- filter(landings, green_weight < 10000)

annual_returns <- mhr %>%
  group_by(fishyear = fyear, fishstock_code = stock_code) %>%
  summarise(ahr = round(sum(quantity, na.rm=TRUE)/1000,1))

annual_landings <- landings %>%
  group_by(fishyear, fishstock_code) %>%
  summarise(landed = round(sum(green_weight, na.rm=TRUE)/1000,1))

comp <- left_join(annual_landings, annual_returns)
comp$ahr[comp$fishyear == '2016'] <- 748.1
comp$ahr[comp$fishyear == '2017'] <- 630.6
#-------------------------------------------------------------------------------

prop_land_test <- prop_land
prop_land$area_month <- paste(prop_land$start_stats_area_code, prop_land$month, sep=":")
prop_land$distance <- prop_land$fishing_duration * prop_land$effort_speed
prop_land$cpueno <- NA
prop_land$fishyear <- as.integer(prop_land$fishyear)
prop_land$month <- as.integer(prop_land$month)
prop_land$vessel_key <- as.integer(prop_land$vessel_key)
prop_land$date <- as.character(prop_land$date)
colnames(prop_land) <- c("event", "version", "trip", "fyear", "month", "method", "target", "area", "vessel", "form", "date", "time","height", "width", "length",
  "speed", "temp", "bottom", "lat", "lon", "duration", "num",  "total", "hooks",'checks', 'vessel_name', 'species', "GUR_est", "GUR_prop", "events", "area_month", "distance", "cpueno" )
prop_land$lat <- as.numeric(prop_land$lat)
prop_land$lon <- as.numeric(prop_land$lon)
prop_land$catch <- prop_land$GUR_prop
prop_land$catch[is.na(prop_land$catch)] <- 0
prop_land$effort <- prop_land$num
prop_land$events <- 1

prop_land$num2 <- NA
prop_land$netlength <- NA
prop_land$inshore <- NA
prop_land$zone <- prop_land$area
prop_land$moon <- NA
prop_land$zone2 <- NA
prop_land$depth <- prop_land$bottom
prop_land$landing_name <- NA
prop_land$season <- NA

#-------------------------------------------------------------------------------
# fisheries characterisation
prop_land$target[prop_land$target %in% c('FLA','BFL','BLF','BRI','ESO','FLO','GFL','LSO','SFL','SOL','TUR','WIT','YBF')] <- 'FLA'
prop_land$target[!prop_land$target %in% c('FLA', 'GUR', 'SNA', 'TRE', 'TAR')] <- 'OTH'
# lastyear <- left_join(dplyr::select(filter(prop_land, fyear %in% c('2016', '2017')), -GUR_est, -catch, -events), dplyr::select(alloc, -species_code), by=c('event'='event_key'))
# lastyear$GUR_est <- lastyear$estimated
# lastyear$catch[is.na(lastyear$catch)] <- 0
# lastyear$events <- 1
# prop_land <- bind_rows(filter(prop_land, !fyear %in% c('2016', '2017')), lastyear[,colnames(prop_land)])

prop_land$mth <- factor(format(as.Date(prop_land$date), format="%b"), levels=c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'))

tcer <- filter(prop_land, fyear >= 2008 & !is.na(bottom) & bottom < 200)
tcer$d_bin <- as.numeric(as.character(gsub("\\(*(\\d+)\\,.*", "\\1",
                cut_width(tcer$bottom, 10, center=5))))
depth_fishery <- left_join(filter(tcer, target %in% c('FLA', 'GUR', 'SNA', 'TRE', 'TAR')) %>%
                    group_by(d_bin, target) %>%
                    summarise(catch = sum(catch, na.rm=TRUE)),
                    filter(tcer, target %in% c('FLA', 'GUR', 'SNA', 'TRE', 'TAR')) %>%
                    group_by(target) %>%
                    summarise(total = sum(catch, na.rm=TRUE)))
depth_fishery$proportion <- depth_fishery$catch / depth_fishery$total


prop_land$fishery <- rep('Other', nrow(prop_land))
prop_land$fishery[prop_land$target %in% c('SNA', 'GUR', 'TRE')] <- 'BT-MIX'
prop_land$fishery[prop_land$target == 'FLA'] <- 'BT-FLA'
prop_land$fishery[prop_land$target == 'TAR'] <- 'BT-TAR'

fisheries <- prop_land %>% group_by(fishery) %>% summarise(annual = sum(catch, na.rm=TRUE)/1000)
fisheries$percent <-  fisheries$annual / colSums(fisheries[,2]) * 100

pdf <- filter(prop_land, fyear >2007)
pdf$lat <- round(pdf$lat, 1)
pdf$lon <- round(pdf$lon, 1)
dist_df <- pdf %>% group_by(lat, lon, fishery) %>% summarise(catch = sum(catch, na.rm=TRUE)/1000)

prop_land$d_bin <-  round(prop_land$depth/5, 0) * 5
prop_land$cpue <-  prop_land$catch / prop_land$effort


form_plot <- left_join(filter(prop_land, form != 'HTC') %>% group_by(form, fyear) %>%
    summarise(freq = n_distinct(event)),
    filter(prop_land, form != 'HTC') %>% group_by(fyear) %>%
        summarise(total = n_distinct(event)))

form_plot$prop <- (form_plot$freq / form_plot$total)* 100


subset <- Subsetter(prop_land,
  target %in% c('GUR', 'SNA', 'TRE') &
  area %in% c("011", "012", "013", "014", "015", "016") &
  fyear %in% 1990:2017
)

events <- filter(subset$data, catch>1381)[c(16,19,29,30,40,54,56,57,86,92,97,120),'event']
events2 <- filter(subset$data, catch>663.5 & fyear >= 2008)[c(24, 25, 52, 56, 127),'event']
prop_land <- filter(prop_land, !event %in% events)

aggregater <- Aggregater(prop_land,
  by = c('vessel', 'date')
  )
aggregater$data$cpueno <- NA

subsetter <- Subsetter(aggregater$data,
  target %in% c('GUR', 'SNA', 'TRE') &
  area %in% c("011", "012", "013", "014", "015", "016") &
  fyear %in% 1990:2017
)
subsetter16 <- Subsetter(aggregater$data,
  target %in% c('GUR', 'SNA', 'TRE') &
  area %in% c("011", "012", "013", "014", "015", "016") &
  fyear %in% 1990:2016
)

head(events)
str(events)
prop_land <- filter(prop_land, !event %in% events2)
subsetter_tow <- Subsetter(prop_land,
  target %in% c('GUR', 'SNA', 'TRE') &
  area %in% c("011", "012", "013", "014", "015", "016") &
  fyear %in% 2008:2017
)
subsetter_tow$data <- filter(subsetter_tow$data, catch < 2000)

aggregater <- Aggregater(subsetter$data,
  by = c('vessel', 'date')
  )
aggregater16 <- Aggregater(subsetter16$data,
  by = c('vessel', 'date')
  )

aggregater$data <- aggregater$data[!is.na(aggregater$data$effort),]
aggregater$data$effort[aggregater$data$effort==0] <-1

corer <- Corer(aggregater$data,
  trips_min = 5,
  years_min= 7
)
corer16 <- Corer(aggregater16$data,
  trips_min = 5,
  years_min= 7
)
corer_tow <- Corer(subsetter_tow$data,
  trips_min = 5,
  years_min= 5
)

restrictor <- Restrictor(corer$data)
restrictor_tow <- Restrictor(corer_tow$data)
restrictor$data <- filter(restrictor$data, !is.na(duration) & duration >1 & duration < 18)
restrictor_tow$data <- filter(restrictor_tow$data, !is.na(bottom) & !is.na(lon) &
  !is.na(lat) & !is.na(duration) & !is.na(speed) & bottom > 0 & bottom < 100 &
  fyear %in% c('2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017')
  & !is.na(width) & !is.na(height) & duration >1 & duration < 8 & width < 40 & width > 5
  & height < 5 )

restrictor$data$Fyear <- as.numeric(as.character(restrictor$data$fyear))
trips <- unique(filter(restrictor$data, Fyear < 2008 & vessel %in% c('235190452',
  '236276327', '879837063', '924510039', '118386200'))$trip)
restrictor$data <- filter(restrictor$data, !trip %in% trips)
restrictor$data$target <- factor(restrictor$data$target)
restrictor_tow$data$target <- factor(restrictor_tow$data$target)

# with(filter(prop_land, !event %in% corer_tow$data$event), table(checks))

occurrence <- glm(
  catch>0 ~ fyear + vessel + target + poly(log(duration), 3) + area,
  family = binomial(link = "logit"),
  data = restrictor$data
)

head(restrictor_tow$data)
occurrence_tow <- glm(
  catch>0 ~ fyear + vessel + month + poly(log(depth), 3) + area,
  family = binomial(link = "logit"),
  data = restrictor_tow$data
)

influ <- Influence$new(occurrence)
influ$calc()

abundance <- survreg(
  Surv(catch) ~ fyear + poly(log(duration), 3) + vessel + target + area_month,
  dist="weibull",
  data=subset(restrictor$data, catch>0)
)

abundance_tow <- survreg(
  Surv(catch) ~ fyear + vessel + poly(log(duration), 3) + area_month + target + poly(bottom, 3) + poly(width, 3),
  dist="weibull",
  data = subset(restrictor_tow$data, catch > 0)
)



fyear <-  seq(from = 1990, to = 2017, by = 1)
coef <- as.data.frame(cbind(fyear, index = c(0,  abundance$coefficients[2:length(fyear)]),
    se = c(0,sqrt(diag(vcov(abundance)))[2:length(fyear)])))
coef[1,3] <- coef[2,3]
coef$index <- exp(coef$index - mean(coef$index))

bincoef <- as.data.frame(cbind(fyear, b_index = c(0, summary.lm(occurrence)$coefficients[2:length(fyear),1]),
    b_se = c(0, summary.lm(occurrence)$coefficients[2:length(fyear),2])))
bincoef[1,3] <- bincoef[2,3]
rawdat <- tapply(restrictor$data$catch[restrictor$data$catch>0], restrictor$data$fyear[restrictor$data$catch>0], length) /
      tapply(restrictor$data$catch, restrictor$data$fyear, length)

prop_yr1 <- 1 - round(100 * (1-rawdat),1)[1]/100
coc <- log(prop_yr1/(1-prop_yr1))
bincoef$b_index <- exp(bincoef[,2] + coc)/(1+exp(bincoef[,2]+coc))

index <- left_join(coef, bincoef, by='fyear')

fyear <-  seq(from = 2008, to = 2017, by=1)
coef <- as.data.frame(cbind(fyear, index = c(0,  abundance_tow$coefficients[2:length(fyear)]),
    se = c(0,sqrt(diag(vcov(abundance_tow)))[2:length(fyear)])))
coef[1,3] <- coef[2,3]
coef$index <- exp(coef$index - mean(coef$index))

bincoef <- as.data.frame(cbind(fyear, b_index = c(0, summary.lm(occurrence_tow)$coefficients[2:length(fyear),1]),
    b_se = c(0, summary.lm(occurrence_tow)$coefficients[2:length(fyear),2])))
bincoef[1,3] <- bincoef[2,3]
rawdat <- tapply(restrictor_tow$data$catch[restrictor_tow$data$catch>0], restrictor_tow$data$fyear[restrictor_tow$data$catch>0], length) /
      tapply(restrictor_tow$data$catch, restrictor_tow$data$fyear, length)

prop_yr1 <- 1 - round(100 * (1-rawdat),1)[1]/100
coc <- log(prop_yr1/(1-prop_yr1))
bincoef$b_index <- exp(bincoef[,2] + coc)/(1+exp(bincoef[,2]+coc))

index_tow <- left_join(coef, bincoef, by='fyear')

table(restrictor$data$fyear)

influ <- Influence$new(survreg(
  Surv(catch) ~ fyear + poly(log(duration), 3) + vessel + area_month + target, data=subset(restrictor$data, catch>0), model = T,
  dist="weibull"),
  data=subset(restrictor$data, catch>0))
influ$calc()
influ_tow <- Influence$new(survreg(
  Surv(catch) ~ fyear + vessel + poly(log(duration), 3) + area_month + target + poly(log(bottom), 3) + poly(log(width), 3),
   data=subset(restrictor_tow$data, catch>0), model = T, dist="weibull"),
  data = subset(restrictor_tow$data, catch > 0))
influ_tow$calc()

#-------------------------------------------------------------------------------

diagnoser <- Diagnoser(abundance, data =subset(restrictor$data, catch > 0))
# diagnoser_tow <- Diagnoser(abundance_tow)
diagnoser_tow <- Diagnoser(abundance_tow, data = subset(restrictor_tow$data, catch > 0))

length(grep('*DCF', subset$data$checks))
table(subset$data$checks)

d_landings$check2 <- rep(NA, nrow(d_landings))
d_landings$check2[grep('*LADTH', d_landings$checks)] <- 'LADTH'
d_landings$check2[grep('*LADUP', d_landings$checks)] <- 'LADUP'
d_landings$check2[grep('*LAGWM', d_landings$checks)] <- 'LAGWM'
d_landings$check2[grep('*LASCD', d_landings$checks)] <- 'LASCD'

options(device="png")
diagnoser$diagnostics_plot()

 as.numeric(max(effort$fishyear, na.rm=TRUE))

 index16 <- data.frame(fyear = seq(from = 1990, to = 2016, by=1),
     index=c(1.248, 1.417, 1.046, 0.975, 1.086, 1.107, 1.177, 0.914, 0.962, 0.883,
             0.966, 1.071, 0.944, 0.929, 1.018, 1.028, 0.911, 0.801, 0.810, 0.868,
             0.953, 0.784, 0.784, 1.005, 1.093, 1.173, 1.373))


save(file = 'gur2.rda', tcer, landings, mhr, est, effort, prop_land, depth_fishery, fisheries,
    dist_df, stats, nz, aggregater, subsetter, subsetter_tow, restrictor, restrictor_tow,
    occurrence, occurrence_tow, corer, corer_tow, index, index_tow, abundance, abundance_tow,
    diagnoser_tow, diagnoser, influ, influ_tow, coast,
    comp, d_landings, allocated, corer16, aggregater16, subsetter16, index16,
    form_plot, historic)
