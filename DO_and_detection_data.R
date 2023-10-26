library(dplyr)
library(lubridate)
library(ggplot2)

## Read in temp and DO csv
setwd("C:/Users/lmcgi/OneDrive/Desktop/CAWS DO and temp data/MWRD_CSVs")
BC_DO <- read_csv("MWRD_DO_I55-BC_June12-July6.csv") # read in MWRD data

## Read in filtered detection dataset
setwd("C:/Users/lmcgi/OneDrive/Desktop/Telemetry analysis")
detections <- read_csv("9_15_23_detections_filtered.csv") # read det data

## subset detections to just Bubbly Creek
detections_BC <- subset(detections, detections$waterway == "Bubbly Creek") 

## Create a CDT column to line up with MWRD environ data
detections_BC$detection_timestamp_CDT <- with_tz(detections_BC$detection_timestamp_utc,
                                         tzone = "America/Chicago") 

## Subset data for the timespan of environ data
start_date <- as.POSIXct("2023-06-12 00:00:00", tz = "America/Chicago")
end_date <- as.POSIXct("2023-07-05 23:59:59", tz = "America/Chicago")

BC_subset <- subset(detections_BC, detection_timestamp_CDT >= start_date & 
                      detection_timestamp_CDT <= end_date)

## Write out subset of data to upload to github
#write_csv(BC_subset, "Bubbly_creek_June_detections.csv")

BC_subset <- read_csv("Bubbly_creek_June_detections.csv")

## Count individuals per hour. First make blank_df to store values
start_times <- seq.POSIXt(from = as.POSIXct("2023-06-12 00:00:00", tz = "America/Chicago"),
                          to = as.POSIXct("2023-07-05 23:00:00", tz = "America/Chicago"),
                          by = "1 hour")
end_times <- start_times + 3600  # Add 3600 seconds (1 hour) to get end times

## Blank df
summary_df <- data.frame(
  Start_Time = start_times,
  End_Time = end_times,
  Unique_Transmitter_ids = numeric(length(start_times))
)

## Count individuals detected per hour
for (i in 1:length(start_times)) {
  block_subset <- BC_subset %>%
    filter(detection_timestamp_CDT >= start_times[i] & detection_timestamp_CDT < end_times[i])
  unique_transmitter_ids <- length(unique(block_subset$Transmitter_id))
  summary_df$Unique_Transmitter_ids[i] <- unique_transmitter_ids
}

### Combine df
# remove 7-6 data from BC_DO (no environmental measurements)
BC_DO <- BC_DO[2:576,]
summary_df <- summary_df[1:575,]

BC_DO_det <- BC_DO %>% 
  left_join(summary_df, join_by("Date_Time" == "End_Time"))

## linear regression
attach(BC_DO_det)
lm <- lm(Unique_Transmitter_ids ~ DO_mgL, data = BC_DO_det)
summary(lm)

## Plot
p1 <- ggplot(data = BC_DO_det) +
  aes(x = Unique_Transmitter_ids, y = DO_mgL)+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Number of individuals detected")+
  ylab("Dissolved oxygen (mg/L)")+
  ggtitle("Dissolved oxygen vs tagged indiviudals present in Bubbly Creek")+
  theme_bw()

## save
ggsave(filename = "DO vs fish presence in BC.png", p1)
