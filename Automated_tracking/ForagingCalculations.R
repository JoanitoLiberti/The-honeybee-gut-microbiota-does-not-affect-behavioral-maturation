# This script focuses on behavioral maturation,
# looking at how foraging effort changes over the experiment for CL and MD bees (referred to as C and D here)

MAINDIR <- "/Volumes/Lacie/BM"

############################
############################

INDIR   <- paste(MAINDIR, "RawData", sep = "/")
OUTDIR  <- paste(MAINDIR, "ProcessedData", sep = "/")
FIGSDIR <- paste(MAINDIR, "Figures", sep = "/")
library('jpeg')

# Read in the trajectory summary data
setwd(INDIR)
files <- list.files(pattern="TrajectorySummary")
for (file in files){
  assign(sub("\\..*", "", file),readRDS(file))
}

# Trim as in script 01 to match start and end times and data black-outs
TrajectorySummary_1C <- TrajectorySummary_1C[TrajectorySummary_1C$start > "2020-05-13 24:00:00",]
TrajectorySummary_1D <- TrajectorySummary_1D[TrajectorySummary_1D$start > "2020-05-13 24:00:00",]
TrajectorySummary_2C <- TrajectorySummary_2C[TrajectorySummary_2C$start > "2020-05-27 24:00:00",]
TrajectorySummary_2D <- TrajectorySummary_2D[TrajectorySummary_2D$start > "2020-05-27 24:00:00",]
TrajectorySummary_3C <- TrajectorySummary_3C[TrajectorySummary_3C$start > "2020-06-03 24:00:00",]
TrajectorySummary_3D <- TrajectorySummary_3D[TrajectorySummary_3D$start > "2020-06-03 24:00:00",]
TrajectorySummary_4C <- TrajectorySummary_4C[TrajectorySummary_4C$start > "2020-06-10 24:00:00",]
TrajectorySummary_4D <- TrajectorySummary_4D[TrajectorySummary_4D$start > "2020-06-10 24:00:00",]
TrajectorySummary_5C <- TrajectorySummary_5C[TrajectorySummary_5C$start > "2020-06-24 24:00:00",]
TrajectorySummary_5D <- TrajectorySummary_5D[TrajectorySummary_5D$start > "2020-06-24 24:00:00",]
TrajectorySummary_6C <- TrajectorySummary_6C[TrajectorySummary_6C$start > "2020-07-01 24:00:00",]
TrajectorySummary_6D <- TrajectorySummary_6D[TrajectorySummary_6D$start > "2020-07-01 24:00:00",]
TrajectorySummary_7C <- TrajectorySummary_7C[TrajectorySummary_7C$start > "2020-07-15 24:00:00",]
TrajectorySummary_7D <- TrajectorySummary_7D[TrajectorySummary_7D$start > "2020-07-15 24:00:00",]
TrajectorySummary_8C <- TrajectorySummary_8C[TrajectorySummary_8C$start > "2020-07-22 24:00:00",]
TrajectorySummary_8D <- TrajectorySummary_8D[TrajectorySummary_8D$start > "2020-07-22 24:00:00",]
TrajectorySummary_9C <- TrajectorySummary_9C[TrajectorySummary_9C$start > "2020-09-16 24:00:00",]
TrajectorySummary_9D <- TrajectorySummary_9D[TrajectorySummary_9D$start > "2020-09-16 24:00:00",]
TrajectorySummary_1C <- TrajectorySummary_1C[TrajectorySummary_1C$start < "2020-05-20 08:00:00",]
TrajectorySummary_1D <- TrajectorySummary_1D[TrajectorySummary_1D$start < "2020-05-20 08:00:00",]
TrajectorySummary_2C <- TrajectorySummary_2C[TrajectorySummary_2C$start < "2020-06-03 08:00:00",]
TrajectorySummary_2D <- TrajectorySummary_2D[TrajectorySummary_2D$start < "2020-06-03 08:00:00",]
TrajectorySummary_3C <- TrajectorySummary_3C[TrajectorySummary_3C$start < "2020-06-10 08:00:00",]
TrajectorySummary_3D <- TrajectorySummary_3D[TrajectorySummary_3D$start < "2020-06-10 08:00:00",]
TrajectorySummary_4C <- TrajectorySummary_4C[TrajectorySummary_4C$start < "2020-06-17 08:00:00",]
TrajectorySummary_4D <- TrajectorySummary_4D[TrajectorySummary_4D$start < "2020-06-17 08:00:00",]
TrajectorySummary_5C <- TrajectorySummary_5C[TrajectorySummary_5C$start < "2020-07-01 08:00:00",]
TrajectorySummary_5D <- TrajectorySummary_5D[TrajectorySummary_5D$start < "2020-07-01 08:00:00",]
TrajectorySummary_6C <- TrajectorySummary_6C[TrajectorySummary_6C$start < "2020-07-08 08:00:00",]
TrajectorySummary_6D <- TrajectorySummary_6D[TrajectorySummary_6D$start < "2020-07-08 08:00:00",]
TrajectorySummary_7C <- TrajectorySummary_7C[TrajectorySummary_7C$start < "2020-07-22 08:00:00",]
TrajectorySummary_7D <- TrajectorySummary_7D[TrajectorySummary_7D$start < "2020-07-22 08:00:00",]
TrajectorySummary_8C <- TrajectorySummary_8C[TrajectorySummary_8C$start < "2020-07-29 08:00:00",]
TrajectorySummary_8D <- TrajectorySummary_8D[TrajectorySummary_8D$start < "2020-07-29 08:00:00",]
TrajectorySummary_9C <- TrajectorySummary_9C[TrajectorySummary_9C$start < "2020-09-23 08:00:00",]
TrajectorySummary_9D <- TrajectorySummary_9D[TrajectorySummary_9D$start < "2020-09-23 08:00:00",]
TrajectorySummary_3C <- TrajectorySummary_3C[!("2020-06-04 21:01:20" < TrajectorySummary_3C$start & TrajectorySummary_3C$start < "2020-06-04 21:01:20"),]
TrajectorySummary_3C <- TrajectorySummary_3C[!("2020-06-06 09:01:04" < TrajectorySummary_3C$start & TrajectorySummary_3C$start < "2020-06-06 11:56:55"),]
TrajectorySummary_3C <- TrajectorySummary_3C[!("2020-06-07 07:50:31" < TrajectorySummary_3C$start & TrajectorySummary_3C$start < "2020-06-07 09:29:01"),]
TrajectorySummary_3C <- TrajectorySummary_3C[!("2020-06-07 14:55:39" < TrajectorySummary_3C$start & TrajectorySummary_3C$start < "2020-06-07 15:38:22"),]
TrajectorySummary_3D <- TrajectorySummary_3D[!("2020-06-04 21:01:20" < TrajectorySummary_3D$start & TrajectorySummary_3D$start < "2020-06-04 21:01:20"),]
TrajectorySummary_3D <- TrajectorySummary_3D[!("2020-06-06 09:01:04" < TrajectorySummary_3D$start & TrajectorySummary_3D$start < "2020-06-06 11:56:55"),]
TrajectorySummary_3D <- TrajectorySummary_3D[!("2020-06-07 07:50:31" < TrajectorySummary_3D$start & TrajectorySummary_3D$start < "2020-06-07 09:29:01"),]
TrajectorySummary_3D <- TrajectorySummary_3D[!("2020-06-07 14:55:39" < TrajectorySummary_3D$start & TrajectorySummary_3D$start < "2020-06-07 15:38:22"),]
TrajectorySummary_4C <- TrajectorySummary_4C[!("2020-06-14 13:46:30" < TrajectorySummary_4C$start & TrajectorySummary_4C$start < "2020-06-14 20:08:30"),]
TrajectorySummary_4C <- TrajectorySummary_4C[!("2020-06-15 22:59:18" < TrajectorySummary_4C$start & TrajectorySummary_4C$start < "2020-06-16 06:35:05"),]
TrajectorySummary_4D <- TrajectorySummary_4D[!("2020-06-14 13:46:30" < TrajectorySummary_4D$start & TrajectorySummary_4D$start < "2020-06-14 20:08:30"),]
TrajectorySummary_4D <- TrajectorySummary_4D[!("2020-06-15 22:59:18" < TrajectorySummary_4D$start & TrajectorySummary_4D$start < "2020-06-16 06:35:05"),]
TrajectorySummary_5C <- TrajectorySummary_5C[!("2020-06-26 20:38:51" < TrajectorySummary_5C$start & TrajectorySummary_5C$start < "2020-06-27 07:58:18"),]
TrajectorySummary_5C <- TrajectorySummary_5C[!("2020-06-27 13:48:20" < TrajectorySummary_5C$start & TrajectorySummary_5C$start < "2020-06-27 16:39:02"),]
TrajectorySummary_5C <- TrajectorySummary_5C[!("2020-06-30 28:58:42" < TrajectorySummary_5C$start),]
TrajectorySummary_5D <- TrajectorySummary_5D[!("2020-06-26 20:38:51" < TrajectorySummary_5D$start & TrajectorySummary_5D$start < "2020-06-27 07:58:18"),]
TrajectorySummary_5D <- TrajectorySummary_5D[!("2020-06-27 13:48:20" < TrajectorySummary_5D$start & TrajectorySummary_5D$start < "2020-06-27 16:39:02"),]
TrajectorySummary_5D <- TrajectorySummary_5D[!("2020-06-30 28:58:42" < TrajectorySummary_5D$start),]
TrajectorySummary_7C <- TrajectorySummary_7C[!("2020-07-20 17:27:42" < TrajectorySummary_7C$start & TrajectorySummary_7C$start < "2020-07-20 18:24:29"),]
TrajectorySummary_7C <- TrajectorySummary_7C[!("2020-07-20 23:47:14" < TrajectorySummary_7C$start & TrajectorySummary_7C$start < "2020-07-21 13:04:28"),]
TrajectorySummary_7D <- TrajectorySummary_7D[!("2020-07-20 17:27:42" < TrajectorySummary_7D$start & TrajectorySummary_7D$start < "2020-07-20 18:24:29"),]
TrajectorySummary_7D <- TrajectorySummary_7D[!("2020-07-20 23:47:14" < TrajectorySummary_7D$start & TrajectorySummary_7D$start < "2020-07-21 13:04:28"),]

# Read in the outlier data
setwd(OUTDIR)
SocOut1C <- read.csv("SocOut1C.csv")$x
SocOut2C <- read.csv("SocOut2C.csv")$x
SocOut3C <- read.csv("SocOut3C.csv")$x
SocOut4C <- read.csv("SocOut4C.csv")$x
SocOut5C <- read.csv("SocOut5C.csv")$x
SocOut6C <- read.csv("SocOut6C.csv")$x
SocOut7C <- read.csv("SocOut7C.csv")$x
SocOut8C <- read.csv("SocOut8C.csv")$x
SocOut9C <- read.csv("SocOut9C.csv")$x
SocOut1D <- read.csv("SocOut1D.csv")$x
SocOut2D <- read.csv("SocOut2D.csv")$x
SocOut3D <- read.csv("SocOut3D.csv")$x
SocOut4D <- read.csv("SocOut4D.csv")$x
SocOut5D <- read.csv("SocOut5D.csv")$x
SocOut6D <- read.csv("SocOut6D.csv")$x
SocOut7D <- read.csv("SocOut7D.csv")$x
SocOut8D <- read.csv("SocOut8D.csv")$x
SocOut9D <- read.csv("SocOut9D.csv")$x

# Remove outliers from trajectory summary data
TrajectorySummary_1C <- TrajectorySummary_1C[!TrajectorySummary_1C$antID %in% SocOut1C,]
TrajectorySummary_1D <- TrajectorySummary_1D[!TrajectorySummary_1D$antID %in% SocOut1D,]
TrajectorySummary_2C <- TrajectorySummary_2C[!TrajectorySummary_2C$antID %in% SocOut2C,]
TrajectorySummary_2D <- TrajectorySummary_2D[!TrajectorySummary_2D$antID %in% SocOut2D,]
TrajectorySummary_3C <- TrajectorySummary_3C[!TrajectorySummary_3C$antID %in% SocOut3C,]
TrajectorySummary_3D <- TrajectorySummary_3D[!TrajectorySummary_3D$antID %in% SocOut3D,]
TrajectorySummary_4C <- TrajectorySummary_4C[!TrajectorySummary_4C$antID %in% SocOut4C,]
TrajectorySummary_4D <- TrajectorySummary_4D[!TrajectorySummary_4D$antID %in% SocOut4D,]
TrajectorySummary_5C <- TrajectorySummary_5C[!TrajectorySummary_5C$antID %in% SocOut5C,]
TrajectorySummary_5D <- TrajectorySummary_5D[!TrajectorySummary_5D$antID %in% SocOut5D,]
TrajectorySummary_6C <- TrajectorySummary_6C[!TrajectorySummary_6C$antID %in% SocOut6C,]
TrajectorySummary_6D <- TrajectorySummary_6D[!TrajectorySummary_6D$antID %in% SocOut6D,]
TrajectorySummary_7C <- TrajectorySummary_7C[!TrajectorySummary_7C$antID %in% SocOut7C,]
TrajectorySummary_7D <- TrajectorySummary_7D[!TrajectorySummary_7D$antID %in% SocOut7D,]
TrajectorySummary_8C <- TrajectorySummary_8C[!TrajectorySummary_8C$antID %in% SocOut8C,]
TrajectorySummary_8D <- TrajectorySummary_8D[!TrajectorySummary_8D$antID %in% SocOut8D,]
TrajectorySummary_9C <- TrajectorySummary_9C[!TrajectorySummary_9C$antID %in% SocOut9C,]
TrajectorySummary_9D <- TrajectorySummary_9D[!TrajectorySummary_9D$antID %in% SocOut9D,]

# Bin the trajectory data into two hour periods to allow for aggregation and average space use to be calculated per bin
TrajectorySummary_1C$startBin <- cut(TrajectorySummary_1C$start, breaks="2 hours")
TrajectorySummary_2C$startBin <- cut(TrajectorySummary_2C$start, breaks="2 hours")
TrajectorySummary_3C$startBin <- cut(TrajectorySummary_3C$start, breaks="2 hours")
TrajectorySummary_4C$startBin <- cut(TrajectorySummary_4C$start, breaks="2 hours")
TrajectorySummary_5C$startBin <- cut(TrajectorySummary_5C$start, breaks="2 hours")
TrajectorySummary_6C$startBin <- cut(TrajectorySummary_6C$start, breaks="2 hours")
TrajectorySummary_7C$startBin <- cut(TrajectorySummary_7C$start, breaks="2 hours")
TrajectorySummary_8C$startBin <- cut(TrajectorySummary_8C$start, breaks="2 hours")
TrajectorySummary_9C$startBin <- cut(TrajectorySummary_9C$start, breaks="2 hours")
TrajectorySummary_1D$startBin <- cut(TrajectorySummary_1D$start, breaks="2 hours")
TrajectorySummary_2D$startBin <- cut(TrajectorySummary_2D$start, breaks="2 hours")
TrajectorySummary_3D$startBin <- cut(TrajectorySummary_3D$start, breaks="2 hours")
TrajectorySummary_4D$startBin <- cut(TrajectorySummary_4D$start, breaks="2 hours")
TrajectorySummary_5D$startBin <- cut(TrajectorySummary_5D$start, breaks="2 hours")
TrajectorySummary_6D$startBin <- cut(TrajectorySummary_6D$start, breaks="2 hours")
TrajectorySummary_7D$startBin <- cut(TrajectorySummary_7D$start, breaks="2 hours")
TrajectorySummary_8D$startBin <- cut(TrajectorySummary_8D$start, breaks="2 hours")
TrajectorySummary_9D$startBin <- cut(TrajectorySummary_9D$start, breaks="2 hours")

# Calculate age at first foraging
# Proportion of time spent foraging
# And total number of foraging trips
First_forage_1C <- c()
Proportion_foraging_1C <- c()
Number_trips_1C <- c()
for (id in unique(TrajectorySummary_1C$antID)){
  trajs <- TrajectorySummary_1C[TrajectorySummary_1C$antID == id,]
  for_trajs <- trajs[trajs$space == 1,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_1C$startBin[1], units = "secs")
  First_forage_1C <- c(First_forage_1C, first_for)
  Proportion_foraging_1C <- c(Proportion_foraging_1C, 3 - mean(trajs$space))
  Number_trips_1C <- c(Number_trips_1C, sum(abs(diff(trajs$space))))
}
First_forage_2C <- c()
Proportion_foraging_2C <- c()
Number_trips_2C <- c()
for (id in unique(TrajectorySummary_2C$antID)){
  trajs <- TrajectorySummary_2C[TrajectorySummary_2C$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_2C$startBin[1], units = "secs")
  First_forage_2C <- c(First_forage_2C, first_for)
  Proportion_foraging_2C <- c(Proportion_foraging_2C, mean(trajs$space))
  Number_trips_2C <- c(Number_trips_2C, sum(abs(diff(trajs$space))))
}
First_forage_3C <- c()
Proportion_foraging_3C <- c()
Number_trips_3C <- c()
for (id in unique(TrajectorySummary_3C$antID)){
  trajs <- TrajectorySummary_3C[TrajectorySummary_3C$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_3C$startBin[1], units = "secs")
  First_forage_3C <- c(First_forage_3C, first_for)
  Proportion_foraging_3C <- c(Proportion_foraging_3C, mean(trajs$space))
  Number_trips_3C <- c(Number_trips_3C, sum(abs(diff(trajs$space))))
}
First_forage_4C <- c()
Proportion_foraging_4C <- c()
Number_trips_4C <- c()
for (id in unique(TrajectorySummary_4C$antID)){
  trajs <- TrajectorySummary_4C[TrajectorySummary_4C$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_4C$startBin[1], units = "secs")
  First_forage_4C <- c(First_forage_4C, first_for)
  Proportion_foraging_4C <- c(Proportion_foraging_4C, mean(trajs$space))
  Number_trips_4C <- c(Number_trips_4C, sum(abs(diff(trajs$space))))
}
First_forage_5C <- c()
Proportion_foraging_5C <- c()
Number_trips_5C <- c()
for (id in unique(TrajectorySummary_5C$antID)){
  trajs <- TrajectorySummary_5C[TrajectorySummary_5C$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_5C$startBin[1], units = "secs")
  First_forage_5C <- c(First_forage_5C, first_for)
  Proportion_foraging_5C <- c(Proportion_foraging_5C, mean(trajs$space))
  Number_trips_5C <- c(Number_trips_5C, sum(abs(diff(trajs$space))))
}
First_forage_6C <- c()
Proportion_foraging_6C <- c()
Number_trips_6C <- c()
for (id in unique(TrajectorySummary_6C$antID)){
  trajs <- TrajectorySummary_6C[TrajectorySummary_6C$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_6C$startBin[1], units = "secs")
  First_forage_6C <- c(First_forage_6C, first_for)
  Proportion_foraging_6C <- c(Proportion_foraging_6C, mean(trajs$space))
  Number_trips_6C <- c(Number_trips_6C, sum(abs(diff(trajs$space))))
}
First_forage_7C <- c()
Proportion_foraging_7C <- c()
Number_trips_7C <- c()
for (id in unique(TrajectorySummary_7C$antID)){
  trajs <- TrajectorySummary_7C[TrajectorySummary_7C$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_7C$startBin[1], units = "secs")
  First_forage_7C <- c(First_forage_7C, first_for)
  Proportion_foraging_7C <- c(Proportion_foraging_7C, mean(trajs$space))
  Number_trips_7C <- c(Number_trips_7C, sum(abs(diff(trajs$space))))
}
First_forage_8C <- c()
Proportion_foraging_8C <- c()
Number_trips_8C <- c()
for (id in unique(TrajectorySummary_8C$antID)){
  trajs <- TrajectorySummary_8C[TrajectorySummary_8C$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_8C$startBin[1], units = "secs")
  First_forage_8C <- c(First_forage_8C, first_for)
  Proportion_foraging_8C <- c(Proportion_foraging_8C, mean(trajs$space))
  Number_trips_8C <- c(Number_trips_8C, sum(abs(diff(trajs$space))))
}
First_forage_9C <- c()
Proportion_foraging_9C <- c()
Number_trips_9C <- c()
for (id in unique(TrajectorySummary_9C$antID)){
  trajs <- TrajectorySummary_9C[TrajectorySummary_9C$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_9C$startBin[1], units = "secs")
  First_forage_9C <- c(First_forage_9C, first_for)
  Proportion_foraging_9C <- c(Proportion_foraging_9C, mean(trajs$space))
  Number_trips_9C <- c(Number_trips_9C, sum(abs(diff(trajs$space))))
}
First_forage_1D <- c()
Proportion_foraging_1D <- c()
Number_trips_1D <- c()
for (id in unique(TrajectorySummary_1D$antID)){
  trajs <- TrajectorySummary_1D[TrajectorySummary_1D$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_1D$startBin[1], units = "secs")
  First_forage_1D <- c(First_forage_1D, first_for)
  Proportion_foraging_1D <- c(Proportion_foraging_1D, mean(trajs$space))
  Number_trips_1D <- c(Number_trips_1D, sum(abs(diff(trajs$space))))
}
First_forage_2D <- c()
Proportion_foraging_2D <- c()
Number_trips_2D <- c()
for (id in unique(TrajectorySummary_2D$antID)){
  trajs <- TrajectorySummary_2D[TrajectorySummary_2D$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_2D$startBin[1], units = "secs")
  First_forage_2D <- c(First_forage_2D, first_for)
  Proportion_foraging_2D <- c(Proportion_foraging_2D, mean(trajs$space))
  Number_trips_2D <- c(Number_trips_2D, sum(abs(diff(trajs$space))))
}
First_forage_3D <- c()
Proportion_foraging_3D <- c()
Number_trips_3D <- c()
for (id in unique(TrajectorySummary_3D$antID)){
  trajs <- TrajectorySummary_3D[TrajectorySummary_3D$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_3D$startBin[1], units = "secs")
  First_forage_3D <- c(First_forage_3D, first_for)
  Proportion_foraging_3D <- c(Proportion_foraging_3D, mean(trajs$space))
  Number_trips_3D <- c(Number_trips_3D, sum(abs(diff(trajs$space))))
}
First_forage_4D <- c()
Proportion_foraging_4D <- c()
Number_trips_4D <- c()
for (id in unique(TrajectorySummary_4D$antID)){
  trajs <- TrajectorySummary_4D[TrajectorySummary_4D$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_4D$startBin[1], units = "secs")
  First_forage_4D <- c(First_forage_4D, first_for)
  Proportion_foraging_4D <- c(Proportion_foraging_4D, mean(trajs$space))
  Number_trips_4D <- c(Number_trips_4D, sum(abs(diff(trajs$space))))
}
First_forage_5D <- c()
Proportion_foraging_5D <- c()
Number_trips_5D <- c()
for (id in unique(TrajectorySummary_5D$antID)){
  trajs <- TrajectorySummary_5D[TrajectorySummary_5D$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_5D$startBin[1], units = "secs")
  First_forage_5D <- c(First_forage_5D, first_for)
  Proportion_foraging_5D <- c(Proportion_foraging_5D, mean(trajs$space))
  Number_trips_5D <- c(Number_trips_5D, sum(abs(diff(trajs$space))))
}
First_forage_6D <- c()
Proportion_foraging_6D <- c()
Number_trips_6D <- c()
for (id in unique(TrajectorySummary_6D$antID)){
  trajs <- TrajectorySummary_6D[TrajectorySummary_6D$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_6D$startBin[1], units = "secs")
  First_forage_6D <- c(First_forage_6D, first_for)
  Proportion_foraging_6D <- c(Proportion_foraging_6D, mean(trajs$space))
  Number_trips_6D <- c(Number_trips_6D, sum(abs(diff(trajs$space))))
}
First_forage_7D <- c()
Proportion_foraging_7D <- c()
Number_trips_7D <- c()
for (id in unique(TrajectorySummary_7D$antID)){
  trajs <- TrajectorySummary_7D[TrajectorySummary_7D$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_7D$startBin[1], units = "secs")
  First_forage_7D <- c(First_forage_7D, first_for)
  Proportion_foraging_7D <- c(Proportion_foraging_7D, mean(trajs$space))
  Number_trips_7D <- c(Number_trips_7D, sum(abs(diff(trajs$space))))
}
First_forage_8D <- c()
Proportion_foraging_8D <- c()
Number_trips_8D <- c()
for (id in unique(TrajectorySummary_8D$antID)){
  trajs <- TrajectorySummary_8D[TrajectorySummary_8D$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_8D$startBin[1], units = "secs")
  First_forage_8D <- c(First_forage_8D, first_for)
  Proportion_foraging_8D <- c(Proportion_foraging_8D, mean(trajs$space))
  Number_trips_8D <- c(Number_trips_8D, sum(abs(diff(trajs$space))))
}
First_forage_9D <- c()
Proportion_foraging_9D <- c()
Number_trips_9D <- c()
for (id in unique(TrajectorySummary_9D$antID)){
  trajs <- TrajectorySummary_9D[TrajectorySummary_9D$antID == id,]
  for_trajs <- trajs[trajs$space == 2,]
  first_for <- difftime(for_trajs$start[1], TrajectorySummary_9D$startBin[1], units = "secs")
  First_forage_9D <- c(First_forage_9D, first_for)
  Proportion_foraging_9D <- c(Proportion_foraging_9D, mean(trajs$space))
  Number_trips_9D <- c(Number_trips_9D, sum(abs(diff(trajs$space))))
}

# Construct one dataframe per subcolony
Foraging_1C <- data.frame(rep = rep("1C", length(unique(TrajectorySummary_1C$antID))),
                          bee = unique(TrajectorySummary_1C$antID),
                          first = First_forage_1C,
                          prop = Proportion_foraging_1C,
                          trips = Number_trips_1C)
Foraging_2C <- data.frame(rep = rep("2C", length(unique(TrajectorySummary_2C$antID))),
                          bee = unique(TrajectorySummary_2C$antID),
                          first = First_forage_2C,
                          prop = Proportion_foraging_2C,
                          trips = Number_trips_2C)
Foraging_3C <- data.frame(rep = rep("3C", length(unique(TrajectorySummary_3C$antID))),
                          bee = unique(TrajectorySummary_3C$antID),
                          first = First_forage_3C,
                          prop = Proportion_foraging_3C,
                          trips = Number_trips_3C)
Foraging_4C <- data.frame(rep = rep("4C", length(unique(TrajectorySummary_4C$antID))),
                          bee = unique(TrajectorySummary_4C$antID),
                          first = First_forage_4C,
                          prop = Proportion_foraging_4C,
                          trips = Number_trips_4C)
Foraging_5C <- data.frame(rep = rep("5C", length(unique(TrajectorySummary_5C$antID))),
                          bee = unique(TrajectorySummary_5C$antID),
                          first = First_forage_5C,
                          prop = Proportion_foraging_5C,
                          trips = Number_trips_5C)
Foraging_6C <- data.frame(rep = rep("6C", length(unique(TrajectorySummary_6C$antID))),
                          bee = unique(TrajectorySummary_6C$antID),
                          first = First_forage_6C,
                          prop = Proportion_foraging_6C,
                          trips = Number_trips_6C)
Foraging_7C <- data.frame(rep = rep("7C", length(unique(TrajectorySummary_7C$antID))),
                          bee = unique(TrajectorySummary_7C$antID),
                          first = First_forage_7C,
                          prop = Proportion_foraging_7C,
                          trips = Number_trips_7C)
Foraging_8C <- data.frame(rep = rep("8C", length(unique(TrajectorySummary_8C$antID))),
                          bee = unique(TrajectorySummary_8C$antID),
                          first = First_forage_8C,
                          prop = Proportion_foraging_8C,
                          trips = Number_trips_8C)
Foraging_9C <- data.frame(rep = rep("9C", length(unique(TrajectorySummary_9C$antID))),
                          bee = unique(TrajectorySummary_9C$antID),
                          first = First_forage_9C,
                          prop = Proportion_foraging_9C,
                          trips = Number_trips_9C)
Foraging_1D <- data.frame(rep = rep("1D", length(unique(TrajectorySummary_1D$antID))),
                          bee = unique(TrajectorySummary_1D$antID),
                          first = First_forage_1D,
                          prop = Proportion_foraging_1D,
                          trips = Number_trips_1D)
Foraging_2D <- data.frame(rep = rep("2D", length(unique(TrajectorySummary_2D$antID))),
                          bee = unique(TrajectorySummary_2D$antID),
                          first = First_forage_2D,
                          prop = Proportion_foraging_2D,
                          trips = Number_trips_2D)
Foraging_3D <- data.frame(rep = rep("3D", length(unique(TrajectorySummary_3D$antID))),
                          bee = unique(TrajectorySummary_3D$antID),
                          first = First_forage_3D,
                          prop = Proportion_foraging_3D,
                          trips = Number_trips_3D)
Foraging_4D <- data.frame(rep = rep("4D", length(unique(TrajectorySummary_4D$antID))),
                          bee = unique(TrajectorySummary_4D$antID),
                          first = First_forage_4D,
                          prop = Proportion_foraging_4D,
                          trips = Number_trips_4D)
Foraging_5D <- data.frame(rep = rep("5D", length(unique(TrajectorySummary_5D$antID))),
                          bee = unique(TrajectorySummary_5D$antID),
                          first = First_forage_5D,
                          prop = Proportion_foraging_5D,
                          trips = Number_trips_5D)
Foraging_6D <- data.frame(rep = rep("6D", length(unique(TrajectorySummary_6D$antID))),
                          bee = unique(TrajectorySummary_6D$antID),
                          first = First_forage_6D,
                          prop = Proportion_foraging_6D,
                          trips = Number_trips_6D)
Foraging_7D <- data.frame(rep = rep("7D", length(unique(TrajectorySummary_7D$antID))),
                          bee = unique(TrajectorySummary_7D$antID),
                          first = First_forage_7D,
                          prop = Proportion_foraging_7D,
                          trips = Number_trips_7D)
Foraging_8D <- data.frame(rep = rep("8D", length(unique(TrajectorySummary_8D$antID))),
                          bee = unique(TrajectorySummary_8D$antID),
                          first = First_forage_8D,
                          prop = Proportion_foraging_8D,
                          trips = Number_trips_8D)
Foraging_9D <- data.frame(rep = rep("9D", length(unique(TrajectorySummary_9D$antID))),
                          bee = unique(TrajectorySummary_9D$antID),
                          first = First_forage_9D,
                          prop = Proportion_foraging_9D,
                          trips = Number_trips_9D)

# Combine into single data-frame
ALL_foraging <- rbind(Foraging_1C, Foraging_2C, Foraging_3C, Foraging_4C, Foraging_5C,
                      Foraging_6C, Foraging_7C, Foraging_8C, Foraging_9C, Foraging_1D,
                      Foraging_2D, Foraging_3D, Foraging_4D, Foraging_5D, Foraging_6D, 
                      Foraging_7D, Foraging_8D, Foraging_9D)

ALL_foraging$prop <- ALL_foraging$prop-1

# Save
setwd(OUTDIR)
write.csv(ALL_foraging, "AllForagingData.csv", row.names = FALSE)
