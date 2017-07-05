library(foreign) # to read the .dbf file housing the raster summary table

# load total ccap data
ccap <- read.dbf(paste(getwd(), "/data/AllStates1996to2010wGreatLakes.dbf", sep=""))

classes <- (unique(ccap$F2010_Clas)) # get unique classes

pixels <-c() # empty storage vector
for (i in 1:length(classes)) { # for each unique class
  sub_df <- subset(ccap, F2010_Clas == classes[i]) # subset ccap for 2010 class
  n = sum(sub_df$Count) # summ the pixel counts in that class
  pixels <- c(pixels, n) # store pixel count
}

ccapPixelCounts <- data.frame(classes, pixels)
ccapPixelCounts <- subset(ccapPixelCounts, pixels > 0)
write.table(ccapPixelCounts, paste(getwd(), "/data/ccapPixelCounts.csv", sep=""), sep=",", row.names=F)

change_df <- subset(ccap, F1996_Clas != F2010_Clas) # subset all changes
change_pixels <- sum(change_df$Count)

noChange_df <- subset(ccap, F1996_Clas == F2010_Clas) # subset all no changes
noChange_pixels <- sum(noChange_df$Count)

cncPixelCounts <- data.frame(classes = c("No.Change", "Change"), pixels = c(noChange_pixels, change_pixels))
write.table(cncPixelCounts, paste(getwd(), "/data/cncPixelCounts.csv", sep=""), sep=",", row.names=F)
