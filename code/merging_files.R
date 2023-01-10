# area_vec <- list.files(path="C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/Outputs/Outputs/", pattern="area.csv", all.files=FALSE, full.names=FALSE)
bin_vec <- list.files(path="C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/Outputs/Outputs/", pattern="bin.csv", all.files=FALSE, full.names=FALSE)

# larger_sample_area <- data.frame()
# for (game in area_vec) {
#   df <- read.csv(paste("C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/Outputs/Outputs/", game, sep=""))
#   larger_sample_area <- rbind(larger_sample_area, df)
# }

# write.csv(larger_sample_area, "C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23\\larger_area_sample.csv", row.names = F)

larger_sample_bin <- data.frame()
for (game in bin_vec) {
  df <- read.csv(paste("C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/Outputs/Outputs/", game, sep=""))
  larger_sample_bin <- rbind(larger_sample_bin, df)
}

write.csv(larger_sample_bin, "C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23\\larger_bin_sample.csv", row.names = F)