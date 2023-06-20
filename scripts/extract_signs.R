# Variables
dir = "../data/photos/"

# Extract GPS coordinates
signs_unsplit <- as.data.frame(system(sprintf("exiftool -gpslongitude -gpslatitude -csv -n %s", dir), intern = TRUE))
signs <- data.frame(do.call(rbind, strsplit(as.character(signs_unsplit[,1]),
                                            split=",")))
rm(signs_unsplit)

# Set column names (i.e., add variables) and clean up values a bit
colnames(signs) <- signs[1,]
signs <- signs[2:nrow(signs),]
signs <- data.frame(signs, Text = "", English = "", French = "", Type = "")
signs$SourceFile <- gsub(dir, "", signs$SourceFile)

# Output
write.csv(signs, file = "../data/signs.csv", fileEncoding = "UTF8", row.names = FALSE)

# Cleanup
rm(list = ls())