library(prism)
library(raster)
library(viridis)

# Set the directory where PRISM data will be downloaded and saved
prism_data_path <- "C:/Users/56328/OneDrive - ICF/Documents/prismtmp/"
prism_set_dl_dir(prism_data_path)

# Define the range of years
start_year <- 2018
end_year <- 2022
num_years <- end_year - start_year + 1

# Initialize matrices to store the counts for each cell over the years
count_over_35_matrix <- matrix(0, nrow(temp_raster), ncol(temp_raster))
count_over_40_matrix <- matrix(0, nrow(temp_raster), ncol(temp_raster))

# Loop through each year
for (year in start_year:end_year) {
  # Define the start and end dates for the year
  start_date <- as.Date(paste0(year, "-07-01"))
  end_date <- as.Date(paste0(year, "-07-05"))
  
  # Download daily maximum temperature data for the year
  get_prism_dailys(type = "tmax", minDate = start_date, maxDate = end_date, keepZip = FALSE)
  
  # Get the list of downloaded files for the year
  daily_files <- prism_archive_subset(type = "tmax", temp_period = "daily", minDate = start_date, maxDate = end_date)
  
  # Initialize matrices to store the counts for the year
  year_count_over_35 <- NULL
  year_count_over_40 <- NULL
  
  # Loop through each file (each day within the year)
  for (file in daily_files) {
    # Construct the full path to the .bil file
    bil_file_path <- paste0(prism_data_path, file, "/", file, ".bil")
    
    # Read the raster file
    temp_raster <- raster(bil_file_path)
    
    # Convert raster values to a matrix for processing
    temp_values <- as.matrix(temp_raster)
    
    # Update the counts for the year
    if (is.null(year_count_over_35)) {
      year_count_over_35 <- (temp_values > 35) & !is.na(temp_values)
      year_count_over_40 <- (temp_values > 40) & !is.na(temp_values)
    } else {
      year_count_over_35 <- year_count_over_35 + ((temp_values > 35) & !is.na(temp_values))
      year_count_over_40 <- year_count_over_40 + ((temp_values > 40) & !is.na(temp_values))
    }
  }
  
  # Update the counts for each cell over the years
  count_over_35_matrix <- count_over_35_matrix + matrix(year_count_over_35, nrow = nrow(temp_raster), ncol = ncol(temp_raster))
  count_over_40_matrix <- count_over_40_matrix + matrix(year_count_over_40, nrow = nrow(temp_raster), ncol = ncol(temp_raster))
}

# Calculate the average for each cell over the years
average_over_35_matrix <- count_over_35_matrix / num_years
average_over_40_matrix <- count_over_40_matrix / num_years

# Convert the average matrices to rasters
average_over_35_raster <- raster(average_over_35_matrix)
average_over_40_raster <- raster(average_over_40_matrix)

crs(average_over_35_raster) <- crs(temp_raster)
crs(average_over_40_raster) <- crs(temp_raster)

# Create a raster template based on temp_raster
raster_template <- raster(nrows = nrow(temp_raster), ncols = ncol(temp_raster), 
                          xmn = xmin(temp_raster), xmx = xmax(temp_raster), 
                          ymn = ymin(temp_raster), ymx = ymax(temp_raster), 
                          crs = crs(temp_raster))

# Convert the average matrices to rasters using the raster template
average_over_35_raster <- setValues(raster_template, average_over_35_matrix)
average_over_40_raster <- setValues(raster_template, average_over_40_matrix)

# Define a color palette using the viridis package
temp_colors <- viridis(10)

# Set the color break points
breaks <- seq(0, 10, 1)

# Plot the rasters with enhanced aesthetics
plot(average_over_35_raster, 
     col=temp_colors, 
     breaks=breaks, 
     main=sprintf("Average number of days over 35 C during 07-01 through 07-05 for %d to %d", start_year, end_year),
     legend.width=1.5, legend.shrink=0.75, legend.args=list(text=list(title="Average Number of Days"), cex=0.8))

plot(average_over_40_raster, 
     col=temp_colors, 
     breaks=breaks, 
     main=sprintf("Average number of days over 40 C during 07-01 through 07-05 for %d to %d", start_year, end_year),
     legend.width=1.5, legend.shrink=0.75, legend.args=list(text=list(title="Average Number of Days"), cex=0.8))

# (Optional) Save the average rasters to files
writeRaster(average_over_35_raster, "average_over_35.tif", format="GTiff", overwrite=TRUE)
writeRaster(average_over_40_raster, "average_over_40.tif", format="GTiff", overwrite=TRUE)
