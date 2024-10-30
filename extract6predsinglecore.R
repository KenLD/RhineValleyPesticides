# Preparation ------------------------------------------------------------------

library(raster)
library(sf)
library(dplyr)
library(tidyverse)
library(readxl)

landuse_types <- data.frame(landtypes = c(1, 2, 3, 4, 5, 6, 7, 8),
                            category = c("grassland",
                                         "cereals",
                                         "othercrops",
                                         "vegetables",
                                         "grape",
                                         "fruit",
                                         "forest",
                                         "urban"))


# Read the raster and shapefile
rast <- raster("Raster/CLC_CTM_compressed.tif") #raster data of land use
#plot(rast)

# specify needed landtypes and sigmas to split computation into several rounds
loopvalues <- data.frame(landtypes = c(6,8),
                         
                         sigma_puff = c(250, 3000))

#opt a
#loopvalues <- loopvalues[7,]

#opt b
#loopvalues <- loopvalues[c(12:13),]

#opt c
#loopvalues <- loopvalues[c(8:11,14:15),]


# Time calculation preparation
pixels <- data.frame(landtypes = landuse_types$landtypes,
                     pixels = c(29664179,
                                32759935,
                                5687153,
                                4799473,
                                4907345,
                                3543282,
                                115684144,
                                27469347))

loopvalues <- left_join(loopvalues, pixels, by = "landtypes")

loopvalues$addtime <- loopvalues$pixels/(sum(loopvalues$pixels)*nrow(loopvalues))

loopvalues$check <- loopvalues$pixels/loopvalues$addtime

loopvalues <- left_join(loopvalues, landuse_types, by = "landtypes")


# select points of interest
points <- st_read("Shapes/predpoints1000m.shp") #meine Pufferzonen


# optional: remove points which are too far from actual measurement locations

#dist <- dist[dist$distance < 16340,]


sample_loc <- points

sample_loc <- sample_loc %>%
  mutate(x = unlist(map(sample_loc$geometry,1)),
         y = unlist(map(sample_loc$geometry,2)))

sample_loc <- as.data.frame(sample_loc)

n_samples <- nrow(sample_loc)
print(paste(n_samples, "sample(s) running"))

imax <- (sum(loopvalues$pixels)*n_samples*nrow(loopvalues))



# Set loop values -------------------------------------------------------------


# there are currently 8 different land types
landtypes <- loopvalues$landtypes

# Track starting time of the loop
t.start <- Sys.time()
t.substart <- Sys.time()

ipixel <- 0
itotal <- 0
isubstart <- 0

library(profvis)
library(Rcpp)

# Declare the C++-functions (outsourced for more speed)

cppFunction('NumericVector calculateDistances(NumericVector x, NumericVector y, double x_loc, double y_loc) {
  return (x - x_loc)*(x - x_loc) + (y - y_loc)*(y - y_loc);
}')

cppFunction('NumericVector calculateWeights(NumericVector dist, double sigma) {
  return exp(-dist / (2 * sigma * sigma));
}')

# Loop -------------------------------------------------------------------------

for(comb in 1:nrow(loopvalues)){

  sigma <- loopvalues[comb,"sigma_puff"]
  
  landtypenumber <- loopvalues[comb,"landtypes"]
  landtype <- landuse_types[landuse_types$landtype == landtypenumber,"category"]
  
  print(paste("Starting loop for sigma", sigma, "and landtype", landtype))
  print(paste("This is combination", comb, "out of", nrow(loopvalues)))
  
  # Create Raster subset for Landtype
  
  rast <- raster("Raster/CLC_CTM_compressed.tif")
  
  rast[rast != landtypenumber] <- NA
  
  rastdf <- as.data.frame(rast, xy=TRUE)
  colnames(rastdf) <- c("x", "y", "layer")
  
  rastdf <- rastdf[is.finite(rastdf$layer),]

  print(paste("Raster prep done for sigma", sigma, "and landtype", landtype))
  
  
  for (i in 1:n_samples) {
    
    # For each sampling point, use the specific coordinates directly
    x_loc <- sample_loc[i, "x"]
    y_loc <- sample_loc[i, "y"]
    
    dist <- calculateDistances(rastdf$x, rastdf$y, x_loc, y_loc)
      
    # calc weights in c++
    weights <- calculateWeights(dist, sigma)
    
      # Should be multiplied by pixel size
      sample_loc[i, paste0("landtype", as.character(landtype), ",sigma", sigma)] <- 
        10 * rowSums(matrix(weights, nrow = 1)) / (2 * pi * sigma^2)
      
      ipixel <- ipixel + loopvalues[comb, "pixels"]
      itotal <- itotal + 1
      
      if (itotal %% 10 == 0) {
        t.subend <- Sys.time() #end time for 100 i
          
        t.submin <- difftime(t.subend, t.substart, units='mins') # difference
        t.subh <- difftime(t.subend, t.substart, units='hours') # difference
        t.substart <- Sys.time() #new time for 100 i
        idiff <- ipixel - isubstart
        isubstart <- ipixel
        
        print(paste(ipixel,"done out of", imax, "(",
                    round(ipixel/imax*100,1), "% )" ))
        
        print(paste("approx.", t.submin/idiff*(imax-ipixel),"minutes left"))
        print(paste("thats", t.subh/idiff*(imax-ipixel),"hours"))
        
      } else {
      }


  }
  
  }


# Export endresults ------------------------------------------------------------

file_name <- format(Sys.time(), "%Y%m%d_%H%M%S")  # e.g., 20230615_134523
file_name <- paste0("Logs/", file_name, "_", n_samples, "_predpoints1000m.xlsx")    # e.g., 20230615_134523_predpoints1000m.xlsx

write_xlsx(data.frame(sample_loc), file_name)

print(paste("Saved results"))

# Time calculation
t.end <- Sys.time()
elapsed_time <- t.end - t.start

# Print the elapsed time
print(elapsed_time)

# Initialize the vector to store the results
n <- n_samples

results <- data.frame(iterations = c(n, n * 10, n * 100, n * 1000, n * 5000),
                      minutes = numeric(5),
                      hours = numeric(5),
                      days = numeric(5))

multiplied <- results$iterations / n

for (i in 1:5) {
  # Calculate the time in minutes, hours, and days
  results$minutes[i] <- as.numeric(elapsed_time, units = "mins") * multiplied[i]
  results$hours[i] <- as.numeric(elapsed_time, units = "hours") * multiplied[i]
  results$days[i] <- as.numeric(elapsed_time, units = "days") * multiplied[i]
}

print(results)

file_name <- format(Sys.time(), "%Y%m%d_%H%M%S")  # e.g., 20230615_134523
file_name <- paste0("Logs/", file_name, "_time.xlsx")    # e.g., 20230615_134523_time.xlsx

write_xlsx(data.frame(results), file_name)
