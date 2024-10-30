#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#                                                                           #
#                     Rhine Valley Pesticides 2024                          #
#                   Ken Mauser, PhD Project - SystemLink                    #
#                                                                           #
#                 Build prediction maps                                     #
#                                                                           #
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#


# preparation -----------------------------------------------------------------

# load packages

library(readxl)
library(dplyr)
library(tidyr)
library(pls)

# set seed

set.seed(999)

# data import -----------------------------------------------------------------

data <- read_excel("Data/data_w_toponhisto.xlsx")

datasmol <- data[,c("sample_number", "v_num", "s_num", "diff")]



# Histo complete --------------------------------------------

# insert all the needed histograms (landuse) you decided for after the optimizer
# We computed them in several steps

points1 <- read_excel("Logs/20240109_013809_10963_predpoints1000m_rest.xlsx")
points2 <- read_excel("Logs/20240109_065430_10963_predpoints1000m_rest.xlsx")
points3 <- read_excel("Logs/20240109_222504_10963_predpoints1000m_rest.xlsx")

points4 <- read_excel("Logs/20231208_060742_10963_predpointssoil1000m_sum.xlsx")
points5 <- read_excel("Logs/20231208_033933_10963_predpointssoil1000m_sum.xlsx")
points6 <- read_excel("Logs/20231208_001433_10963_predpointssoil1000m_sum.xlsx")

points7 <- read_excel("Logs/20231207_043957_10963_predpointsveg1000m_sum.xlsx")
points8 <- read_excel("Logs/20231206_214227_10963_predpointsveg1000m_sum.xlsx")
points9 <- read_excel("Logs/20231206_212505_10963_predpointsveg1000m_sum.xlsx")

points10 <- read_excel("Logs/20231206_071118_10963_predpointssoil1000mb.xlsx")
points11 <- read_excel("Logs/20231206_024817_10963_predpointssoil1000mb.xlsx")
points12 <- read_excel("Logs/20231205_235449_10963_predpointssoil1000mb.xlsx")
points13 <- read_excel("Logs/20231202_060740_10963_predpoints1000m.xlsx")
points14 <- read_excel("Logs/20231126_002530_10963_predpoints1000mb.xlsx")
points15 <- read_excel("Logs/20231125_114209_10963_predpoints1000mb.xlsx")

points16 <- read_excel("Logs/20240111_181859_10963_predpoints1000m_restrestrest.xlsx")
points17 <- read_excel("Logs/20240115_100954_10963_predpoints1000m_missings.xlsx")

points18 <- read_excel("Logs/20240225_225721_10963_predpoints1000mrest2024.xlsx")
points19 <- read_excel("Logs/20240308_173817_10963_predpoints1000m_rest.xlsx")




points <- left_join(points1, points2[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))

points <- left_join(points, points3[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points4[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points5[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points6[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points7[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points8[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points9[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points10[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points11[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points12[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points13[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points14[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points15[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points16[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points17[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points18[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))
points <- left_join(points, points19[,-c(2:4)], by = "id", suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))

# change the names of the columns

current_colnames <- colnames(points)[-c(1:4)]

new_colnames <- gsub("landtype|\\.sigma", "", current_colnames)
new_colnames <- paste0(new_colnames, "s")

colnames(points) <- c(colnames(points)[c(1:4)], new_colnames)




# V_NUM ----------------------------------------------------------------
# veg model

m1 <- glm(v_num ~ grassland2750s +
            cereals250s +
            othercrops250s +
            vegetables1500s +
            grape2500s +
            fruit3750s +
            forest500s +
            urban3000s,
          
          data = data, family = "poisson")

summary(m1)


with(summary(m1), 1 - deviance/null.deviance)

# export

library(broom)
library(writexl)

#write_xlsx(tidy(m1), "pred_v_num_raw2.xlsx")

# prediction
pred1 <- predict(m1, newdata = points, type = "response")


summary(pred1) # check for mistakes

quantile(pred1, 0.01) # check for mistakes


exportnum <- points
exportnum$prediction <- as.vector(pred1)

exportnum$prediction <- ifelse(exportnum$prediction >= 0, # condition
                               exportnum$prediction,    # what if condition is TRUE
                               0)          # what if condition is FALSE

#### rasterize prediction for veg num -----------------------------------------

library(sf)
library(raster)
library(terra)
predshapenum <- st_as_sf(exportnum, coords = c("x", "y"))

shape <- read_sf(dsn = "Shapes", layer = "poi6")
raster_template = rast(ext(shape), resolution = 1000,
                       crs = st_crs(shape)$wkt)

rnum <- rasterize(predshapenum, raster_template, field="prediction", fun = sum, na.rm = T)

plot(rnum)

## export end ----

#terra::writeRaster(rnum, "Predmaps/v_num_1000poiss.tif", overwrite = T)


