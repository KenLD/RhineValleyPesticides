library('corrr')
library(dplyr)
library(readxl)

df <- read.csv(file = "Finallogs/Optimal_regression_v_num_2024-02-24_.csv")


category = c("grassland",
             "cereals",
             "othercrops",
             "vegetables",
             "grape",
             "fruit",
             "forest",
             "urban")

catdf <- data.frame(category = category, sigma = 1:8)


data <- read_excel("Data/data_w_toponhisto.xlsx")

data$s_sum_log <- log(data$s_sum+1e-2)
data$v_sum_log <- log(data$v_sum+1e-2)



# take each optima only once
df2 <- df[order(df$identifier),]

df2$change <- append(1,diff(df2$identifier))
df2 <- df2[df2$change != 0,]
df2 <- df2[df2$optimal,]



# calc weight by p

library(dplyr)

library(broom)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = nrow(df2), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")


for(i in 1:nrow(df2)){
  
  for(cat in category){
    
    catdf[catdf$category == cat, "sigma"] <- as.numeric(df2[i, cat])
    
  }
  
  lm_parts1 <- paste0(catdf$category, catdf$sigma, "s")
  lm_parts2 <- data.frame(part = lm_parts1, predictor = catdf$category)
  
  formula <- paste0(lm_parts1, collapse = "+")
  
  # select variable of interest here and be aware of the family
  m <- glm(data = data, paste("v_num ~", formula, collapse = ""), family = poisson) 
  
  for (lm_part in lm_parts1){
    
    p_value <- tidy(m) %>%
      filter(term == lm_part) %>%
      pull(p.value)
    
    estimate <- tidy(m) %>%
      filter(term == lm_part) %>%
      pull(estimate)
    
    place <- lm_parts2[lm_parts2$part == lm_part, "predictor"]
    
    df2[i, paste0(place, "none")] <- 1
    
    df2[i, paste0(place, "p")] <- p_value
    
    df2[i, paste0(place, "Est")] <- estimate
    
  }
  
  setTxtProgressBar(pb, i)
  
}

close(pb)

df2 <- df2[,-1]

df3 <- df2
df4 <- df2
df5 <- df2

for(cat in category){
  df3[,cat] <- -log(df3[,paste0(cat, "p")])*1 #weight (optional)
}

for(cat in category){
  df4[,cat] <- abs(df4[,paste0(cat, "Est")])*1 #weight by estimate (optional)
}

for(cat in category){
  df5[,cat] <- df5[,paste0(cat, "none")]*1 #weight 1 each (optional)
}

## ggplot

# Melt den Dataframe, damit er ggplot2 besser verarbeitet
df2_long <- tidyr::gather(df2[,1:8])
df3_long <- tidyr::gather(df3[,1:8])
df4_long <- tidyr::gather(df4[,1:8])
df5_long <- tidyr::gather(df5[,1:8])

df_long <- cbind(df3_long, df2_long[,2], df4_long[,2], df5_long[,2])

colnames(df_long) <- c("key", "weighted optimas p", "sigma", "weighted optimas Est", "none weighted optimas")

df_graph <- df_long %>% group_by(key, sigma) %>% summarise(pointsp = sum(`weighted optimas p`),
                                                           pointsEst = sum(`weighted optimas Est`),
                                                           points_noweight = sum(`none weighted optimas`))


library(ggplot2)
library(ggthemes)
library(ggplot2)

ggplot(data = df_graph[df_graph$sigma >50 &
                         df_graph$sigma <5000,], aes(x = sigma, y = pointsp)) +
  geom_bar(stat = "identity") +
  labs(x = "Value", y = "sum weights by -log(p-value)") +
  facet_grid(key ~., scales = "free_y") + 
  xlab(label = "Sigma value" )+
  scale_x_continuous(breaks = seq(0, 5000, by = 250),
                     minor_breaks = seq(0, 5000, 50)) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor.x = element_line(color = "grey",
                                    size = 0.1,
                                    linetype = 1),
        panel.grid.major.x = element_line(color = "black",
                                          size = 0.1,
                                          linetype = 1),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())


ggplot(data = df_long[df_long$sigma >50 &
                        df_long$sigma <5000,], aes(x = sigma, y = `weighted optimas Est`)) +
  geom_point() +
  labs(x = "Value", y = "sum weights with abs(estimate)") +
  facet_grid(key ~., scales = "free_y") + 
  xlab(label = "Sigma value" )+
  scale_x_continuous(breaks = seq(0, 5000, by = 250),
                     minor_breaks = seq(0, 5000, 50)) +# Hier wird facet_grid verwendet
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor.x = element_line(color = "grey",
                                          size = 0.1,
                                          linetype = 1),
        panel.grid.major.x = element_line(color = "black",
                                          size = 0.1,
                                          linetype = 1),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())


# grassland #61ca8e
# cereals #dcd439
# other crops #ea8bdc
# fruit #ce5153
# vegetables #569be1
# grape #a560ef
# forest #096924
# urban #6d6d6d


new_df <- df_long %>%
  group_by(key) %>%
  slice(rep(1:n(), each = `weighted optimas Est`)) %>%
  ungroup()


p1 <- ggplot(data = new_df[new_df$sigma >50 &
                       new_df$sigma <5000,], aes(x = sigma, y = key, fill = key)) +
  #geom_area() +
  #facet_wrap(~key, nrow = length(unique(df_graph$key)),  strip.position = c("left"), scales = "free")+
  geom_density_ridges() +
  labs(x = "Value", y = "density of weighted optimas", subtitle="Vegetation", title = "Number of CUPs") +
  xlab(label = "Sigma value") +
  scale_x_continuous(breaks = seq(0, 5000, by = 500)) +
  scale_y_discrete(limits = c("urban","forest","grassland","othercrops", "fruit","grape","vegetables","cereals")) +
  scale_fill_manual(values = c("#61ca8e", "#dcd439", "#ea8bdc", "#ce5153", "#569be1", "#a560ef", "#096924", "#6d6d6d"),
                    breaks = c("grassland", "cereals", "othercrops", "fruit", "vegetables", "grape", "forest", "urban")) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor.x = element_line(color = "grey", size = 0.1, linetype = 1),
    panel.grid.major.x = element_line(color = "black", size = 0.1, linetype = 1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45 , hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black")
    )


























