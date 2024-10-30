# Preparation ------------------------------------------------------------------

category <- c("grassland",
             "cereals",
             "othercrops",
             "vegetables",
             "grape",
             "fruit",
             "forest",
             "urban")

library(readxl)
data <- read_excel("Data/data_w_toponhisto.xlsx")

data$s_sum_log <- log(data$s_sum+1e-2) # log for sum concentrations
data$v_sum_log <- log(data$v_sum+1e-2)


# computes the R2 value for the given sigmas
model_test <- function(sigmas, res){
  if (any(sigmas<50)){
    return(0)
  }
  if (any(sigmas>5000)){
    return(0)
  }
  # create formula
  formula <- paste(category, sigmas, rep("s", length(sigmas)),
                     sep = "", collapse = " + ")
  if (substr(res, 3, 5) == "num"){
    model <- glm(paste(res," + ",formula, sep = ""), data = data,
               family = poisson)}
  else {
    model <- glm(paste(res," + ",formula, sep = ""), data = data,
                 family = gaussian)
  }
  return(with(summary(model), 1 - deviance/null.deviance))
}

# finds a better estimate for the given sigmas
# checks all neighbors where only one sigma changes
check_some_neighbors <- function(sigmas, res,
                                 depth = 1){
  # find current
  R_2 <- model_test(sigmas, res)
  R2 <- 1:8
  changes <- rep(0, 8)
  # change each variable
  for (i in 1:8){
    for (change in c(-50, 50)){
      sigmas_copy = sigmas
      sigmas_copy[i] <- sigmas_copy[i] + change
      R_2_test <- model_test(sigmas_copy, res = res)
      R2[i] <- R_2_test
      if (R_2_test>R_2) changes[i] <- change
    }
  }

  if (all(changes == 0))    return (sigmas)
  else return(check_some_neighbors(sigmas + changes, res, depth +1))
}

# finds a better estimate for the given sigmas
# checks all neighbors
check_all_neighbors <- function(sigmas, res){
  # find current
  R_2 <- model_test(sigmas, res)
  
  # check all neighbors
  changes <- expand.grid(rep(list(c(-50,0,50)),8))
  
  # create random order to check
  order <- sort(runif(nrow(changes)), index.return = T)$ix
  
  # check all possilbe neighbors
  for (i in order){
    sig <- sigmas + changes[i,]
    R_2_test <- model_test(sig, res = res)
    if (R_2_test>R_2) return(c(F, sig))
  }
  
  # all neighbors have lower R2
  return (c(T, sigmas))
}

optimizer <- function(sigmas, res){
  sub_optimal <- 1
  index <- 0
  while(sub_optimal != 0 ){
    index <- index +1
    sigmas <- check_some_neighbors(sigmas, res)
    #current <- check_all_neighbors(sigmas, res)
    #sub_optimal <- current[1]
    #sigmas <- current[2:9]
    sub_optimal <- 0
  }
  return (c(sigmas,model_test(sigmas, res)))
}


itera = 5000 #1000-5000 seems sufficient

for (variable in c("v_num", "v_sum_log", "s_num", "s_sum_log")){
  t_start <- Sys.time()
  df <- data.frame(matrix(NA, nrow = itera, ncol = length(category)))
  colnames(df) <- category
  df[,"R2"] <- NA
  for (i in 1:itera){
    df[i,1:8] <- 50*sample(1:100, 8)
    try(
      df[i,] <- optimizer(df[i,1:8], res = paste(variable, "~ "))
      )
    print(c(variable, i, difftime(Sys.time(), t_start, units='mins'), (difftime(Sys.time(), t_start, units='mins'))/i))
  }
  
  # give a unique identifier
  df$identifier <- colSums(2^(0:7)*t(df[,1:8]/50))
  
  # check which results are actual local optima?
  df$optimal <- NA
  identifiers <- c()
  for (i in 1:itera){
    print(i)
    # has this solution already been checked?
    if (df$identifier[i] %in% identifiers) next
    else identifiers <- append(identifiers, df$identifier[i])
    
    sig_new <- check_all_neighbors(df[i,category], res = paste(variable, "~ "))
    df[i, "optimal"] <- sig_new[1]
  }
  
  write.csv(df, paste("Optimal_regression", variable, Sys.Date(), ".csv", sep = "_"))
}

