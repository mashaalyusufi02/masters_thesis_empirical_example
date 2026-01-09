#setwd("c:/Users/masha/OneDrive/Documents/Honours Coppula Gaussian Graphical Model")
library(fredr)
library(tidyr)
library(mombf)
library(mvtnorm)
library(tibble)
library(tidyverse)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(pROC)
library(metRology)
library(MASS)      
library(huge)
library(forecast)
library(purrr)
library(igraph)
library(ggnetwork)
library(maps)
library(ggraph)
library(tseries)
library(igraph)




fredr_set_key("99ac315c26b771c9a0a29a89e1cdf1c1")
states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

state_series_ids <- sapply(states, function(state) {
  result <- fredr_series_search_text(
    search_text = paste(state, "unemployment rate"),
    filter_variable = "frequency",
    filter_value = "Monthly",
    order_by = "popularity",
    limit = 1
  )
  result$id[1]
})


#remove the state names and keep the ids only
state_ids_only <- unname(state_series_ids)

#these series ids were wronly identified by fred_series_search_text() method so I manually changed them to the correct series ids
state_ids_only[1] <-"ALUR"
state_ids_only[2] <- "AKUR"
state_ids_only[3] <- "AZUR"
state_ids_only[4] <- "ARUR"
state_ids_only[5] <- "CAUR"
state_ids_only[6] <- "COUR"
state_ids_only[7] <- "CTUR"
state_ids_only[8] <- "DEUR"
state_ids_only[9] <- "FLUR"
state_ids_only[10] <- "GAUR"
state_ids_only[11] <- "HIUR"
state_ids_only[12] <- "IDUR"
state_ids_only[13] <- "ILUR"
state_ids_only[14] <- "INUR"
state_ids_only[15] <- "IAUR"
state_ids_only[16] <- "KSUR"
state_ids_only[17] <- "KYUR"
state_ids_only[18] <- "LAUR"
state_ids_only[19] <- "MEUR"
state_ids_only[20] <- "MDUR"
state_ids_only[21] <- "MAUR"
state_ids_only[22] <- "MIUR"
state_ids_only[23] <- "MNUR"
state_ids_only[24] <- "MSUR"
state_ids_only[25] <- "MOUR"
state_ids_only[26] <- "MTUR"
state_ids_only[27] <- "NEUR"
state_ids_only[28] <- "NVUR"
state_ids_only[29] <- "NHUR"
state_ids_only[30] <- "NJUR"
state_ids_only[31] <- "NMUR"
state_ids_only[32] <- "NYUR"
state_ids_only[33] <- "NCUR"
state_ids_only[34] <- "NDUR"
state_ids_only[35] <- "OHUR"
state_ids_only[36] <- "OKUR"
state_ids_only[37] <- "ORUR"
state_ids_only[38] <- "PAUR"
state_ids_only[39] <- "RIUR"
state_ids_only[40] <- "SCUR"
state_ids_only[41] <- "SDUR"
state_ids_only[42] <- "TNUR"
state_ids_only[43] <- "TXUR"
state_ids_only[44] <- "UTUR"
state_ids_only[45] <- "VTUR"
state_ids_only[46] <- "VAUR"
state_ids_only[47] <- "WAURN"
state_ids_only[48] <- "WVUR"
state_ids_only[49] <- "WIUR"


print(state_ids_only)

#extract each time series from FRED for UNRATE using the ids of the time series
##for each state, there should be 541 monthly observations from Jan 1980 - Dec 2024
state_data <- lapply(state_ids_only, function(id) {
  fredr(
    series_id = id,
    observation_start = as.Date("1980-01-01"),
    observation_end = as.Date("2025-01-01"),
    frequency = "m"
  )
})

# Combine into one data frame

combined_data <- bind_rows(state_data, .id = "state_index")
combined_data <- combined_data %>% dplyr::mutate(state_index = as.integer(state_index))

state_names_df <- tibble(
  state_index = 1:length(states),
  state_name = states
)

combined_data <- combined_data %>%
  left_join(state_names_df, by = "state_index")





#check which states have fewer observations than 547
#state_with_fewer_observations <- which(sapply(state_data, nrow) < 547)
#all states have the same number of observations  i.e 547
#print(state_with_fewer_observations) 

#each row in combined_data represents an individual unemployment rate observation for each of the 50 time series.
combined_data <- data.frame(combined_data)
#Remove the last two columns. The remaining two columns gave date time of extraction of data from FRED

combined_data <- combined_data %>%rename(unem = value)

combined_data <- combined_data %>% dplyr::select(date,unem,state_name)

#count the number of rows with missing values in the unemployment rate column
print(nrow(combined_data[is.na(combined_data$unem),]))

#if the above command prints anything other than 0, print the rows with NA values
print(combined_data[is.na(combined_data$unem),])

#remove NA's by uncommenting the line below this one
#combined_data <- combined_data[!is.na(combined_data$unem),]


#demean and standardize the data 
combined_data <- combined_data %>% group_by(state_name) %>% mutate(
  demeaned_standardized_unem = (unem - mean(unem))/sd(unem)) %>% ungroup()


state_unem_summary <- combined_data %>%
  group_by(state_name) %>%
  summarise(
    mean_unem = mean(demeaned_standardized_unem, na.rm = TRUE),
    sd_unem = sd(demeaned_standardized_unem, na.rm = TRUE)
  )

print(state_unem_summary)





##print histograms of unemployment rates for the state of Alabama, Alaska, Iowa, and Massachusetts
##Notice how the distribution of the unemployment rate for these states is right skewed
for (i in c(1,2,15,21)) {
  
  state <- states[i]
  hist_data <- combined_data %>% 
    dplyr::filter(state_name == state) %>% 
    dplyr::pull(demeaned_standardized_unem)
  
  mean_val <- mean(hist_data, na.rm = TRUE)
  sd_val   <- sd(hist_data, na.rm = TRUE)
  skew_val <- DescTools::Skew(hist_data, na.rm = TRUE)
  kurt_val <- DescTools::Kurt(hist_data, na.rm = TRUE)
  
  # capture histogram object
  h <- hist(hist_data[1:436],
            prob = TRUE,
            breaks = 10, 
            main = state,
            xlab = "unemployment rate", 
            ylab = "Density",
            col = "skyblue", 
            border = "white")

}
##filter_out unemployment series from combined_data. Keep standardized unemployment
combined_data_prepared_for_model_fitting <- combined_data %>% dplyr::select(state_name,date,demeaned_standardized_unem)


####FIT AN AUTO.ARIMA Model TO THE ENTIRE DATA SET
combined_data_by_state_name <- combined_data_prepared_for_model_fitting %>% group_by(state_name) %>% nest()

#create a function to fit an ARIMA model to the standardized demeaned unemployment column
fit_best_arima <- function(df) {
  ts_data <- ts(df$demeaned_standardized_unem, frequency = 12, start = c(1980, 1))
  auto.arima(ts_data)
}

##fit the ARIMA model
combined_data_model_fitting_by_state_name <- combined_data_by_state_name %>%
  mutate(
    arima_model = purrr::map(.x = data, .f = ~fit_best_arima(.x)),
  )

##extract the residuals from the ARIMA model
combined_data_model_fitting_by_state_name<- combined_data_model_fitting_by_state_name %>%
  mutate(
    resids = purrr::map(arima_model,residuals))
  

###unlist the residuals of each state into a long format and store in tibble

resids <- combined_data_model_fitting_by_state_name %>%
   dplyr::select(state_name, resids) %>%
  unnest(resids)


##create standardized residuals with mean=0 and variance=1
resids <- resids %>%  group_by(state_name) %>%
  mutate(
  standardized_resids = (resids - mean(resids))/sd(resids)) %>% ungroup()


##store and present the summary statistics of the ARIMA residuals 
resids_state_unem_summary <- resids %>%
  group_by(state_name) %>%
  summarise(
    mean_unem = mean(standardized_resids, na.rm = FALSE),
    sd_unem = sd(standardized_resids, na.rm = FALSE)
  )

print(resids_state_unem_summary)


## Plot the histograms of 4 states


par(mfrow = c(2, 2), mar = c(4, 4, 4, 6)) 
par(bg = "white")

for (i in c(1,2,15,21)) {

  state <- states[i]
  resids_hist <- resids %>% dplyr::filter(state_name == state) %>% dplyr::pull(standardized_resids)

  mean_val <- mean(resids_hist, na.rm = TRUE)
  sd_val <- sd(resids_hist, na.rm = TRUE)
  
  hist(resids_hist[1:436],
       prob = TRUE,
       breaks = 10, 
       main = state,
       #xlim = c(-3,3),
       xlab = "ARIMA Residuals", ylab = "Density",
       col = "skyblue", border = "white")
  
}

  
#Plot the histograms the residuals of the same states
  state <- states[i]
  resids_hist <- resids %>% dplyr::filter(state_name == state) %>% dplyr::pull(standardized_resids)

  mean_val <- mean(resids_hist, na.rm = TRUE)
  sd_val <- sd(resids_hist, na.rm = TRUE)
  
  hist(resids_hist[1:436],
       prob = TRUE,
       breaks = 10, 
       main = state,
       xlab = "ARIMA Residuals", ylab = "Density",
       col = "skyblue", border = "white")
  
}


  




####GGM Estimation

#create a data vector Y with dimensions (t x p) where t=436, and p=50
Y <- resids %>% dplyr::select(state_name,standardized_resids) %>% pivot_wider(names_from = state_name, values_from=standardized_resids)
result <- matrix(NA, nrow = 541, ncol = ncol(Y))

# Loop through each column and unlist the list into the matrix
for (i in 1:ncol(Y)) {
  result[, i] <- unlist(Y[[i]])
}

# Convert to data frame and assign column names
Y_fixed <- as.data.frame(result)
colnames(Y_fixed) <- colnames(Y)
Y_fixed <- as.matrix(Y_fixed)

#split the data into the training set and the test set
training_set <- Y_fixed[1:436,]
test_set <- Y_fixed[437:nrow(Y_fixed),]

##fitting the gaussian graphical model by running an MCMC
p <- ncol(training_set)
iterations <- 10000
warmup <- 5000
s_slab <- 1
w_slab <- 0.2

posteriorSample <- modelSelectionGGM(
  training_set, scale = FALSE,
  niter = iterations + warmup,
  burnin = warmup,
  priorCoef = normalidprior(s_slab^2),
  priorModel = modelbinomprior(w_slab),
  priorDiag = exponentialprior(lambda = 1) )

###insert the marginal posterior probabilities into a matrix
graph_inclusion_probabilities <- matrix(NA, nrow = p, ncol = p,dimnames = list(states,states))
graph_inclusion_probabilities[upper.tri(graph_inclusion_probabilities, diag = TRUE)] <- posteriorSample$margpp
##coeficients of the posterior sample
estimated_parameters_GGM <- coef(posteriorSample)
#the line below gives the Precision Matrix of the Posterior Sample
esimated_precision_matrix_GGM <- icov(posteriorSample)


saveRDS(graph_inclusion_probabilities,"graph_inclusion_probabilities.rds")


Y_npn_unemployment <- huge.npn(training_set,npn.func = "truncation")

npn_posteriorSample <- modelSelectionGGM(
  training_set, scale = FALSE,
  niter = iterations + warmup,
  burnin = warmup,
  priorCoef = normalidprior(s_slab^2),
  priorModel = modelbinomprior(w_slab),
  priorDiag = exponentialprior(lambda = 1) )
  
npn_graph_inclusion_probabilities <- matrix(NA, nrow = p, ncol = p,dimnames = list(states,states))
npn_graph_inclusion_probabilities[upper.tri(npn_graph_inclusion_probabilities, diag = TRUE)] <- npn_posteriorSample$margpp
estimated_npn_precision_matrix <- icov(npn_posteriorSample)

saveRDS(npn_graph_inclusion_probabilities,"npn_graph_inclusion_probabilities.rds")



######GGM: compute log likelihood for test set
log_likelihood_ggm_npn <- function(Y_matr, Est_Prec_Mat) {
  n <- nrow(Y_matr)
  p <- ncol(Y_matr)
  quadratic_form <- sum(diag(Y_matr %*% Est_Prec_Mat %*% t(Y_matr)))
  log_det <- determinant(Est_Prec_Mat, logarithm = TRUE)$modulus[1]
  constant_terms <- - (n * p / 2) * log(2 * pi)
  ll <- (n / 2) * log_det - 0.5 * quadratic_form + constant_terms
  return(ll)
}

log_likelihood_ggm_npn(test_set,esimated_precision_matrix_GGM)

#log_likelihood_ggm_npn(test_set,estimated_npn_precision_matrix)

#####create plots of graphical models####
#create a US map
#plot network on top of US map
library(ggplot2)
library(maps)
library(dplyr)

# --- Setup ---
#change threshold to generate graph and edge count at that threshold
threshold <- 0.95

# Get state coordinates
states_coordinates <- data.frame(state.name, state.center)
states_coordinates <- states_coordinates %>% rename(state = state.name, x = x, y = y)

# Reposition Alaska and Hawaii
states_coordinates$x[states_coordinates$state == "Alaska"] <- -120
states_coordinates$y[states_coordinates$state == "Alaska"] <- 25
states_coordinates$x[states_coordinates$state == "Hawaii"] <- -100
states_coordinates$y[states_coordinates$state == "Hawaii"] <- 20

###Divide the states and count how many edges there are in each state 
###These results will be put in a table
northeast <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania")
midwest <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin","Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota")
south <- c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina","Virginia", "West Virginia", "Alabama", "Kentucky", "Mississippi", "Tennessee","Arkansas", "Louisiana", "Oklahoma", "Texas")
west <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming","Alaska", "California", "Hawaii", "Oregon", "Washington")



#  Build adjacency matrix 
adj_matrix_GGM <- matrix(NA, nrow = p, ncol = p, dimnames = list(states, states))
for (i in 1:p) {
  for (j in 1:p) {
    adj_matrix_GGM[i, j] <- ifelse(!is.na(graph_inclusion_probabilities[i, j]) &&
                                   graph_inclusion_probabilities[i, j] >= threshold, 1, 0)
  }
}

#  Convert edges of GM to data frame 
edge_list <- which(adj_matrix_GGM == 1, arr.ind = TRUE)
edges_df <- data.frame(
  x_start = states_coordinates$x[edge_list[, 1]],
  y_start = states_coordinates$y[edge_list[, 1]],
  x_end   = states_coordinates$x[edge_list[, 2]],
  y_end   = states_coordinates$y[edge_list[, 2]]
)




##create a subregion dataframe to merge with us_map so I can plot colour the map by 4 subregions
##subregions have values northeast, midwest, south, west
subregion_df <- data.frame(
  state = c(northeast, midwest, south, west),
  subregion = c(
    rep("Northeast", length(northeast)),
    rep("Midwest", length(midwest)),
    rep("South", length(south)),
    rep("West", length(west))
  ))
# --- Get map data ---
us_map <- map_data("state")
us_map <- us_map %>% filter(state != "District Of Columbia")

us_map <- map_data("state") %>%
  mutate(state = tools::toTitleCase(region)) %>%  # Convert lowercase to title case
  left_join(subregion_df, by = "state")

us_map <- us_map %>%
  # Combine the two into one — prefer subregion.y, fallback to .x if .y is NA:
  mutate(subregion = coalesce(subregion.y, subregion.x)) %>%
  # Drop the old columns to avoid confusion
  dplyr::select(-subregion.x, -subregion.y) %>%
  # Drop rows where subregion is missing
  filter(!is.na(subregion))


# plot the US map of states
ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat,group=group, fill=subregion),
               color = "white", alpha = 0.7) +
  geom_segment(data = edges_df,
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               color = "blue", linewidth = 0.2) +
  geom_point(data = states_coordinates, aes(x = x, y = y),
             color = "blue", size = 1.5) +
  geom_text(data = states_coordinates, aes(x = x, y = y, label = state),
            size = 2.0, color = "black", hjust = 0.5, vjust = -0.5) +
  coord_fixed(xlim = c(-130, -65), ylim = c(20, 50)) +
  ggtitle(paste("GGM Graph Threshold =", threshold)) + theme_void() +theme(plot.title = element_text(hjust = 0.5))



###count number of edges for northeast
###count number of edges for midwest
###count number of edges for south
###count number of edges for west

count_edges_within_region <- function(all_regions, adj_matrix) {
  count <- 0
  for (i in all_regions) {
    for (j in all_regions) {
      if (i < j && adj_matrix[i, j] == 1) {
        count <- count + 1
      }
    }
  }
  return(count)  # divide by 2 to avoid double-counting
}

count_edges_within_region(northeast,adj_matrix_GGM)
count_edges_within_region(midwest,adj_matrix_GGM)
count_edges_within_region(south,adj_matrix_GGM)
count_edges_within_region(west,adj_matrix_GGM)




###NPN matrix
threshold_NPN <- 0.5
adj_matrix_NPN <- matrix(NA, nrow = p, ncol = p, dimnames = list(states, states))

for (i in 1:p) {
  for (j in 1:p) {
    adj_matrix_NPN[i, j] <- ifelse(!is.na(npn_graph_inclusion_probabilities[i, j]) &&
                                   npn_graph_inclusion_probabilities[i, j] >= threshold_NPN, 1, 0)
  }
}
# --- Convert edges of NPN to data frame to map to coordinates for plotting ---
edge_list_NPN <- which(adj_matrix_NPN == 1, arr.ind = TRUE)
edges_df_NPN <- data.frame(
  x_start = states_coordinates$x[edge_list_NPN[, 1]],
  y_start = states_coordinates$y[edge_list_NPN[, 1]],
  x_end   = states_coordinates$x[edge_list_NPN[, 2]],
  y_end   = states_coordinates$y[edge_list_NPN[, 2]]
)

  
ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat,group=group, fill=subregion),
               color = "white", alpha = 0.7) +
  geom_segment(data = edges_df_NPN,
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               color = "red", linewidth = 0.2) +
  geom_point(data = states_coordinates, aes(x = x, y = y),
             color = "red", size = 1.5) +
  geom_text(data = states_coordinates, aes(x = x, y = y, label = state),
            size = 2.0, color = "black", hjust = 0.5, vjust = -0.5) +
  coord_fixed(xlim = c(-130, -65), ylim = c(20, 50)) +
  ggtitle(paste("NPN Graph Threshold =", threshold_NPN)) + theme_void() +theme(plot.title = element_text(hjust = 0.5))



count_edges_within_region(northeast,adj_matrix_NPN)
count_edges_within_region(midwest,adj_matrix_NPN)
count_edges_within_region(south,adj_matrix_NPN)
count_edges_within_region(west,adj_matrix_NPN)
 
#num edges in both graphs. This varies as threshold and threshold_NPN values are changed 
cat("Number of GGM edges:", nrow(edges_df), "\n")
cat("Number of NPN edges:", nrow(edges_df_NPN), "\n")
  




###notes on the semi-parametric copula
###explains how npn.func= "truncation" works
###calculates then Frobenius norm of the  n x p data matrix Y i.e squares each off diagonal element in the matrix and takes the sum of them all
###Frobenius norm: https://mathworld.wolfram.com/FrobeniusNorm.html

save.image(file="unem_II.RData")

