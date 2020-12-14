#' Suitable for ANOVA:
#' The function "blindDataAnova_ScrambleCellss" re-assigns at random the labels 
#' related to the specific combination of groups a case belongs to (e.g., 
#' expert: low, conflict: no, can get assigned to expert: high, conflict: yes). 
#' The number of permutations can be requested by user (e.g., if you want to 
#' have 6 datasets, input n_pertubations = 6; also defaults to 6). This function
#' works with unequal groups. The function returns a list with dataframes and a 
#' dataframe that contains all the permutations (labelled by permutation) for 
#' ease of use. The function also returns the number of possible permutations 
#' and the number of unique group combinations.
#' 
#' @param df_original # original dataframe
#' @param y # name of dependent variable
#' @param predictors # name of predictors, for example c("expert", "conflict)
#' @param n_permutations # number of requested permuted datasets
#' @keywords Scramble cells, ANOVA
#' @importFrom stats rnorm
#' @importFrom dplyr %>%  mutate select arrange 
#' @importFrom tibble rownames_to_column
#' @importFrom tidyselect all_of
#' @importFrom gtools permutations
#' 

blindDataAnova_ScrambleCells <- function(df_original, 
                                         y, 
                                         predictors, 
                                         n_permutations = 6){

  # Step 1: Create original dataframe with rowname and then Y first
  df_original = 
    df_original %>% 
    rownames_to_column() %>% 
    mutate(rowname = rowname %>% as.numeric) %>% 
    select(rowname, all_of(y), all_of(predictors))
  
  # Step 2: Calculate number of predictors 
  n_predictors = length(predictors)
  
  # Step 3: Calculate number of combinations by multiplying the factor levels
  # of all predictors
  n_combinations = 1 # Set number of combinations between predictors to 1
  
  # LOOP:
  # Calculate number of combinations by multiplying the factor levels of all 
  # predictors
  for(i in 1:n_predictors){
    # Mutliply the levels of var1 * var2 * var3 etc...
    n_combinations[1] = 
      n_combinations[1]*length(levels(df_original[,predictors[i]]))
  }
  
  # Step 4: Create a list with separate subsets based on each unique predictor
  # combination
  list_df_orig <- by(df_original[,], df_original[,predictors], subset)
  
  # Step 5: Create clone of list to pertube
  list_df_blinded = list_df_orig
  
  # Step 6: Create empty dataframe that holds all factor combinations of the 
  # original dataset
  df_factorlevels = data.frame(matrix(nrow = length(list_df_orig[[1]]), 
                                      ncol = length(predictors)))
  
  names(df_factorlevels) = predictors
  df_factorlevels
  
  # Step 7: Save all factors in the dataframe created by step 6
  # LOOP:
  # i: For each dataframe in list_df_blinded
  #    j: for each of the predictors
  #       -> extract the factor level used 
  for(i in 1:length(list_df_blinded)){
    for(j in 1:n_predictors){ 
      df_factorlevels[i,j] = 
        list_df_orig[[i]][,predictors[j]] %>% 
        as.character %>% unique
    }
  }
 
  # Step 8: Make the df_factorlevels into a factor (as it was) so it can get 
  # slotted into list_df_blinded
  df_factorlevels <- apply(df_factorlevels, 2, as.factor)
  
  # Step 9: Calculate pertubations
  df_permutations <- 
    gtools::permutations(n = n_combinations, 
                         # number of possible outcomes (e.g. n = 4, then 
                         # r = 1,2,3, of or 4, if  repeats.allowed = false
                         r = n_combinations, 
                         # input vector (here, numbers are combinations of 
                         # factor levels from dataset
                         v = 1:n_combinations,
                         repeats.allowed = F) 
  
  # Step 10: Select number of permutations as given by user (n_permutations), defaults to 6
  df_permutations = df_permutations[sample(1:nrow(df_permutations),
                                           n_permutations, 
                                           replace = F),]
  df_permutations
  
  # Step 11: Create empty list to store the lists in that contain the grouped_by dataframes
  list_list_df_blinded <- list()
  
  # Step 12: assign the corresponding combination of permutations from 
  #          df_permutations instead of the original value
  # LOOP:
  # p: For each requested permuated dataframe (n_permutations)
  #    i: For each dataframe in df_list_blinded
  #       j: for each predictor
  #          -> assign the corresponding combination of permutations from 
  #             df_permutations instead of the original value
  for(p in 1:n_permutations){
    
    for(i in 1:length(list_df_blinded)){
      
      for(j in 1:n_predictors){
        
        # For each permutation (e.g. 1234 vs 1243), take the combinations from 
        # the factorlevels that have been sampled from the original order
        # (e.g., 1 = low expert + no conflict)
        permutation_vector = df_permutations[p,]
        
        # Overwrite 
        list_df_blinded[[i]][,predictors[j]] <-
          df_factorlevels[permutation_vector, predictors[j]][i] 
        # [i] is the row in df_factorlevels, that corresponds to dataframe[[i]]
      }
    }
    
    # Save in the blinded list at position p
    list_list_df_blinded[[p]] <- list_df_blinded
  }
  
  # Step 13: Create empty dataframe as output from this function
  df_blindScambleCells <- 
    data.frame(matrix(nrow = 0,# nrow(df_original)*length(list_list_df_blinded),
                      ncol = ncol(df_original)))
  
  # Set correct names
  names(df_blindScambleCells) <- names(df_original)
  
  # Create empty list for dataframes as output from this function
  list_df_blindScrambleCells <- list()
  
  # Step 14: Fill the empty dataframe and list with the pertubed data
  # LOOP:
  # i: For each list (which is a grouped by dataframe) in this list
  #    j: for each group in that list (which is a dataframe)
  #       -> put a copy in a dataframe that is the output of ScrambleCells
  
  for(i in 1:length(list_list_df_blinded)){
    
    # Create temp dataframe in loop i to store dataframes for 
    # list_df_blindScrambleCells
    df_temp = data.frame(matrix(nrow = 0,
                                ncol = ncol(df_original)))
    
    
    for(j in 1:length(list_df_blinded)){
      
      # Save output in dataframe
      df_blindScambleCells = rbind(df_blindScambleCells, 
                                   list_list_df_blinded[[i]][[j]])
      
      # Save temp 
      df_temp = rbind(df_temp, 
                      list_list_df_blinded[[i]][[j]])
    }
    
    # Save output in list with dataframes
    list_df_blindScrambleCells[[i]] = 
      df_temp %>% 
      arrange(rowname) %>% 
      mutate(permutation = i) %>% 
      select(permutation, everything())
  }
  
  # Step 15: Create a column that indicates which permutated dataset the data 
  # is part of
  df_blindScambleCells$permutation <- 
    rep(1:n_permutations, each = nrow(df_original))
  
  # Reorder dataframe 
  df_blindScambleCells = 
    df_blindScambleCells %>% 
    select(permutation, everything()) %>%
    arrange(permutation, rowname)
  
  # Return
  return(list(df_blindScambleCells = df_blindScambleCells,
              list_df_blindScrambleCells = list_df_blindScrambleCells,
              df_original_factorlevels = df_factorlevels, 
              n_combinations_possible_from_factorlevels = n_combinations,
              n_permutations_possible = factorial(n_combinations),
              n_permutations_requested = n_permutations))
  
} # End blindDataAnova_ScrambleCells