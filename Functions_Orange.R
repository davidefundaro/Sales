###-----------------------------------------------------------------------###
#-----            Reading and writing excel files                       -----         
###-----------------------------------------------------------------------###

#Reads a file a returns a List with one dataframe for each excel sheet
read_doc_and_save_df <- function(file_path) {
  sheets <- excel_sheets(file_path)
  tibble_list <- lapply(sheets, function(x) read_excel(file_path, sheet = x))
  # Convert each sheet to a data frame
  DT <- lapply(tibble_list, as.data.frame)
  # Set names for the list elements based on sheet names
  names(DT) <- sheets
  return(DT)
}

###-----------------------------------------------------------------------###

#-----            Cleaning  Functions                                   -----         
###-----------------------------------------------------------------------###

#Delete first x columns and make the next one the colname
deleteXrowsAndRenameColumns <- function(x, df){
  colnames(df) <- df[x, ] 
  df <- df[-1:-x,]
  rownames(df) <- 1:nrow(df)
  df <- df[-1,]
  rownames(df) <- 1:nrow(df)
  return(df)
}
#Example:
#original.BorrowerData <- deleteXrowsAndRenameColumns(4, original.BorrowerData)

clean_column_names <- function(names) {
  # Use a regular expression to find positions between consecutive uppercase and lowercase letters to solve for example BorrowerName
  pattern <- "(?<=[a-z])(?=[A-Z])"
  names <- gsub(pattern, ".", names, perl = TRUE)
  
  # Make.names handles other invalid characters and ensures uniqueness
  names <- make.names(names, unique = TRUE)
  # Use gsub to replace consecutive dots with a single dot and remove trailing dots
  names <- gsub("\\.{2,}", ".", names)
  names <- gsub("\\.$", "", names)
  
  # Convert all characters to lowercase
  names <- tolower(names)
  
  return(names)
}
# Running example:
# colnames(df) <- clean_column_names(colnames(df))


# Function to split values, and create two rows when there's one with multiple values divided by a separator
divide_column_by_character <- function(dataframe, column_name, separator) {
  dataframe %>%
    mutate(across({{column_name}}, ~ ifelse(is.na(.), "NA", .))) %>%
    rowwise() %>%
    separate_rows({{column_name}}, sep = separator, convert = TRUE) %>%
    mutate_all(~str_trim(., 'left')) %>% mutate_all(~str_trim(., 'right'))
}
# Running example:
# NDG_N <- divide_column_by_character(NDG_N, "Judicial.Procedures.CODE", "\\+ ")

###-----------------------------------------------------------------------###

#-----            Normalization-Related Functions                       -----         
###-----------------------------------------------------------------------###


#Returns the possible primary keys of a given dataframe, in the form of a list with the column, its uniqueness and weather it has NAs or not
possiblePKs <- function(df){ 
  # Calculate the ratio of uniqueness as a numeric vector
  uniqueness_ratios <- apply(df, 2, function(x) {
    if (any(is.na(x))) {
      ((length(unique(x)) - sum(is.na(x))) / length(x)) * 100
    } else {
      length(unique(x)) / length(x) * 100
    }
  })
  # Combine column names and ratios into a data frame
  PK <- data.frame(Column = names(uniqueness_ratios), Ratio = sprintf("%.2f%%", uniqueness_ratios))
  # Add a new column indicating if there are NAs in the corresponding columns
  PK$Has_NA <- apply(df, 2, function(x) any(is.na(x)))
  #Only gets ratios over 80
  PK <- PK %>% filter(as.numeric(gsub("%", "", Ratio))>80)
  # Get the indices that sort the ratios in descending order
  sorted_indices <- order(as.numeric(gsub("%", "", PK$Ratio)), decreasing = TRUE)
  # Reorder PK based on the sorted indices
  PK <- PK[sorted_indices, ]
  return(PK)
}

#Compares one given column with the rest of the columns on the given dataframe and returns a one row df with the functional dependencies
functionalDependencies_Of_Column <- function(df, column_to_asses) {
  result <- df %>%
    group_by_at(vars(column_to_asses)) %>%
    summarize(across(everything(), ~n_distinct(.x), .names = "{.col}"), .groups = "drop") 
  result <- result %>% summarize(across(-1, ~ nrow(result)/sum(.))) %>% 
    mutate(!!column_to_asses := 1)
  return (result)
}
# Running example:
# result <- functionalDependencies_Of_Column(df, column_to_asses)


#Creates a matrix with the functional dependencies of the whole dataframe
functionalDependencies_Matrix <- function(df) {
  column_names <- names(df)
  # Create an empty matrix with rows and columns named after the columns of the dataframe
  fd_matrix <- matrix(0, nrow = length(column_names), ncol = length(column_names), dimnames = list(column_names, column_names))
  for (column in colnames(df)){
    results <- functionalDependencies_Of_Column(df, column)
    for (row in colnames(results) ){
      fd_matrix[row, column] <- as.numeric(results[row])
    }
  }
  #Creates a row with the sum of what we're certain are functional dependencies
  certain_functional_dependencies <- apply(fd_matrix, 2, function(x) {
    sum(x == 1)
  })
  fd_matrix <- rbind(certain_functional_dependencies, fd_matrix) %>% round(digits = 2)
}
# Running example:
#matrix_fd <- functionalDependencies_Matrix(LOANS)



###-----------------------------------------------------------------------###
#-----                Entity Table Functions                        -----         
###-----------------------------------------------------------------------###
clean_cf.piva <- function(column) {
  column <- gsub(" ", "", column)
  column <- ifelse(str_length(column) == 10, paste0(0, column), column)
  return(column)
}
# Running example:
#ENTITIES$cf.piva <- clean_cf.piva(ENTITIES$cf.piva)

add_age_column <- function(data) {
  result <- data %>%
    mutate(
      is_individual = nchar(cf.piva) == 16,
      age = case_when(
        is_individual ~ {
          year_of_birth <- as.numeric(stringr::str_sub(cf.piva, start = 7L, end = 8L))
          current_year <- as.numeric(format(Sys.Date(), "%Y"))
          ifelse(
            year_of_birth >= 0 & year_of_birth <= (current_year - 2018),
            current_year - (2000 + year_of_birth),
            current_year - (1900 + year_of_birth)
          )
        },
        TRUE ~ NA_real_
      )
    ) %>%
    select(-is_individual)
  
  return(result)
}
# Running example:
#ENTITIES <- add_age_column(ENTITIES)

# Define the age categories based on the age column
add_age_range_column <- function(data) {
  breaks <- c(0, 25, 50, 65, 75, Inf)
  labels <- c("0-25", "25-50", "50-65", "65-75", "75+")
  result <- data %>%
    mutate(
      range.age = cut(age, breaks = breaks, labels = labels, right = FALSE)
    )
  return(result)
}
# Running example:
#ENTITIES <- add_age_range_column(ENTITIES)


#Creates a type_subject_column based on the cf.piva column
add_type_subject_column <- function(data) {
  result <- data %>%
    mutate(
      type.subject = ifelse(
        is.na(cf.piva), 
        type.subject,  # Keep old value
        ifelse(
          grepl("^[A-Za-z]+$", cf.piva),
          "individual",
          "corporate"
          
        )
      )
    )
  
  return(result)
}
# Running example:
# ENTITIES <- add_type_subject_column(ENTITIES)

add_sex_column <- function(data) {
  result <- data %>%
    mutate(sex = case_when(
      !is.na(type.subject) & type.subject == "individual" & as.numeric(str_sub(cf.piva, start = 10L, end = 11L)) > 40 ~ "f",
      !is.na(type.subject) & type.subject == "individual" & as.numeric(str_sub(cf.piva, start = 10L, end = 11L)) <= 40 ~ "m",
      TRUE ~ NA_character_
    ))
  return(result)
}
# Running example:
# ENTITIES <-   add_sex_column (ENTITIES)

add_type.pg_column <- function(data) {
  result <- data %>%
    mutate(type.pg = case_when(
      str_detect(name, "srl|s.r.l|s.r.l.|srls")  ~ "srl",
      str_detect(name, "d.i|d.i.")  ~ "di",
      str_detect(name, " ss |s.s|s.s.|societa' semplice")  ~ "ss",
      str_detect(name, " sas |s.a.s|s.a.s.")  ~ "sas",
      str_detect(name, "snc|s.n.c|s.n.c.|sncs")  ~ "snc",
      str_detect(name, " sc |s.c|s.c.|scs")  ~ "sc",
      TRUE ~ NA_character_
    ))
} 