check_primary_key <- function(table, column) 
  if(column %in% colnames(table)) {
    if (length(unique(table[[column]])) == nrow(table) && !any(is.na(table[[column]]))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return('Error: column not found')
  }


#clean column names and turn into lower case
colname_function <- function(name) {
  name <- gsub('[/, ]', '.', name)
  name <- tolower(name)
  return(name)
}



check_col1_in_col2 <- function(col1, col2){
  #lista <- c()
  for(i in 1:length(col1)){
    if(col1[i] %in% col2){
      check <- TRUE
    } else {
      check <- FALSE
      #lista <- c(lista,col1[i])
    }
  }
  result <- check
  #result <- c(check,lista)
  return(result)
}








check_dependencies<- function(table, key) {
  lista_num <- list()
  for (col in names(table)) {
    grouped_data <- split(table[[col]], table[[key]], drop = TRUE) 
    dependency_check <- sapply(grouped_data, function(group) length(unique(group)))
    
    if (all(dependency_check ==1 )) {
      lista_num[[col]] <- 1
    } else {
      lista_num[[col]] <- length(dependency_check) / sum(dependency_check)
    }
  }
  
  result <- lista_num
  return(result)
}


find_dependencies_matrix <- function(table) {
  num_cols <- ncol(table)
  dependency_matrix <- matrix(NA, nrow = num_cols, ncol = num_cols)
  colnames(dependency_matrix) <- colnames(table)
  rownames(dependency_matrix) <- colnames(table)
  
  for (col_2 in colnames(dependency_matrix)) {
    new_column <- check_dependencies(table, col_2) 
    new_column <- unlist(new_column)
    dependency_matrix[,col_2] <- new_column
  }
  return(dependency_matrix)
}


map_types_Loans <- function(table, table_types){
  table_types$type <- str_trim(tolower(table_types$type))
  
  table <- table %>% mutate(type = gsub("n\\..*", "", type))
  table$type <- str_trim(tolower(table$type))
  
  table_mapped <- table %>%
    left_join(table_types, by = "type")
  table_mapped <- table_mapped %>% select(-type)
  table_mapped <- table_mapped %>% relocate(Type, .before = status) %>% rename(type = Type)
  return(table_mapped)
}



divide_column_by_character <- function(dataframe, column_name, separator) {
  dataframe %>%
    mutate(across({{column_name}}, ~ ifelse(is.na(.), "NA", .))) %>%
    rowwise() %>%
    separate_rows({{column_name}}, sep = separator, convert = TRUE) %>%
    mutate_all(~str_trim(., 'left')) %>% mutate_all(~str_trim(., 'right'))
}





add_age_range_column <- function(data) {
  breaks <- c(0, 25, 50, 65, 75, Inf)
  labels <- c("0-25", "25-50", "50-65", "65-75", "75+")
  result <- data %>%
    mutate(
      range.age = cut(age, breaks = breaks, labels = labels, right = FALSE)
    )
  return(result)
}

create_region_city <- function(table, key1) {
  for (j in 1:length(key1)) {                 
    if (is.na(key1[j])) {
      table$region[j] <- NA  
    } else {
      matching_row <- subset(GeoData, city == toupper(key1[j]))
      if (nrow(matching_row) > 0) {
        table$region[j] <- matching_row$region
      } else {
        table$region[j] <- NA
      }
    }
  }
  return(table)
}

create_area_city <- function(table, key1) {
  for (j in 1:length(key1)) {                 
    if (is.na(key1[j])) {
      table$area[j] <- NA  
    } else {
      matching_row <- subset(GeoData, city == toupper(key1[j]))
      if (nrow(matching_row) > 0) {
        table$area[j] <- matching_row$area
      } else {
        table$area[j] <- NA
      }
    }
  }
  return(table)
}

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
        is.na(cf.piva) | cf.piva == "",
        "individual",
        ifelse(
          grepl("^[A-Za-z0-9]{16}$", cf.piva),  # 16 letters and numbers
          "individual",
          ifelse(
            grepl("^[0-9]{11}$", cf.piva),  # 11 numbers
            "corporate",
            "other"
          )
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
      type.subject == "corporate" ~ case_when(
        str_detect(name, "spa|s.p.a|s.p.a.|spas")  ~ "spa",
        str_detect(name, "srl|s.r.l|s.r.l.|srls")  ~ "srl",
        str_detect(name, "d.i|d.i.")  ~ "di",
        str_detect(name, " ss |s.s|s.s.|societa' semplice")  ~ "ss",
        str_detect(name, " sas |s.a.s|s.a.s.")  ~ "sas",
        str_detect(name, "snc|s.n.c|s.n.c.|sncs")  ~ "snc",
        str_detect(name, " sc |s.c|s.c.|scs|cons|consor|sportiva|associazionesportiva|cooperativa|coop")  ~ "sc",
        TRUE ~ "Other"  # Catch-all condition for "corporate"
      ),
      TRUE ~ NA_character_
    ))
  
  return(result)
}

fct.emp <- function(x) { 
  
  # base irpef (RAL - 9% contributi) 
  
  y <- x * 12 * (1 - 0.09)  
  
  # aliquote progressive 
  
  x.1 <- 15*10^3 
  
  x.2 <- 28*10^3 
  
  x.3 <- 50*10^3 
  
  p.1 <- 0.23 
  
  p.2 <- 0.25 
  
  p.3 <- 0.35 
  
  p.4 <- 0.43 
  
  # calcolo tasse 
  
  t.1 <- pmin(y, x.1) %>% pmax(0) * p.1 
  
  t.2 <- pmin(y - x.1, x.2 - x.1) %>% pmax(0) * p.2 
  
  t.3 <- pmin(y - x.2, x.3 - x.2) %>% pmax(0) * p.3 
  
  t.4 <- pmin(y - x.3) %>% pmax(0) * p.4 
  
  # importo netto 
  
  z <- (y - (t.1+t.2+t.3+t.4))/12 

  return(z) 
  
} 


checkcolumns <- function(col1, col2) {
  not_in_col2 <- col1[!(col1 %in% col2)]
  result <- list(match = all(col1 %in% col2), not_in_col2 = not_in_col2)
  return(result)
}