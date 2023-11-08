paths_content <- readLines("File/file_paths.txt")
train_line <- grep("^train", paths_content)
train_value <- sub("^train=\\s*", "", paths_content[train_line])
train_Raw <- read.csv(train_value)

paths_content <- readLines("File/file_paths.txt")
test_line <- grep("^test", paths_content)
test_value <- sub("^test=\\s*", "", paths_content[test_line])
test_Raw <- read.csv(test_value)

paths_content <- readLines("File/file_paths.txt")
transactions_line <- grep("^transactions", paths_content)
transactions_value <- sub("^transactions=\\s*", "", paths_content[transactions_line])
transactions_Raw <- read.csv(transactions_value)

paths_content <- readLines("File/file_paths.txt")
oil_line <- grep("^oil", paths_content)
oil_value <- sub("^oil=\\s*", "", paths_content[oil_line])
oil_Raw <- read.csv(oil_value)

paths_content <- readLines("File/file_paths.txt")
holidays_events_line <- grep("^holidays_events", paths_content)
holidays_events_value <- sub("^holidays_events=\\s*", "", paths_content[holidays_events_line])
holidays_events_Raw <- read.csv(holidays_events_value)

paths_content <- readLines("File/file_paths.txt")
stores_line <- grep("^stores", paths_content)
stores_value <- sub("^stores=\\s*", "", paths_content[stores_line])
stores_Raw <- read.csv(stores_value)