paths_content <- readLines("File/file_paths.txt")
train_line <- grep("^train", paths_content)
train_value <- sub("^train=\\s*", "", paths_content[train_line])
train_Raw <- read.csv(train_value)

paths_content <- readLines("File/file_paths.txt")
test_line <- grep("^test", paths_content)
test_value <- sub("^test=\\s*", "", paths_content[test_line])
test_Raw <- read.csv(test_value)
