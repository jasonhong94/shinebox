library(sodium)

# dataframe that holds usernames, passwords and other user data
user_base <- data.frame(
  user = c("admin"),
  password = sapply(c("admin"), sodium::password_store), 
  permissions = c("admin"),
  name = c("User One"),
  stringsAsFactors = FALSE,
  row.names = NULL
)

saveRDS(user_base, "user_base.rds")
