# # Code to create the credential files
# dir.create("shiny/admin/") # creates folder if it doesn't exist

dir.create("shiny_app/admin/")

credentials_df <- data.frame(
  user = c("user1"),
  password = c("password1"),
  stringsAsFactors = FALSE
)

saveRDS(credentials_df, "shiny_app/admin/credentials.rds")


