# create AES encrypted sqlite db for user credentials

create_user_db <- function(initial_user, password, passphrase) {
  
  credentials <- tibble(user = c(initial_user),
                        password = c(password),
                        admin = c(TRUE),
                        write = c(TRUE))
  
  create_db(
    credentials_data = credentials,
    sqlite_path = "auth/users.sqlite",
    passphrase = passphrase
  ) 
}