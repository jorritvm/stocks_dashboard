#######################################
#
# This setup script will generate the 
# files required for authentication
#
#######################################

library(here)
library(dotenv)
library(shinymanager)

# Load environment variables from the credentials.env file
# either there is an env file and we should read from that
#  (e.g. dev environment) or the env is set (e.g. docker)
fpfn_env = here("../scripts/credentials.env")
if (file.exists(fpfn_env)) {
  dotenv::load_dot_env(fpfn_env)
}
shiny_user <- Sys.getenv("shiny_user")
shiny_password <- Sys.getenv("shiny_password")
cred_db_password <- Sys.getenv("cred_db_password")


# init credentials data
credentials <- data.frame(
  user = shiny_user,
  password = shiny_password,
  admin = TRUE
)

# # use keyring package to set database encryption key
# key_set_with_value(service  = "R_stock_dashboard_shinymanager-key", 
#                    username = NULL,
#                    password = keyring_password)

# write the credentials to the db using the encryption key
create_db(
  credentials_data = credentials,
  sqlite_path = here("../auth/credentials.sqlite"), 
  passphrase = cred_db_password
)
