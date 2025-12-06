# Deployment script for blockr.dm demo to Posit Connect
#
# Prerequisites:
# 1. Have rsconnect configured with your Connect server credentials
# 2. Run this script from the deploy/ directory

library(rsconnect)

# Check if account is configured
accounts <- rsconnect::accounts()
if (nrow(accounts) == 0) {
  stop("No Connect account configured. Please configure rsconnect first.")
}

# Find cynkra connect account
cynkra_account <- accounts[accounts$server == "connect.cynkra.com", ]
if (nrow(cynkra_account) == 0) {
  cat("Available accounts:\n")
  print(accounts)
  stop("No connect.cynkra.com account found. Please configure rsconnect.")
}

cat("Deploying to account:", cynkra_account$name[1], "on", cynkra_account$server[1], "\n")

# Deploy the application
rsconnect::deployApp(
  appDir = ".",
  appName = "blockr-dm-adam",
  account = cynkra_account$name[1],
  server = cynkra_account$server[1],
  forceUpdate = TRUE
)
