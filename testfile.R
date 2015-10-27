library(Rfacebook)

# for app Roauthtest:
auth_test <- fbOAuth(app_id = "899832843417285",
        app_secret = "XXX", 
        extended_permissions = FALSE)

insights_test <- getInsights(
  token = auth_test,
  object_id = "104762689574183",
  metric = "page_impressions",
  period = "days_28")




# Abfrage von Ad-Statistiken
temp <- getads("6031785088708", auth_test)

adDataToDF(temp)

# Alternative mit folgendem pkg
# http://user2015.math.aau.dk/presentations/210.pdf



