library(Rfacebook)
library(ggplot2)

# for app Roauthtest:
auth_test <- fbOAuth(app_id = "899832843417285",
        app_secret = "151b44df592844be5c905b9d7a44c07f", 
        extended_permissions = FALSE)

save(auth_test, file = "fb_oauth")



# insights_test <- getInsights(
#   token = auth_test,
#   object_id = "104762689574183",
#   metric = "page_impressions",
#   period = "days_28")


# Alternative mit folgendem pkg
# http://user2015.math.aau.dk/presentations/210.pdf

### hier mit eigener Erweiterung von RFacebook

# Abfrage von Ad-Statistiken
temp <- getads(campaign = "6031785088708", 
               token = auth_test, 
               fields = c("ad_id",
                          "ad_name",
                          "adset_id",
                          "adset_name",
                          "impressions",
                          "cost_per_unique_click",
                          "ctr",
                          "frequency",
                          "reach",
                          "spend",
                          "unique_clicks",
                          "actions",
                          "cost_per_action_type"),
               breakdowns = c("age","gender"),
               limit = 100)



temp.df <- adDataToDF(temp$data)

temp.df.groups <- temp.df
temp.df.groups$adset_name[temp.df.groups$adset_name == "Gruppe B2"] <- "Gruppe B"

# Reichweite
aggregate(temp.df.groups$reach,by = list(temp.df.groups$adset_name), FUN = sum)

a <- ggplot(temp.df.groups)

a + geom_dotplot(aes(x = adset_name, y = sum(reach)))


# Clicks
click.agg <- aggregate(temp.df.groups$action.link_click,by = list(temp.df.groups$adset_name), FUN = sum)
colnames(click.agg) <- c("Gruppe","Klicks")
clickplot <- ggplot(click.agg)
clickplot + geom_bar(aes(x = Gruppe, y = Klicks), stat = "identity")

clickplot + geom_dotplot(binaxis = "y",method = "histodot", aes(x = Gruppe, y = Klicks))


aggregate(temp.df.groups$action.link_click, by = list(temp.df.groups$age), FUN = sum)
          