library(devtools)
install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
install_github("httr", "hadley", subdir = "httr")

https://github.com/hadley/httr
library(Rfacebook)
library(ggplot2)

# for app Roauthtest:
auth_test <- fbOAuth(app_id = "XXX",
        app_secret = "XXX", 
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

###################################################
# Alternative mit manuellem Export aus AdManager

temp2 <- read.csv(file = "manual_export.csv")

spalten.temp <- colnames(temp2)
spalten.temp[3] <- "ad_name"
spalten.temp[4] <- "age"
spalten.temp[5] <- "gender"
spalten.temp[8] <- "reach"
spalten.temp[10] <- "spend"
spalten.temp[12] <- "impressions"
spalten.temp[13] <- "ctr"
spalten.temp[14] <- "cost_per_unique_click"
spalten.temp[15] <- "action.link_click"
spalten.temp[16] <- "frequency"
spalten.temp[18] <- "adset_name"
spalten.temp[19] <- "adset_id"

colnames(temp2) <- spalten.temp

temp.df <- temp2
temp.df.groups <- temp2
temp.df.groups$adset_name[temp.df.groups$adset_name == "Gruppe B2"] <- "Gruppe B"

###################################################


# Reichweite
aggregate(temp.df.groups$reach,by = list(temp.df.groups$adset_name), FUN = sum)

df.for.reach <- aggregate(temp.df.groups$reach,by = list(temp.df.groups$adset_name), FUN = sum)
colnames(df.for.reach) <- c("Gruppe","Reichweite")
plot.reach <- ggplot(df.for.reach)
plot.reach + geom_bar(aes(x = Gruppe, y = Reichweite), stat="identity")



# Clicks
df.for.click <- aggregate(temp.df.groups$action.link_click, by = list(temp.df.groups$adset_name), FUN = sum, na.rm = TRUE)
colnames(df.for.click) <- c("Gruppe","Klicks")
plot.click <- ggplot(df.for.click)
plot.click + geom_bar(aes(x = Gruppe, y = Klicks), stat = "identity")

# visual ctr

df.mixed <- merge(x = df.for.reach, y = df.for.click, by = "Gruppe")

plot.click.reach <- ggplot(df.mixed)
plot.click.reach + geom_point(aes(x=Reichweite, y = Klicks))
# dotsize
# label
# style


# bring cost into the equation

df.mixed <- merge(x = df.mixed, y = aggregate(temp.df.groups$spend, by = list(temp.df.groups$adset_name), FUN = sum, na.rm = TRUE), by.x = "Gruppe", by.y = "Group.1")
colnames(df.mixed)[4] <- "Ausgaben"
df.mixed$CPC <- df.mixed$Ausgaben/df.mixed$Klicks

aggregate(temp.df.groups$action.link_click, by = list(temp.df.groups$age), FUN = sum)
          