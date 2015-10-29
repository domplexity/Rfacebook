library(devtools)
install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
install_github("httr", "hadley", subdir = "httr")

library(Rfacebook)
library(ggplot2)

# for app Roauthtest:

auth_test <- fbOAuth(app_id = "XXX",
        app_secret = "XXX",
        extended_permissions = FALSE)

save(auth_test, file = "fb_oauth")

load(file = "fb_oauth")

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

#save as local json

save(temp, file = "json_export")

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

library(ggplot2)
library(RColorBrewer)


#look at the available palettes:
display.brewer.all()

diffferent.theme <- theme(axis.title=element_text(size=16,face="bold"),
                          axis.text=element_text(size=12, face="bold"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          legend.position="none")

# Reichweite
df.for.reach <- aggregate(temp.df.groups$reach,by = list(temp.df.groups$adset_name), FUN = sum)
colnames(df.for.reach) <- c("Gruppe","Reichweite")

plot.reach <- ggplot(df.for.reach)
plot.reach + geom_bar(aes(x = Gruppe, y = Reichweite, fill = Gruppe), stat="identity", width = .7) + 
  scale_fill_brewer(palette = "Oranges") + diffferent.theme
 



# Clicks
df.for.click <- aggregate(temp.df.groups$action.link_click, by = list(temp.df.groups$adset_name), FUN = sum, na.rm = TRUE)
colnames(df.for.click) <- c("Gruppe","Klicks")

plot.click <- ggplot(df.for.click)
  plot.click + geom_bar(aes(x = Gruppe, y = Klicks), stat = "identity", width = .7) + 
    scale_fill_brewer(palette = "Oranges") + diffferent.theme

# visual ctr

df.mixed <- merge(x = df.for.reach, y = df.for.click, by = "Gruppe")

plot.click.reach <- ggplot(df.mixed)
plot.click.reach + geom_point(aes(x=Reichweite, y = Klicks), size = 6) +
  geom_text(aes(x=Reichweite, y = Klicks,label=Gruppe),hjust=-0.2, vjust=0) +
  scale_fill_brewer(palette = "Oranges") + diffferent.theme

# dotsize
# label
# style


# bring cost into the equation

df.mixed <- merge(x = df.mixed, y = aggregate(temp.df.groups$spend, by = list(temp.df.groups$adset_name), FUN = sum, na.rm = TRUE), by.x = "Gruppe", by.y = "Group.1")
colnames(df.mixed)[4] <- "Ausgaben"
df.mixed$CPC <- df.mixed$Ausgaben/df.mixed$Klicks


plot.directly <- ggplot(data = temp.df.groups)
plot.directly + stat_summary(fun.y=sum,geom="bar",(aes(adset_name, reach))) + facet_grid(. ~ gender)

plot.directly + stat_summary(fun.y=sum,geom="point",(aes(x = reach, y = action.link_click))) + facet_grid(gender ~ adset_name)

plot.directly + stat_summary(fun.y=sum,geom="bar",(aes(x = adset_name, y = action.link_click))) + facet_grid(age ~ gender)
