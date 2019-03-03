#####################################
## PART 2
####################################
##############
# Subpart 4
##############
# Influenza National Summary 


if (getwd()=="/Users/JohnnyD")
{
  setwd("/Users/JohnnyD/Documents/CSE487/LAB1EDA/part2")
}

data <- read.csv("InfluenzaNationalSummaryFeb2.csv")

df <- data.frame(data)
sub_cols <- c("YEAR", "WEEK")
sub_df <- df[sub_cols]
sub_df <- transform(sub_df,YEARWEEK=paste0(YEAR,WEEK))
attach(sub_df)
df$YEARWEEK = YEARWEEK
df$YEAR = NULL
df$WEEK = NULL
df$TOTAL.SPECIMENS = NULL
df
attach(df)
df
df1 <- df[c("YEARWEEK","TOTAL.A")]
df1 <- cbind(df1, TOTAL.B)
library(reshape2)
df1 <- melt(df1)
df1
df2 <- df[c("YEARWEEK","TOTAL.B")]
df3 <- df[c("YEARWEEK","PERCENT.POSITIVE")]
df4 <- df[c("YEARWEEK","PERCENT.A")]
df5 <- df[c("YEARWEEK","PERCENT.B")]

library(ggplot2)
g1 <- ggplot() +
  geom_col(data=df1, aes(x=YEARWEEK, y=value, fill=variable),color="black") +
  scale_fill_manual(values=c("yellow","#228B22"),labels=c(" A"," B")) +
  geom_line(data=df3, aes(x=YEARWEEK, y=PERCENT.POSITIVE*8000/25, color="Percent Positive"),group=1) +
  geom_line(data=df4, aes(x=YEARWEEK, y=PERCENT.A*8000/25, color="% Positive Flu A"), linetype="longdash",group=1)+
  geom_line(data=df5, aes(x=YEARWEEK, y=PERCENT.B*8000/25, color="% Positive Flu B"), 
            linetype="longdash", group=1) +
  scale_color_manual(values=c("Percent Positive"="black","% Positive Flu A"="#999900",
                              "% Positive Flu B"="#32CD32")) +
  theme(legend.title=element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5))+
  ggtitle("Influenza Positive Tests Reported to CDC by U.S. Clinical Laboratories,\n 
          National Summary, 2018-2019 Season\n")+
  scale_y_continuous(name = expression("Number of Positive Specimens"), 
                     limits = c(0, 8000), sec.axis = sec_axis(~. *25/8000, 
                                                              name = "Percent Positive"),expand=c(0,0),breaks=seq(0,8000,1000)) +
  xlab("Week") +
  ylab("Number of Positive Specimens")
plot(g1)

# Influenza Positive Tests

if (getwd()=="/Users/JohnnyD")
{
  setwd("/Users/JohnnyD/Documents/CSE487/LAB1EDA/part2")
}

data <- read.csv("PositiveTestedFeb2.csv")
df <- data.frame(data)
sub_cols <- c("YEAR", "WEEK")
sub_df <- df[sub_cols]
sub_df <- transform(sub_df,YEARWEEK=paste0(YEAR,WEEK))
attach(sub_df)
df$YEARWEEK = YEARWEEK
df$YEAR = NULL
df$WEEK = NULL
df$TOTAL.SPECIMENS = NULL
df

df <- melt(df)
df

g2 <- ggplot() +
  geom_col(data=df, aes(x=YEARWEEK, y=value, fill=variable), color="black") +
  scale_fill_manual(values=c("#FFFF00","#F7990D","#E50000","#8A2BE2","#008000","#ADFF2F","#00FF00"),
                    labels=c(" A (subtyping not performed)"," A (H1N1)pdm09"," A (H3N2)"," H3N2v"," B (lineage not performed)"," B (Victoria Lineage)", " B (Yamagata Lineage"))+
  xlab("Week") +
  ylab("Number of Positive Specimens") +
  theme(legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.key=element_blank(),axis.text.x = element_text(angle = 90), plot.title = element_text(hjust=.5)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1800), breaks=seq(0,1800, 200)) +
  ggtitle("Influenza Positive Tests Reported to CDC by Public Health Laboratories,\n National Summary, 2018-2019 Season\n")
plot(g2)

# Flu Heat Map

if (getwd()=="/Users/JohnnyD")
{
  setwd("/Users/JohnnyD/Documents/CSE487/LAB1EDA/part2")
}

data <- read.csv("Flu_HeatMapFeb2.csv")

df <- data.frame(data)

keep <- c("STATENAME","ACTIVITY.LEVEL", "ACTIVITY.LEVEL.LABEL")
df <- df[keep]
df
library(stringr)
df$LEVEL <- str_remove(df$ACTIVITY.LEVEL, "Level ")
df$ACTIVITY.LEVEL <- NULL  
attach(df)
df

# has all 50 states plus DC
library(urbnmapr)
library(ggplot2)
sts <- states
sts <- fortify(sts)
#sts <- rbend(sts, )
# gonna print this to csv, because urbnmapr is not available in jupyter notebook, 
# and I need this for part 3

#write.csv(sts, "statesLatLong.csv")

# getting Puerto Rico data
library(raster)
pr <- getData("GADM", country="Puerto Rico", level=0)
pr_df <- fortify(pr)

attach(pr_df)

# removing unnecessary columns
keep <- c("long", "lat", "order", "group", "state_name")
sts <- sts[keep]

pr_df$state_name <- "Puerto Rico"
pr_df <- pr_df[keep]

# combining pr with other states
areas <- rbind(sts, pr_df)
areas <- fortify(areas)

# get NYC coordinates
library(sf)

nyc <- read_sf(dsn="Borough Boundaries", layer="geo_export_4fb1db71-cb5b-4de4-91e6-6a0b0816dead")
nyc <- fortify(nyc)
# need to rearrange lat long coordinates

group_nyc <- c()
grp_start <- 57
# getting the coordinates (terribly inefficient)
nyc_lat <- c()
nyc_long <- c()
for (i in nyc$geometry){
  for (p in i){
    for (j in p){
      for (g in j){
        if (g > 0) {
          nyc_lat <- c(nyc_lat, g)
          group_nyc <- c(group_nyc, grp_start)
        }
        else {
          nyc_long <- c(nyc_long, g)
        }
        
      }
    }
  }
  grp_start <- grp_start + 0.1
  cat("borough done\n")
}
# making amount of coordinates smaller
#nyc_lat <- nyc_lat[seq(1, length(nyc_lat), 1)]
#nyc_long <- nyc_long[seq(1, length(nyc_long), 1)]

# making nyc dataframe
nyc_df <- data.frame(nyc_lat)
nyc_df$lat <- as.factor(nyc_df$nyc_lat)
nyc_df$nyc_lat <- NULL
nyc_df$long <- as.factor(nyc_long)

nyc_df$state_name <- "New York City"

ord <- seq(max(areas$order)+1, max(areas$order)+length(nyc_df$state_name), 1)
nyc_df$order <- as.factor(ord)
nyc_df$group <- as.factor(group_nyc)
#nyc_df <- fortify(nyc_df)
#nyc_df <- as.factor(nyc_df)
# now combine with other states
areas <- rbind(areas, nyc_df)



# now need to combine original flu data with areas
df$state_name <- df$STATENAME
df$STATENAME <- NULL

library(dplyr)
areas_df <- areas %>% left_join(df, by = "state_name")
areas_df <- fortify(areas_df)
head(areas_df)

#areas_df1 <- areas_df
# don't need level label anymore
areas_df$ACTIVITY.LEVEL.LABEL <- NULL
head(areas_df)
areas_df1 <- fortify(areas_df)
write.csv(areas_df, file ="StateData.csv")
# after this line, we really don't need to do anything above anymore. just start here when you want to plot this graph

data <- read.csv("StateData.csv")
areas_df1 <- data.frame(data)

prevGroup <- areas_df1$group[1]
prevState <- areas_df1$state_name[1]
cur_num <- 1.00
groupp <- c(cur_num)

for (i in 2:length(areas_df1$group)){
  if (areas_df1$state_name[i] == prevState){
    if (areas_df1$group[i] == prevGroup){
      groupp <- c(groupp, cur_num)
    }
    else {
      cur_num <- cur_num + .01
      groupp <- c(groupp, cur_num)
      prevGroup <- areas_df1$group[i]
      
    }
  }
  else
  {
    if (ceiling(cur_num) == cur_num){
      cur_num <- ceiling(cur_num) + 1
      
    }
    else{
       cur_num <- ceiling(cur_num)
    }
    prevGroup <- areas_df1$group[i]
    groupp <- c(groupp, cur_num)
    prevState <- areas_df1$state_name[i]
  }
  print(i)
  cat(" ")
}
areas_df1$group2 <- groupp
cpy <- areas_df1
areas_df1 <- cpy
areas_df1$group <- NULL

areas_df1 <- fortify(data)
areas_df1$X <- NULL

# need to change coordinate systems of DC, Puerto Rico, and NYC
#first Puerto Rico
areas_df1$lat[areas_df1$state_name == "Puerto Rico"] <- areas_df1$lat[areas_df1$state_name == "Puerto Rico"]+9
mean_latpr <- mean(areas_df1$lat[areas_df1$state_name == "Puerto Rico"])
areas_df1$lat[areas_df1$state_name == "Puerto Rico"] <- areas_df1$lat[areas_df1$state_name == "Puerto Rico"] + 1.5*(areas_df1$lat[areas_df1$state_name == "Puerto Rico"] - mean_latpr)
areas_df1$long[areas_df1$state_name == "Puerto Rico"] <- areas_df1$long[areas_df1$state_name == "Puerto Rico"]-21
mean_longpr <- mean(areas_df1$long[areas_df1$state_name == "Puerto Rico"])
areas_df1$long[areas_df1$state_name == "Puerto Rico"] <- areas_df1$long[areas_df1$state_name == "Puerto Rico"] + 1.5*(areas_df1$long[areas_df1$state_name == "Puerto Rico"] - mean_longpr)
# now district of columbia
areas_df1$lat[areas_df1$state_name == "District of Columbia"] <- areas_df1$lat[areas_df1$state_name == "District of Columbia"]-3
mean_latdc <- mean(areas_df1$lat[areas_df1$state_name == "District of Columbia"])
areas_df1$lat[areas_df1$state_name == "District of Columbia"] <- areas_df1$lat[areas_df1$state_name == "District of Columbia"] + 10*(areas_df1$lat[areas_df1$state_name == "District of Columbia"] - mean_latdc)
areas_df1$long[areas_df1$state_name == "District of Columbia"] <- areas_df1$long[areas_df1$state_name == "District of Columbia"]+5
mean_longdc <- mean(areas_df1$long[areas_df1$state_name == "District of Columbia"])
areas_df1$long[areas_df1$state_name == "District of Columbia"] <- areas_df1$long[areas_df1$state_name == "District of Columbia"] + 10*(areas_df1$long[areas_df1$state_name == "District of Columbia"] - mean_longdc)
# now nyc
areas_df1$long[areas_df1$state_name == "New York City"] <- areas_df1$long[areas_df1$state_name == "New York City"]+7
mean_latnyc <- mean(areas_df1$lat[areas_df1$state_name == "New York City"])
areas_df1$lat[areas_df1$state_name == "New York City"] <- areas_df1$lat[areas_df1$state_name == "New York City"] + 9*(areas_df1$lat[areas_df1$state_name == "New York City"] - mean_latnyc)
mean_longnyc <- mean(areas_df1$long[areas_df1$state_name == "New York City"])
areas_df1$long[areas_df1$state_name == "New York City"] <- areas_df1$long[areas_df1$state_name == "New York City"] + 9*(areas_df1$long[areas_df1$state_name == "New York City"] - mean_longnyc)

# something weird is going on with the NYC's coordinates in this range, so I'm just gonna take them out
#cc <- seq(105750,105950, 1)
#areas2 <- areas_df1[-cc, ]

# get the proper color palette
#colors <- colorRampPalette(c("red","yellow","green"))
#colors <- colors(10)
#write.csv(areas_df1, file="StateData.csv")
# only need it from hear on down
areas_df1 <- read.csv("StateData.csv")
attach(areas_df1)
#setwd("/Users/JohnnyD/Documents/CSE487/Lab1EDA/part2")

colors <- c("10"="#CC0000","9"="#FF4500","8"="#FF8C00", "7"="#FFA500","6"="#FFD700", "5"="#E6FF00","4"="#ABFA33","3"="#97FA33","2"="#3EFC04","1"="#0BE404")
attach(areas_df1)

g <- ggplot(areas_df1, aes(long, lat, group = group2, fill=as.factor(LEVEL))) +
  geom_polygon(color = "gray", size = .05) +
  scale_fill_manual(values=colors) + 
  annotate("text", label = "Alaska", x =-122.794213 , y = 30.569676, size = 3, colour = "black")+
  annotate("text", label = "Hawaii", x =-108.794213 , y = 29.569676, size = 3, colour = "black")+
  annotate("text", label = "Puerto Rico", x =-88.794213 , y = 25.569676, size = 3, colour = "black")+
  annotate("text", label = "District of Columbia", x =-70.794213 , y = 34.569676, size = 3, colour = "black")+
  annotate("text", label = "New York City", x =-68.794213 , y = 38.569676, size = 3, colour = "black")+
  #,breaks=c("10","9", "8","7",  "6","5", "4",  "3", "2","1"),
  #  labels=c("","","","","","","","","",""))+
  coord_map(projection = "mercator") +
  ggtitle("2018-19 Influenza Season \n Week ending Feb 02, 2019")+
  theme(plot.title = element_text(face = "bold",hjust=.5,size=11), 
        legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        axis.title.y=element_blank(),axis.title.x=element_blank(),
        axis.line=element_blank()) 





# and here we do the extremely tedious task of putting lines and labels to the legend items
# heavily taken from stackoverflow
# create data to plot the legend
# x and y to create a vertical row of points
# all levels of the variable to be represented in the legend (here z)
library(data.table)
d <- data.table(x = 1, y = 1:10, z = factor(1:10))

# cut z into groups which should be displayed as text in legend
d[ , grp := cut(as.numeric(z), breaks = c(0, 2, 5, 7, 11),
                labels = c("   Minimal", "Low", "      Moderate", "High"))]

# calculate the start, end and mid points of each group
# used for vertical segments
d2 <- d[ , .(x = 1, y = min(y), yend = max(y), ymid = mean(y)), by = grp]

# end points of segments in long format, used for horizontal 'ticks' on the segments  
d3 <- data.table(x = 1, y = unlist(d2[ , .(y, yend)]))

# offset (trial and error)
v <- 0.5
# plot the 'legend'
p2 <- ggplot(mapping = aes(x = x-.63, y = y)) +
  geom_point(data = d, aes(color = z), size = 5) +
  geom_segment(data = d2,
               aes(x = x + v-.73, xend = x + v-.73, yend = yend)) +
  geom_segment(data = d3,
               aes(x = x + v-.73, xend = x + (v - 0.1)-.73, yend = y)) +
  geom_text(data = d2, aes(x = x + v - 0.23, y = ymid, label = grp)) +
  scale_color_manual(values = colors, guide = FALSE) +
  scale_x_continuous(limits = c(0, 4)) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.line=element_blank())+
  theme_void()

library(cowplot)

gg <- plot_grid(g,
          plot_grid(NULL, p2, NULL, nrow = 3, rel_heights = c(1, 1.5, 1)),
          rel_widths = c(2, 1)) +draw_label("ILI Activity Level", .65, 0.75, hjust = 0, vjust = 0,size=10)

ggsave("HeatMapInfluenza.png",gg,width=7,height=4)

# Influenza Mortality Rate

if (getwd()=="/Users/JohnnyD")
{
  setwd("/Users/JohnnyD/Documents/CSE487/LAB1EDA/part2")
}

data <- read.csv("MortalityJan26.csv")
df <- data.frame(data)
# getting only necessary data
keep <- c("WEEK","PERCENT.P.I","THRESHOLD","BASELINE","SEASON")
df <- df[keep]

attach(df)



# gonna convert SEASONS to an integer, then append Weeks to it
#df$SEASON <- gsub( "-", "", as.character(df$SEASON))
# for some reason there is a week 53 in the data, so I'm gonna get rid of that
df <- df[!(WEEK == 53),]

nums <- seq(1,length(df$SEASON),1)

df$order <- nums


library(ggplot2)
# gonna make our xticks here (probably a better way to do this but oh well)
xticks <- c()
for (i in WEEK)
{
  if (as.numeric(i) %% 10 == 0) {
    xticks <- c(xticks, i)
  }
  else
  {
    xticks <- c(xticks, "")
  }
}
# for some reason there was an extra element
xticks <- xticks[-length(xticks)]

yticks <- c()
for (i in seq(4,12,.2))
{
  if (i == 4 || i == 6 || i == 8 || i == 10 || i == 12) 
  {
    yticks <- c(yticks, i)
  }
  else
  {
    yticks <- c(yticks, "")
  }
}
ggplot() + 
  geom_line(data=df, aes(x=df$order, y=df$PERCENT.P.I), color="red")+
  stat_smooth(df,mapping=aes(x =df$order , y = df$BASELINE), method = "lm",
              formula = y ~ poly(x, 25), color="black")+
  stat_smooth(df,mapping=aes(x =df$order , y = df$THRESHOLD), method = "lm",
              formula = y ~ poly(x, 25), color="black")+
  xlab("MMWR Week") +
  scale_x_continuous(breaks=df$order, labels=xticks,expand=c(0,0))+
  scale_y_continuous(limits=c(4,12),breaks=seq(4,12,.2),labels=yticks,expand=c(0,0))+
  ylab("% of All Deaths Due to P&I")+
  labs(title="Pneumonia and Influenza Mortality from \n the National Center of Health Statistics Mortality
       Surveillance System\n",
       subtitle="Data through the week ending February 2, 2019")+
  geom_segment(aes(x=86-15, xend=86, y= 6.6-.8, yend= 6.6),
               arrow = arrow(length = unit(0.5, "cm")))+
  annotate(geom="text", x=86 - 15, y=6.6-1, label="Seasonal Baseline", color="black",size=3)+
  geom_segment(aes(x=53-15, xend=53, y=6.5+2.5, yend= 6.5), arrow = arrow(length = unit(0.5, "cm")))+
  annotate(geom="text", x=53-15, y=6.5+2.6, label="Epidemic Threshold", color="black", size=3)+
  annotate(geom="text", x=8, y=4.2, label="2014", size=2.5)+
  annotate(geom="text", x=43, y=4.2, label="2015", size=2.5)+
  annotate(geom="text", x=100, y=4.2, label="2016", size=2.5)+
  annotate(geom="text", x=155, y=4.2, label="2017", size=2.5)+
  annotate(geom="text", x=212, y=4.2, label="2018", size=2.5)+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.ticks.length = unit(.1, "cm"),
        axis.ticks.x=element_blank(),
        axis.text=element_text(size=8),
        axis.title=element_text(size=10),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# Pediatric Deaths
if (getwd()=="/Users/JohnnyD")
{
  setwd("/Users/JohnnyD/Documents/CSE487/LAB1EDA/part2")
}

data <- read.csv("Pediatric_DeathsFeb2.csv",skip=1)
df <- data.frame(data)
attach(df)
# need to get the sum of deaths for each year
total_deaths <- df$NO..OF.DEATHS
deaths1516 <- df$NO..OF.DEATHS[which(df$SEASON=="2015-16")]
deaths1617 <- df$NO..OF.DEATHS[which(df$SEASON=="2016-17")]
deaths1718 <- df$NO..OF.DEATHS[which(df$SEASON=="2017-18")]
deaths1819 <- df$NO..OF.DEATHS[which(df$SEASON=="2018-19")]
total1516 <- sum(deaths1516)
total1617 <- sum(deaths1617)
total1718 <- sum(deaths1718)
total1819 <- sum(deaths1819)
subtitle1516 <- "2015-2016\n Number of Deaths \n Reported = "
subtitle1516 <- paste(subtitle1516, toString(total1516))
subtitle1617 <- "2016-2017\n Number of Deaths \n Reported = "
subtitle1617 <- paste(subtitle1617, toString(total1617))
subtitle1718 <- "2017-2018\n Number of Deaths \n Reported = "
subtitle1718 <- paste(subtitle1718, toString(total1718))
subtitle1819 <- "2018-2019\n Number of Deaths \n Reported = "
subtitle1819 <- paste(subtitle1819, toString(total1819))


library(reshape2)
df <- melt(df, id.vars=c("WEEK.NUMBER","SEASON"),measure.vars=c("PREVIOUS.WEEK.DEATHS","CURRENT.WEEK.DEATHS"))
attach(df)
xlabels <- df$WEEK.NUMBER
xlabels <- as.character(xlabels)
seqnc <- c()
for (i in 1:length(WEEK.NUMBER))
{
  if (((i+5) %% 6) != 0)
  {
    seqnc <- c(seqnc, i)
  }
}
seqnc
xlabels <- replace(xlabels, seqnc, "")
xlabels
library(ggplot2)

ggplot() +
  geom_col(df, mapping=aes(x=WEEK.NUMBER, y=value, fill=variable), color="black",size=.3)+
  scale_y_continuous(expand=c(0,0),limits=c(0,25))+
  scale_x_discrete(breaks=WEEK.NUMBER,labels=xlabels)+
  xlab("Week of Death")+
  ylab("Number of Deaths")+
  ggtitle("Number of Influenza-Associated Pediatric Deaths\n 
          by Week of Death: 2015-2016 season to present")+
  scale_fill_manual(values=c("#308014","#00FFFF"),
                    labels=c(" Deaths Reported Previous Week"," Deaths Reported Current Week"))+
  annotate(geom="text", x=25, y=21, label=subtitle1516, color="black", size=2.7, fontface=2)+
  annotate(geom="text", x=75, y=21, label=subtitle1617, color="black", size=2.7, fontface=2)+
  annotate(geom="text", x=125, y=21, label=subtitle1718, color="black", size=2.7, fontface=2)+
  annotate(geom="text", x=165, y=21, label=subtitle1819, color="black", size=2.7, fontface=2)+
  theme(axis.line = element_line(size = .5, color="black"),
        axis.title.x = element_text(colour="black",face="bold"),
        axis.title.y = element_text(colour="black",face="bold"),
        legend.position="bottom",
        legend.box.background = element_rect(colour = "black",linetype="solid"),
        axis.text.x = element_text(angle = 90),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold"))

##############
# Subpart 5
##############

# Influenza National Summary Whole Year
if (getwd()=="/Users/JohnnyD")
{
  setwd("/Users/JohnnyD/Documents/CSE487/LAB1EDA/part2")
}

data <- read.csv("InfluenzaNationSummaryWholeYear.csv")

df <- data.frame(data)

sub_cols <- c("YEAR", "WEEK")
sub_df <- df[sub_cols]
sub_df <- transform(sub_df,YEARWEEK=paste0(YEAR,WEEK))
attach(sub_df)
df$YEARWEEK = YEARWEEK
df$YEAR = NULL
df$WEEK = NULL
df$TOTAL.SPECIMENS = NULL
df
attach(df)
df
df1 <- df[c("YEARWEEK","TOTAL.A")]
df1 <- cbind(df1, TOTAL.B)
library(reshape2)
df1 <- melt(df1)
df1
df2 <- df[c("YEARWEEK","TOTAL.B")]
df3 <- df[c("YEARWEEK","PERCENT.POSITIVE")]
df4 <- df[c("YEARWEEK","PERCENT.A")]
df5 <- df[c("YEARWEEK","PERCENT.B")]

# gotta do this so that the order of the x values will be in the correct order, and not alphabetized
# yes im sure there is a better way to do this, but it still got the job done
df1$YEARWEEK <- as.character(df1$YEARWEEK)
df1$YEARWEEK <- factor(df1$YEARWEEK, levels=unique(df1$YEARWEEK))
df2$YEARWEEK <- as.character(df2$YEARWEEK)
df2$YEARWEEK <- factor(df2$YEARWEEK, levels=unique(df2$YEARWEEK))
df3$YEARWEEK <- as.character(df3$YEARWEEK)
df3$YEARWEEK <- factor(df3$YEARWEEK, levels=unique(df3$YEARWEEK))
df4$YEARWEEK <- as.character(df4$YEARWEEK)
df4$YEARWEEK <- factor(df4$YEARWEEK, levels=unique(df4$YEARWEEK))
df5$YEARWEEK <- as.character(df5$YEARWEEK)
df5$YEARWEEK <- factor(df5$YEARWEEK, levels=unique(df5$YEARWEEK))

# doing this so it will be every other week on x axis label
xlabels <- df1$YEARWEEK
xlabels <- as.character(xlabels)
seqnc <- c()
for (i in 1:length(df1$YEARWEEK))
{
  if ((i %% 2) == 0)
  {
    seqnc <- c(seqnc, i)
  }
}
seqnc
xlabels <- replace(xlabels, seqnc, "")
xlabels
YEARWEEK

library(ggplot2)

g1 <- ggplot() +
  geom_col(data=df1, aes(x=YEARWEEK, y=value, fill=variable),color="black") +
  scale_fill_manual(values=c("yellow","#228B22"),labels=c(" A"," B")) +
  geom_line(data=df3, aes(x=YEARWEEK, y=PERCENT.POSITIVE*25000/28, color="Percent Positive"),group=1) +
  geom_line(data=df4, aes(x=YEARWEEK, y=PERCENT.A*25000/28, color="% Positive Flu A"), linetype="longdash",group=1)+
  geom_line(data=df5, aes(x=YEARWEEK, y=PERCENT.B*25000/28, color="% Positive Flu B"), 
            linetype="longdash", group=1) +
  scale_color_manual(values=c("Percent Positive"="black","% Positive Flu A"="#999900",
                              "% Positive Flu B"="#32CD32")) +
  theme(legend.title=element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5))+
  ggtitle("Influenza Positive Tests Reported to CDC by U.S. Clinical Laboratories,\n 
          National Summary, 2017-2018 Season\n")+
  scale_x_discrete(breaks=df1$YEARWEEK, labels=xlabels)+
  scale_y_continuous(name = expression("Number of Positive Specimens"), 
                     limits = c(0, 25000), sec.axis = sec_axis(~. *28/25000, 
                                                               name = "Percent Positive",breaks=seq(0,28,2)),expand=c(0,0),breaks=seq(0,25000,2000)) +
  xlab("Week") +
  ylab("Number of Positive Specimens")
plot(g1)


# Influenza Positive Tests Whole Year

if (getwd()=="/Users/JohnnyD")
{
  setwd("/Users/JohnnyD/Documents/CSE487/LAB1EDA/part2")
}
data <- read.csv("InfluenzaPositiveTestsWholeYear.csv")

df <- data.frame(data)

sub_cols <- c("YEAR", "WEEK")
sub_df <- df[sub_cols]
sub_df <- transform(sub_df,YEARWEEK=paste0(YEAR,WEEK))
YEARWEEK
attach(sub_df)
df$YEARWEEK = YEARWEEK
df$YEAR = NULL
df$WEEK = NULL
df$TOTAL.SPECIMENS = NULL
df

library(reshape2)
df <- melt(df, id.vars=c("YEARWEEK"))
df
attach(df)
df

# gotta do this so that the order of the x values will be in the correct order, and not alphabetized
df$YEARWEEK <- as.character(df$YEARWEEK)
df$YEARWEEK <- factor(df$YEARWEEK, levels=unique(df$YEARWEEK))

# doing this so it will be every other week on x axis label
xlabels <- df$YEARWEEK
xlabels <- as.character(xlabels)
seqnc <- c()
for (i in 1:length(YEARWEEK))
{
  if (((i+5) %% 3) != 0)
  {
    seqnc <- c(seqnc, i)
  }
}
seqnc
xlabels <- replace(xlabels, seqnc, "")
xlabels

g2 <- ggplot() +
  geom_col(data=df, aes(x=YEARWEEK, y=value, fill=variable), color="black") +
  scale_fill_manual(values=c("#FFFF00","#F7990D","#E50000","#8A2BE2","#008000","#ADFF2F","#00FF00"),
                    labels=c(" A (subtyping not performed)"," A (H1N1)pdm09"," A (H3N2)"," H3N2v",
                             " B (lineage not performed)"," B (Victoria Lineage)", " B (Yamagata Lineage"))+
  scale_x_discrete(breaks=YEARWEEK, labels=xlabels)+
  xlab("Week") +
  ylab("Number of Positive Specimens") +
  theme(legend.title=element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        axis.text.x = element_text(angle = 90), plot.title = element_text(hjust=.5,size=10))+
  scale_y_continuous(expand=c(0,0),limits=c(0,5000), breaks=seq(0,5000, 500)) +
  ggtitle("Influenza Positive Tests Reported to CDC by Public Health Laboratories,\n 
          National Summary, 2017-2018 Season\n")
plot(g2)

##############
# Subpart 6
##############

# New York Influenza Summary Whole Year

if (getwd()=="/Users/JohnnyD")
{
  setwd("/Users/JohnnyD/Documents/CSE487/LAB1EDA/part2")
}

data <- read.csv("InfluenzaNewYorkSummaryWholeYear.csv")

df <- data.frame(data)

sub_cols <- c("YEAR", "WEEK")
sub_df <- df[sub_cols]
sub_df <- transform(sub_df,YEARWEEK=paste0(YEAR,WEEK))
attach(sub_df)
df$YEARWEEK = YEARWEEK
df$YEAR = NULL
df$WEEK = NULL
df$TOTAL.SPECIMENS = NULL
df
attach(df)
df
df1 <- df[c("YEARWEEK","TOTAL.A")]
df1 <- cbind(df1, TOTAL.B)
library(reshape2)
df1 <- melt(df1)
df1
df2 <- df[c("YEARWEEK","TOTAL.B")]
df3 <- df[c("YEARWEEK","PERCENT.POSITIVE")]
df4 <- df[c("YEARWEEK","PERCENT.A")]
df5 <- df[c("YEARWEEK","PERCENT.B")]

# gotta do this so that the order of the x values will be in the correct order, and not alphabetized
# yes im sure there is a better way to do this, but it still got the job done
df1$YEARWEEK <- as.character(df1$YEARWEEK)
df1$YEARWEEK <- factor(df1$YEARWEEK, levels=unique(df1$YEARWEEK))
df2$YEARWEEK <- as.character(df2$YEARWEEK)
df2$YEARWEEK <- factor(df2$YEARWEEK, levels=unique(df2$YEARWEEK))
df3$YEARWEEK <- as.character(df3$YEARWEEK)
df3$YEARWEEK <- factor(df3$YEARWEEK, levels=unique(df3$YEARWEEK))
df4$YEARWEEK <- as.character(df4$YEARWEEK)
df4$YEARWEEK <- factor(df4$YEARWEEK, levels=unique(df4$YEARWEEK))
df5$YEARWEEK <- as.character(df5$YEARWEEK)
df5$YEARWEEK <- factor(df5$YEARWEEK, levels=unique(df5$YEARWEEK))

# doing this so it will be every other week on x axis label
xlabels <- df1$YEARWEEK
xlabels <- as.character(xlabels)
seqnc <- c()
for (i in 1:length(df1$YEARWEEK))
{
  if ((i %% 2) == 0)
  {
    seqnc <- c(seqnc, i)
  }
}
seqnc
xlabels <- replace(xlabels, seqnc, "")
xlabels
YEARWEEK

library(ggplot2)

g1 <- ggplot() +
  geom_col(data=df1, aes(x=YEARWEEK, y=value, fill=variable),color="black") +
  scale_fill_manual(values=c("yellow","#228B22"),labels=c(" A"," B")) +
  geom_line(data=df3, aes(x=YEARWEEK, y=PERCENT.POSITIVE*2000/35, color="Percent Positive"),group=1) +
  geom_line(data=df4, aes(x=YEARWEEK, y=PERCENT.A*2000/35, color="% Positive Flu A"), linetype="longdash",group=1)+
  geom_line(data=df5, aes(x=YEARWEEK, y=PERCENT.B*2000/28, color="% Positive Flu B"), 
            linetype="longdash", group=1) +
  scale_color_manual(values=c("Percent Positive"="black","% Positive Flu A"="#999900",
                              "% Positive Flu B"="#32CD32")) +
  theme(legend.title=element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(),
        axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5))+
  ggtitle("Influenza Positive Tests Reported to CDC by U.S. Clinical Laboratories,\n 
          New York, 2017-2018 Season\n")+
  scale_x_discrete(breaks=df1$YEARWEEK, labels=xlabels)+
  scale_y_continuous(name = expression("Number of Positive Specimens"), 
                     limits = c(0, 2600), sec.axis = sec_axis(~. *35/2600, 
                                                              name = "Percent Positive",breaks=seq(0,35,5)),expand=c(0,0),breaks=seq(0,2600,200)) +
  xlab("Week") +
  ylab("Number of Positive Specimens")
plot(g1)
