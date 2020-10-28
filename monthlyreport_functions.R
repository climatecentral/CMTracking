#converting code to functions

#parsing data pulled from master sheet
reformat_googlesheet <- function(data) {
  #remove additional columns
  tracking.data <- data[,c(1:33)]
  #rename columns
  new.col.names <- c("name", "station", "date", "year", "month", "location", "user.type", 
                     "program.content", "source", "rtc", "graphic.used", "logged", "rt", "tw", "fb", "other", 
                     "online.article", "online.found", "mw", "radio", "radio.time", 
                     "tv", "tv.time", "tv.found", "iq.graphic", "audience.size", 
                     "publicity.value", "cch.connects", "cc.hits", "cm.hits", 
                     "cmn.hits", "spanish", "download")
  colnames(tracking.data) <- new.col.names
  #add hit ID column
  #create hit IDs
  #replace space between first and last names with '-'
  #tracking.data$name <- as.character(tracking.data$name)
  #fullnames <-as.character(tracking.data$name)
  #fullnames.split <- strsplit(fullnames, ' ')
  #firstnames <- sapply(fullnames.split, function(x) x[1])
  #lastnames <- sapply(fullnames.split, function(x) x[length(x)])
  #replace blank cells with 0
  #firstnames[is.na(firstnames)] = 0
  #lastnames[is.na(lastnames)] = 0
  #trimws(firstnames, which='both')
  #trimws(lastnames, which='both')
  #tracking.data$name <- paste(firstnames, '', lastnames)
  #add hit_ID column
  #tracking.data$date <- as.Date(tracking.data$date, format="%m/%d/%Y")
  #tracking.data$ID <- paste(tracking.data$name, "_", tracking.data$date)
  #add row IDs
  tracking.data$ID <- seq.int(nrow(tracking.data))
  #reorder columns in dataframe
  tracking.data <- tracking.data[,c("name", "station", "ID", "date", "year", "month", "location", "user.type", "program.content", "source", 
                                    "rtc", "graphic.used", "logged", "rt", "tw", "fb", "other", "online.article", "online.found", "mw", 
                                    "radio", "radio.time", "tv", "tv.time", "tv.found", "iq.graphic", "audience.size", "publicity.value", "cch.connects", 
                                    "cc.hits", "cm.hits", "cmn.hits", "spanish", "download")]
  return(tracking.data)
}

#rank releases by number of hits
rankReleases <- function(dat){
  tracking.data_releases <- dat[,c("name", "ID", "date", "year", "month", "location", "user.type",
                                   "program.content","rt", "tw", "fb", "other","online.article", "radio","tv")]
  #disaggregate 'program.content' column into one release per cell
  library("tidyr")
  tracking.data_releases.sep <- separate_rows(tracking.data_releases, program.content, sep = ",")
  #trim white space at the start and end of each release name
  tracking.data_releases.sep$program.content <- trimws(tracking.data_releases.sep$program.content, which="both")
  #count hits for each release
  library("dplyr")
  tracking.data_releases.count <- tracking.data_releases.sep %>%
    group_by(program.content) %>%
    summarise(rt=sum(rt), tw=sum(tw), fb=sum(fb), other=sum(other), 
              online.article=sum(online.article), radio=sum(radio), tv=sum(tv))
  #make a condensed format
  tracking.data_releases.summarized <- tracking.data_releases.count %>%
    as_tibble() %>%
    mutate(
      social=rt+tw+fb+other,
      stories=online.article+radio,
      tv=tv)
  #add 20% to TV
  tracking.data_releases.summarized$tv <- round((tracking.data_releases.summarized$tv*1.2), digits=0)
  tracking.data_releases.summarized <- tracking.data_releases.summarized[,c("program.content", "social", "stories", "tv")]
  #add total hits column
  tracking.data_releases.summarized$total <- tracking.data_releases.summarized$social + tracking.data_releases.summarized$stories + tracking.data_releases.summarized$tv
  #sort by most to least popular
  countedSortedReleases <- tracking.data_releases.summarized[order(tracking.data_releases.summarized$total, decreasing=TRUE),]
  return(countedSortedReleases)
}

#return data frame of most popular releases
releases.popularity <- function(dat){
  #subset data
  tracking.data_releases <- dat[,c("name", "ID", "date", "year", "month", "location", "user.type", "program.content")]
  #disaggregate 'program.content' column into one release per cell
  library("tidyr")
  tracking.data_releases.sep <- separate_rows(tracking.data_releases, program.content, sep = ",")
  #trim whitespace at the start and end of each release name
  tracking.data_releases.sep$program.content <- trimws(tracking.data_releases.sep$program.content, which="both")
  #count number of instances of each release
  library("dplyr")
  tracking.data_releases.tally <- tracking.data_releases.sep %>%
    group_by(program.content) %>%
    summarize(n())
  #remove count of NA cells 
  tracking.data_releases.tally<- tracking.data_releases.tally[-which(is.na(tracking.data_releases.tally$program.content)),]
  #rename columns
  names(tracking.data_releases.tally) <- c("Release", "Hits")
  #sort by most to least popular
  mostpopular.releases <- tracking.data_releases.tally[order(tracking.data_releases.tally$Hits, decreasing=TRUE),]
  #format table
  library(formattable)
  mostpopular.releases.formatted <- formattable(mostpopular.releases, align=c("l", "c"))
  return(mostpopular.releases.formatted)
}


#return dataframe of most popular releases
releases.popularitytop10 <- function(dat){
  #subset data
  tracking.data_releases <- dat[,c("name", "ID", "date", "year", "month", "location", "user.type", "program.content")]
  #disaggregate 'program.content' column into one release per cell
  library("tidyr")
  tracking.data_releases.sep <- separate_rows(tracking.data_releases, program.content, sep = ",")
  #trim whitespace at the start and end of each release name
  tracking.data_releases.sep$program.content <- trimws(tracking.data_releases.sep$program.content, which="both")
  #count number of instances of each release
  library("dplyr")
  tracking.data_releases.tally <- tracking.data_releases.sep %>%
    group_by(program.content) %>%
    summarize(n())
  #remove count of NA cells 
  tracking.data_releases.tally<- tracking.data_releases.tally[-which(is.na(tracking.data_releases.tally$program.content)),]
  #rename columns
  names(tracking.data_releases.tally) <- c("Release", "Hits")
  #sort by most to least popular
  mostpopular.releases <- tracking.data_releases.tally[order(tracking.data_releases.tally$Hits, decreasing=TRUE),]
  #limit to Top 10
  top10.releases <- mostpopular.releases[c(1:10),]
  #format table
  library(formattable)
  top10.releases.formatted <- formattable(top10.releases, align=c("l", "c"))
  return(top10.releases.formatted)
}

rankGraphics <- function(dat){
  tracking.data_Graphics <- dat[,c("name", "ID", "date", "year", "month", "location", "user.type",
                                   "graphic.used","rt", "tw", "fb", "other","online.article", "radio","tv")]
  #disaggregate 'graphic.used' column into one release per cell
  library("tidyr")
  tracking.data_Graphics.sep <- separate_rows(tracking.data_Graphics, graphic.used, sep = ",")
  #trim white space at the start and end of each release name
  tracking.data_Graphics.sep$graphic.used <- trimws(tracking.data_Graphics.sep$graphic.used, which="both")
  #count hits for each release
  library("dplyr")
  tracking.data_Graphics.count <- tracking.data_Graphics.sep %>%
    group_by(graphic.used) %>%
    summarise(rt=sum(rt), tw=sum(tw), fb=sum(fb), other=sum(other), 
              online.article=sum(online.article), radio=sum(radio), tv=sum(tv))
  #make a condensed format
  tracking.data_Graphics.summarized <- tracking.data_Graphics.count %>%
    as_tibble() %>%
    mutate(
      social=rt+tw+fb+other,
      stories=online.article+radio,
      tv=tv)
  #add 20% to TV
  tracking.data_Graphics.summarized$tv <- round((tracking.data_Graphics.summarized$tv*1.2), digits=0)
  tracking.data_Graphics.summarized <- tracking.data_Graphics.summarized[,c("graphic.used", "social", "stories", "tv")]
  #add total hits column
  tracking.data_Graphics.summarized$total <- tracking.data_Graphics.summarized$social + tracking.data_Graphics.summarized$stories + tracking.data_Graphics.summarized$tv
  #sort by most to least popular
  countedSortedGraphics <- tracking.data_Graphics.summarized[order(tracking.data_Graphics.summarized$total, decreasing=TRUE),]
  return(countedSortedGraphics)
}

#return dataframe of most popular graphics
graphics.popularity <- function(dat){
  #subset data
  tracking.data_graphics <- dat[,c("name", "ID", "date", "year", "month", "location", "user.type", "graphic.used")]
  #disaggregate 'graphic.used' column into one release per cell
  library("tidyr")
  tracking.data_graphics.sep <- separate_rows(tracking.data_graphics, graphic.used, sep = ",")
  #trim whitespace at the start and end of each release name
  tracking.data_graphics.sep$graphic.used <- trimws(tracking.data_graphics.sep$graphic.used, which="both")
  #count number of instances of each graphic
  library("dplyr")
  tracking.data_graphics.tally <- tracking.data_graphics.sep %>%
   group_by(graphic.used) %>%
    summarize(n())
  #remove count of blank and NA cells
  tracking.data_graphics.tally <-tracking.data_graphics.tally[-which(is.na(tracking.data_graphics.tally$graphic.used)),]
  #tracking.data_graphics.tally <- tracking.data_graphics.tally[-which(tracking.data_graphics.tally$graphic.used==""),]
  #rename columns
  names(tracking.data_graphics.tally) <- c("Graphic", "Hits")
  #sort by most to least popular
  mostpopular.graphics <- tracking.data_graphics.tally[order(tracking.data_graphics.tally$Hits, decreasing=TRUE),]
  #limit to Top 10
  library(formattable)
  top10.graphics <- mostpopular.graphics[c(1:10),]
  formattable(top10.graphics, align=c("l", "c"))
}

#compute hits by regions
hitsbyregion <- function(dat){
  regions.states <- read.csv("regionsandstates.csv")
  #subset relevant data
  hits.forregions <- dat[,c("ID", "location", 
                                      "rt", "tw", "fb", "other", 
                                      "online.article", "radio", 
                                      "tv")]
  #add regions to subsetted data
  hits.regions <- merge(hits.forregions, regions.states, by.x="location", by.y="state", all.x=FALSE)
  #reorder columns
  hits.regions <- hits.regions[,c("ID", "location", "region", "rt", "tw", "fb", 
                                  "other", "online.article", "radio", "tv")]
  hits.byregion <- subset(hits.regions, select=-c(ID, location))
  hits.byregion <- hits.byregion %>%
    group_by(region) %>%
    summarise(rt=sum(rt), tw=sum(tw), fb=sum(fb), other=sum(other), 
              online.article=sum(online.article), radio=sum(radio), tv=sum(tv))
  #make a condensed format
  library("dplyr")
  condensed.hits.byregion <- hits.byregion %>%
    as_tibble() %>%
    mutate(
      social=rt+tw+fb+other,
      stories=online.article+radio 
    )
  condensed.hits.byregion <- condensed.hits.byregion[,c("region", "social", "stories", "tv")]
  #add a total hits column
  condensed.hits.byregion <- mutate(condensed.hits.byregion, total=social+stories+tv)
  #sort region by total hits
  sorted.hitsbyregion <- condensed.hits.byregion[order(condensed.hits.byregion$total, decreasing = TRUE),]
  names(sorted.hitsbyregion) <- c("Region", "Social Media", "Stories", "TV", "Total")
  #format table
  library("formattable")
  sorted.hitsbyregion.formatted <- formattable(sorted.hitsbyregion, align=c("r", "c", "c", "c", "c"), list(''))
  return(sorted.hitsbyregion.formatted)
}

#output heatmap of TV hits (including AJ FOX)
tvheatmap.withAJFox <- function(dat){
  #subset data
  tv.data <- dat[,c("location", "program.content", "tv")]
  #aggregate tracking data to TV hits by state
  tvhits.bystate <- aggregate(tv.data$tv, by=list(states=tv.data$location), FUN=length)
  #eliminate non-states from "location" column
  data(state)
  tvhits.bystate <- tvhits.bystate[which(tvhits.bystate$states %in% state.abb),]
  names(tvhits.bystate) <- c("state", "hits")
  #plot data
  library("usmap")
  library("ggplot2")
  tvhits.plot.withAJFOX <- plot_usmap(data=tvhits.bystate, values="hits", color="black", labels = TRUE) + 
    scale_fill_continuous(name="TV Hits", label=scales::comma, high="dark blue", low="white", na.value="white") + 
    labs(title="TV hits by State") + 
    theme(legend.position="right")
  return(tvhits.plot.withAJFOX)
}


#output heatmap of TV hits (excluding AJ FOX)
tvheatmap.withoutAJFox <- function(dat){
  #subset data
  tv.data.woAJFOX <- tracking.data[,c("name", "location", "program.content", "tv")]
  tv.data.woAJFOX <- tv.data.woAJFOX[-which(tv.data.woAJFOX$name=="A.J. Fox"),]
  #aggregate tracking data to TV hits by state
  tvhits.bystate.woAJFOX <- aggregate(tv.data.woAJFOX$tv, by=list(states=tv.data.woAJFOX$location), FUN=sum)
  #eliminate non-states from "location" column
  tvhits.bystate.woAJFOX <- tvhits.bystate.woAJFOX[which(tvhits.bystate.woAJFOX$states %in% state.abb),]
  names(tvhits.bystate.woAJFOX) <- c("state", "hits")
  #plot hits by state
  library("usmap")
  library("ggplot2")
  library("dplyr")
  tvhits.plot.withoutAJFOX <- plot_usmap(data=tvhits.bystate.woAJFOX, values="hits", color="black", labels=TRUE) + 
    scale_fill_continuous(name="TV Hits", label=scales::comma, high="dark blue", low="white", na.value="white") + 
    labs(title="TV hits by State (excluding AJ Fox)") + 
    theme(legend.position="right")
  return(tvhits.plot.withoutAJFOX)
}

#compute hits by program
hitsbyprogram <- function(dat){
  hits.data <- dat[,c("ID", "source", 
                                "rt", "tw", "fb", "other", 
                                "online.article", "radio", 
                                "tv")]
  #separate into one source per row
  library("tidyr")
  hits.sepsource <- separate_rows(hits.data, source, sep = ",")
  #sum hits by source/program
  library("dplyr")
  hits.bysource <- hits.sepsource %>%
    group_by(source) %>%
    summarise(rt=sum(rt), tw=sum(tw), fb=sum(fb), other=sum(other), 
              online.article=sum(online.article), radio=sum(radio), tv=sum(tv))
  library("reshape2")
  hits.melt <- melt(hits.sepsource, id=c("ID","source"))
  recasted.hits <- dcast(hits.melt, ID+source~variable, sum)
  #sum hits by column
  recasted.hits.noID <- recasted.hits[,-1]
  summary.data <- recasted.hits.noID %>%
    group_by(source) %>%
    summarise(rt=sum(rt), tw=sum(tw), fb=sum(fb), other=sum(other), 
              online.article=sum(online.article), radio=sum(radio), tv=sum(tv))
  #add 20% to tv column of summary data
  summary.data.plus20pc <- summary.data
  summary.data.plus20pc$tv <- as.integer(summary.data$tv*1.2)
  #create tables of hits
  #expanded view
  expanded.hits.draft <- summary.data.plus20pc %>%
    as_tibble() %>%
    mutate(
      Twitter=rt+tw,
    )
  return(expanded.hits.draft)
}

#YTD TV chart
ytd.tv <- function(dat){
  #subset 2020 data
  data2020 <-dat[which(tracking.data$year=="2020"),]
  #replace NAs in tv column with 0s
  data2020$tv[is.na(data2020$tv)] <- 0
  #summarize tv by month
  library("dplyr")
  tvbymonth <- data2020 %>%
    group_by(month) %>%
    summarise(tv=sum(tv))
  #add 20% to tv
  tvbymonth$tv <- tvbymonth$tv*1.2
  #order data by month
  library("dplyr")
  tvbymonth %>% factor(month, levels=month.name)
  library("tidyverse")
  tvbymonth$month <- sort(tvbymonth$month)
  #add column for cumulative sum of tv airings
  tvbymonth$cumulative.tv <- cumsum(tvbymonth$tv)
  tvbymonth <- as.data.frame(tvbymonth)
  #tv goals
  tv.goals <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000)
  month <- month.name
  tvgoals.bymonth <- data.frame(month, tv.goals)
  #merge hits and goals
  tvgoalsandhits <- merge(tvbymonth, tvgoals.bymonth, by="month", all.y = TRUE)
  #remove tv column
  tvgoalsandhits <- subset(tvgoalsandhits, select=-tv)
  #reorder columns
  tvgoalsandhits <- tvgoalsandhits[,c(1,3,2)]
  #melt dataframe
  library("reshape2")
  melted.tvgoalsandhits <- melt(tvgoalsandhits)
  #make variable a factor
  melted.tvgoalsandhits$variable <- factor(melted.tvgoalsandhits$variable, levels = c("tv.goals", "cumulative.tv"))
  melted.tvgoalsandhits$variable <- sort(melted.tvgoalsandhits$variable)
  #plot chart
  library("ggplot2")
  tvgoalschart <- ggplot(melted.tvgoalsandhits, aes(month, value, group=variable)) + 
    theme(axis.text.x = element_text(angle=45, vjust=0.7)) +
    geom_line(aes(color=variable)) +
    labs(y="TV Airings", x="") +
    scale_fill_discrete(name="", labels=c("Goals", "Cumulative Airings"))
  return(tvgoalschart)
}

#compute hits by program
hitsbyprogram <- function(dat){
  hits.data <- dat[,c("ID", "source", 
                      "rt", "tw", "fb", "other", 
                      "online.article", "radio", 
                      "tv")]
  #separate into one source per row
  library("tidyr")
  hits.sepsource <- separate_rows(hits.data, source, sep = ",")
  #sum hits by source/program
  library("dplyr")
  hits.bysource <- hits.sepsource %>%
    group_by(source) %>%
    summarise(rt=sum(rt), tw=sum(tw), fb=sum(fb), other=sum(other), 
              online.article=sum(online.article), radio=sum(radio), tv=sum(tv))
  library("reshape2")
  hits.melt <- melt(hits.sepsource, id=c("ID","source"))
  recasted.hits <- dcast(hits.melt, ID+source~variable, sum)
  #sum hits by column
  recasted.hits.noID <- recasted.hits[,-1]
  summary.data <- recasted.hits.noID %>%
    group_by(source) %>%
    summarise(rt=sum(rt), tw=sum(tw), fb=sum(fb), other=sum(other), 
              online.article=sum(online.article), radio=sum(radio), tv=sum(tv))
  #add 20% to tv column of summary data
  summary.data.plus20pc <- summary.data
  summary.data.plus20pc$tv <- as.integer(summary.data$tv*1.2)
  #create tables of hits
  #expanded view
  expanded.hits.draft <- summary.data.plus20pc %>%
    as_tibble() %>%
    mutate(
      Twitter=rt+tw,
    )
  return(expanded.hits.draft)
}
