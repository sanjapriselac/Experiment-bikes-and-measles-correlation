#source code 
library(ggplot2)

#Dataset 1
############################################################################################
dataset1 <- read.csv("", sep =';')  #PLEASE INCLUDE FILE PATH TO DATASET 1 IN /data/raw/

#for plotting raw data
raw_matrix <- as.matrix(dataset1)[,-1]

#transform first column into Date format
dataset1$Datum = as.Date(as.character(dataset1$Datum), "%Y%m%d")
summary(dataset1)


#Raw Data plot
#######
rdf1 <- data.frame(dataset1$Datum, rowSums(raw_matrix))
names(rdf1) <- c('Date', 'All_Rides')

p1 <- ggplot(rdf1, aes(x = Date))
p1 <- p1 + geom_line(aes(y = All_Rides, colour = "All Rides per Day"))
p1
######

############################################################
#transform data in week intervals

slice <- function(input, by=7){ 
  starts <- seq(1,length(input),by)
  tt <- lapply(starts, function(y) input[y:(y+(by-1))])
  lapply(tt, function(x) x[!is.na(x)])
}


week_dates <- dataset1$Datum[seq(1,length(dataset1$Datum),7)][-53]
data_week <- matrix(nrow=52,ncol=13)

for (i in 2:ncol(dataset1)) {
  newcol <- sapply(slice(dataset1[[i]]), sum)[-53]
  data_week[,i-1] <- newcol
}


#Data frame for week intervals
week_data <- data.frame(week_dates,data_week)
names(week_data) <- names(data)


###########################################################
#data for 4 weeks
month_dates <- week_dates[seq(1,length(week_dates),4)]
data_month <- matrix(nrow=13,ncol=13)

for (i in 2:ncol(dataset1)) {
  newcol <- sapply(slice(week_data[[i]],4), sum)
  data_month[,i-1] <- newcol
}

#Data frame for 4 weeks interval 
month_data <- data.frame(month_dates,data_month)
names(month_data) <- names(dataset1)

#Data frame for all month rides on all routes
data_frame1 <- data.frame(1:12,rowSums(data_month)[-12])
names(data_frame1) <- c('Months', 'Month_Rides')


#Dataset2
#############################################################################################
raw_dataset2 <- read.csv(file = "", header=TRUE) #PLEASE INCLUDE FILE PATH TO DATASET 2 IN /data/


dates <- c(20171015,	20171115,	20171215,	20180115,	20180215,	20180315,	20180415,	20180515,
           20180615,	20180715,	20180815,	20180915)
dates <- as.Date(as.character(dates), '%Y%m%d')


dataset2 <- as.matrix(t(raw_dataset2))
dataset2 <- unname(dataset2[-1,])
dataset2 <- apply(dataset2, c(1,2), as.integer)

data_frame2 <- data.frame(dates,dataset2[1:12,])
names(data_frame2) <- c('Dates',as.character(raw_dataset2[,1]))


#plot dataset2
######
p2 <- ggplot(data_frame2, aes(x = Dates))
p2 <- p2 + geom_line(aes(y = `EU/EEA`, colour = "`EU/EEA`"))
p2
#######


#############################################################################################
#correlation plot and output file

##Correlation Plot
########
p <- ggplot(data_frame1, aes(x = Months))
p <- p + geom_line(aes(y = Month_Rides, colour = "Bike Monthly Rides"))

p <- p + geom_line(aes(y = (data_frame2$`EU/EEA`)*600 -160, colour = "Measles infections in EU/EEA"))

# now adding the secondary axis
p <- p + scale_y_continuous(sec.axis = sec_axis(~./600 + 160, name = "EU/EEA"))
p <- p + scale_x_discrete(limits=1:12)

# modifying colours and theme options
p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Monthly Rides",
              x = "Month",
              colour = "Dataset")
p <- p + theme(legend.position = 'bottom')
p
########


#output file
output_df <- data.frame(as.integer(format(month_dates[-1], "%Y%m%d")),rowSums(data_month)[-12],as.integer(format(data_frame2$Dates, "%Y%m%d")),data_frame2$`EU/EEA`)
names(output_df) <- c('Dates_#bikes','#bikes','dates_#measlesinf', '#measlesinf')
str(output_df)

write.csv(output_df, file = "Produced_Data.csv")
