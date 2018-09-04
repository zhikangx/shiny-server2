library(shiny)
library(ggplot2)  # for the diamonds dataset
library(RCurl)
library("readr")
library(dplyr)
library(stringr)

if(!require("plyr")){
        install.packages("plyr")
        library("plyr")
}
if(!require("ggplot2")){
        install.packages("ggplot2")
        library("ggplot2")
}

if(!require("fitdistrplus")){
        install.packages("fitdistrplus")
        library("fitdistrplus")
}

if(!require("GGally")){
        install.packages("GGally")
        library("GGally")
}

if(!require("lubridate")){
        install.packages("lubridate")
        library("lubridate")
}

if (!require("scales")) {
        install.packages("scales")
        library(scales)
}

if (!require("grid")) {
        install.packages("grid")
        library(grid)
}

if (!require("RColorBrewer")) {
        install.packages("RColorBrewer")
        library(RColorBrewer)
}




# Read data

url <- 'http://172.16.4.242:8080/WebReport/ReportServer?reportlet=WorkBook4.cpt&op=write&format=csv'
data_activity <- read.csv(url, header=TRUE, stringsAsFactors=FALSE, fileEncoding="GB18030")

str(data_activity)
data_activity$region <- as.integer(data_activity$region)
data_activity$trade <- as.integer(data_activity$trade)
data_activity$wish <- as.integer(data_activity$wish)
data_activity$nums <- as.integer(data_activity$nums)
data_activity$aim <- as.integer(data_activity$aim)
data_activity$create_time <- as.POSIXct(as.character(data_activity$create_time), format = "%Y-%m-%d %H:%M:%S")
str(data_activity)

data <- data_activity[,c("id","name","tel","create_time","region","trade","wish","nums","aim","url")]
colnames(data)[5:9]<- c("region_id","industry_id","exp_date_id","size_id","purpose_id")
str(data)

# data cleaning

data_source <- data[,c("id","name","tel","create_time","url","purpose_id")]
head(data_source,10)
nrow(data_source)

## Remove data with empty source
data_valid_source <- data_source[data_source$url != "",]
data_valid_source <- data_valid_source[data_valid_source$url != "/",]
data_valid_source <- data_valid_source[data_valid_source$url != "NA",]
data_valid_source <- data_valid_source[data_valid_source$url != "na",]

nrow(data_valid_source)
str(data_valid_source)
head(data_valid_source$url,10)

## Remove data with duplicate url and phone number
data_valid_src_unique <- data_valid_source [!duplicated(data_valid_source[c("url","tel")]),]
nrow(data_valid_src_unique)
head(data_valid_src_unique,10)


## Remove data with unvalid phone number

data_valid_number <-  data_valid_src_unique
data_valid_number <- data_valid_number[-c(0:nrow(data_valid_number)),]
data_valid_number

nrow(data_valid_src_unique)
for (i in 1:nrow(data_valid_src_unique)) {
        # i <- 29
        # data_valid_src_unique$tel[i]
        tem_num <- as.numeric(as.character(data_valid_src_unique$tel[i]))
        tem_name <- as.character(data_valid_src_unique$name[i])
        # class(tem)
        # tem
        if (!is.na(tem_num)) {
                # Remove numbers less than eight numbers
                if (trunc(tem_num/10000000)<10) {
                        next
                }
                else if(str_detect(tem_name, "test")){
                        next
                }
                else if(str_detect(tem_name, "测")){
                        next
                }
                else{
                        data_valid_number <- rbind(data_valid_number,data_valid_src_unique[i,])
                }
        }
        # print(i)
}

str(data_valid_number)

# 
# for (i in 1:nrow(data_valid_src_unique)) {
#         # i <- 29
#         # data_valid_src_unique$tel[i]
#         tem <- as.numeric(as.character(data_valid_src_unique$tel[i]))
#         # class(tem)
#         # tem
#         if (!is.na(tem)) {
#                 if (trunc(tem/10000000000)==1) {
#                         data_valid_number <- rbind(data_valid_number,data_valid_src_unique[i,])
#                 }
#         }
#         # print(i)
# }

data_valid <- data_valid_number
str(data_valid)

##

## Create DB for seperating and saving url info

df <- data.frame(id=as.Date(character()),
                 create_time=character(), 
                 url=character(), 
                 url_1=character(), 
                 V1=character(),
                 V2=character(),           
                 V3=character(),           
                 V4=character(),           
                 V5=character(),           
                 V6=character(),           
                 stringsAsFactors=FALSE) 

for (i in 1:nrow(data_valid)) {
        file <- data_valid[i,]
        # file
        tem1 <- str_split_fixed(data_valid[i,"url"],"//", 2)[2]
        tem1 
        tem2 <- str_split_fixed(tem1,"/", 6)
        # str(merge(t,tem2))
        tem3 <- merge(file,tem2)
        df <- rbind(df,tem3)
}

head(df)
str(df)
colnames(df)[7:12] <- c("url_1","url_2","url_3","url_4","url_5","url_6")

df$source_type <- "unknown"

for (i in 1:nrow(df) ){
        t <- df$url_2[i]
        if (t=="#") {
                df$source_type[i] <- "direct" 
        }
        else if (as.character(strtrim(t, 4) == "?utm")) {
                df$source_type[i] <- "promotion"
        }
}

df$promotion_detail <- NA
df$promotion_id <- NA
head(df)

for (i in 1:nrow(df) ) {
        # i<- 17
        if (df$source_type[i]=="promotion") {
                tem1 <- str_split_fixed(df[i,"url_2"],"=", 2)[2]
                df$promotion_detail[i] <- as.character(tem1)
                tem2 <- str_split_fixed(df[i,"url_2"],"=", 5)[5]
                df$promotion_id[i] <- as.character(tem2)
        }
}

### Define Source
df$promotion_name <- NA
### Check Available source name, which are needed to be defined


str(df)
for (i in 1:nrow(df) ) {
        if (df$source_type[i]=="promotion") {
                if (df$promotion_detail[i]=="test1#") {
                        df$promotion_name[i] <- "Google test 1"
                }
                else if (df$promotion_id[i]=="woV3v592#" ){
                        df$promotion_name[i] <- "Guanfang Weibo"}
                else if (df$promotion_id[i]=="QReX7N9q#" ){
                        df$promotion_name[i] <- "Miyu Guanwei"}   
                else if (df$promotion_id[i]=="q9AYDE9W#" ){
                        df$promotion_name[i] <- "Miyu Guanwei"}   
                else if (df$promotion_id[i]=="noqLeAPk#" ){
                        df$promotion_name[i] <- "Tangxiansheng Mianguan"}
                else if (df$promotion_id[i]=="nP2qLZRm#" ){
                        df$promotion_name[i] <- "Wechat LBS Advertisement"}
                else if (df$promotion_id[i]=="5Rpn2yoN#" ){
                        df$promotion_name[i] <- "Niurendao"}
                else if (df$promotion_id[i]=="1R3GBrRg#" ){
                        df$promotion_name[i] <- "Wechat Fuli"}
                else if (df$promotion_id[i]=="Yo1a4vol#" ){
                        df$promotion_name[i] <- "51Gongxiangbangong"}
                else if (df$promotion_id[i]=="korvazo1#" ){
                        df$promotion_name[i] <- "Wechat xiaochengxu"}
                else if (df$promotion_id[i]=="EoZ2yQok#" ){
                        df$promotion_name[i] <- "pinpai wanliu"}
                else if (df$promotion_id[i]=="noqvAK9k#" ){
                        df$promotion_name[i] <- "yuzu"}
                else if (df$promotion_id[i]=="a9BGdVRn#" ){
                        df$promotion_name[i] <- "zheli weidao"}
                else if (df$promotion_id[i]=="woVxvYR2#" ){
                        df$promotion_name[i] <- "changqi xianxia shangye cuxiao"}
                else if (df$promotion_id[i]=="noqLeAPk#" ){
                        df$promotion_name[i] <- "jili jizhi"}
                else if (df$promotion_id[i]=="QPDjgB9N#" ){
                        df$promotion_name[i] <- "huodong yixing lipai"}
                else if (df$promotion_id[i]=="QReX5L9q#" ){
                        df$promotion_name[i] <- "pinpaishouce"}
                else if (df$promotion_id[i]=="nRbMWG93#" ){
                        df$promotion_name[i] <- "changqi youhuiquan"}                
                else if (df$promotion_id[i]=="a9axXAoB#" ){
                        df$promotion_name[i] <- "waibucanyuhuodong"}
                else if (df$promotion_id[i]=="DPWWyvPn#" ){
                        df$promotion_name[i] <- "laituibaxiezoulou ditui"}
                else if (df$promotion_id[i]=="korQmZP1#" ){
                        df$promotion_name[i] <- "zhuban chengban huodong"}
                else if (df$promotion_id[i]=="z98ekdRa#" ){
                        df$promotion_name[i] <- "changdi decor"}
                else if (df$promotion_detail[i]=="test3#"){
                        df$promotion_name[i] <- "ditie xianxia"}
                else if (df$promotion_detail[i]=="test4#"){
                        df$promotion_name[i] <- "baidu pingpai qipaoxian"}
                else if (df$promotion_detail[i]=="test5#"){
                        df$promotion_name[i] <- "Baidu Guanjianci Lianhebangong old"}
                else if (df$promotion_detail[i]=="test6#"){
                        df$promotion_name[i] <- "Baidu Guanjianci Gongxiangbangong old"}
                else if (df$promotion_detail[i]=="尝鲜季&utm_medium=官微自流量&utm_source=米域官微&gio_link_id=q9AYDE9W&from=singlemessage&isappinstalled=0#"){
                        df$promotion_name[i] <- "Miyu Guanwei"}
                else if (df$promotion_detail[i]=="baidu&utm_medium=baidu&utm_campaign=lianhebangong&utm_content=lianhebangong&utm_term=lianhebangonggongxiangbangong#"){
                        df$promotion_name[i] <- "Baidu Guanjianci Lianhebangong"}
                else if (df$promotion_detail[i]=="baidu&utm_medium=baidu&utm_campaign=gongxiangbangong&utm_content=gongxiangbangong&utm_term=gongxiangbangong#"){
                        df$promotion_name[i] <- "Baidu Guanjianci Gongxiangbangong"}
                else{
                        df$promotion_name[i] <- "Other"
                }
        }
}

### Process time info
df$time <- df$create_time
str(df)

for (i in 1:nrow(df)) {
        df$month[i] <- month(df$time[i])
        df$date[i] <- as.character(date(df$time[i]))
}
str(df)
head(df)

df$Hour <- substr(df$time, 12, 13)
str(df)


# table(data_clean$promotion_detail)

data_clean <- df[,c("id","time","month","date","Hour","tel","url","purpose_id","source_type","promotion_detail","promotion_name")]
head(data_clean)

data_clean$purpose <- NA

for (i in 1:nrow(data_clean)) {
        # i=1
        if (data_clean$purpose_id[i]==1 && !is.na(data_clean$purpose_id[i])) {
                data_clean$purpose[i] <- "Bangong xuanzhi"
        }
        else if (data_clean$purpose_id[i]==2 && !is.na(data_clean$purpose_id[i])) {
                data_clean$purpose[i] <- "Paizhao"
        }
        else if (data_clean$purpose_id[i]==3 && !is.na(data_clean$purpose_id[i])) {
                data_clean$purpose[i] <- "Yue huodong Changdi"
        }
        else if (data_clean$purpose_id[i]==4 && !is.na(data_clean$purpose_id[i])) {
                data_clean$purpose[i] <- "Shangwu Hezuo"
        }
        else
                data_clean$purpose_id[i] <- "Other"
}

# colnames(data_clean)[8]
# colnames(data_clean)[8] <- "purpose"
head(data_clean)



head(data_clean)
str(data_clean)

dat <- Sys.Date()
dat <- as.character(dat)

data_exp_rec <- data_clean[,c("id","date","Hour","time","tel","purpose","source_type","promotion_detail","promotion_name","url")]
write.csv(data_exp_rec, paste(dat,"data_record.csv",sep = "_"))

### promotion table by day
data_promo <- subset(data_clean, source_type=="promotion")
str(data_promo)

data_exp_by_promotion <- count(data_promo, c("date", "promotion_name"))
head(data_exp_by_promotion)
write.csv(data_exp_by_promotion, paste(dat,"data_promotion.csv",sep = "_"))

### source table by day
data_exp_by_source <- count(data_clean, c("date", "source_type"))
head(data_exp_by_source)
write.csv(data_exp_by_source, paste(dat,"data_source.csv",sep = "_"))

# Export data
library(RCurl)

# ftpUpload(paste(dat,"data_record.csv",sep = "_"), paste('ftp://ftp:ftp@172.16.4.242//', dat,'data_record.csv', sep = ""), userpwd = "yimihaodi/lipan:midi_123")
# ftpUpload(paste(dat,"data_promotion.csv",sep = "_"), paste('ftp://ftp:ftp@172.16.4.242//', dat,'data_promotion.csv', sep = ""), userpwd = "yimihaodi/lipan:midi_123")
# ftpUpload(paste(dat,"data_source.csv",sep = "_"), paste('ftp://ftp:ftp@172.16.4.242//', dat,'data_source.csv', sep = ""), userpwd = "yimihaodi/lipan:midi_123")

###

options(stringsAsFactors = FALSE)
data_promotion_latest <- data_exp_by_promotion
data_promotion_latest <- data_promotion_latest[,c("date","promotion_name","freq")]

data_record_latest <- data_exp_rec
data_record_latest <- data_record_latest[,c("id","date","Hour","time","purpose")]

data_source_latest <- data_exp_by_source
data_source_latest <- data_source_latest[,c("date","source_type","freq")]

ui <- navbarPage("XReservation", id="nav",
                 
                 tabPanel("Promotion",
                          hr(),
                          DT::dataTableOutput("promotion"),
                          downloadButton("downloadData_promotion", "Download")
                  ),
                
                 
                 tabPanel("Source",
                          hr(),
                          DT::dataTableOutput("source"),
                          downloadButton("downloadData_source", "Download")
                 ),
                 
                 tabPanel("Record",
                          hr(),
                          DT::dataTableOutput("record"),
                          downloadButton("downloadData_record", "Download")
                 ),
                 tabPanel("Plotting",
                          hr(),
                          # DT::dataTableOutput("record")
                          plotOutput("res_promotion"),
                          plotOutput("res_source")
                 ),
                 
                 
                 conditionalPanel("false", icon("crosshair"))
)

server <- function(input, output) {
    
        output$downloadData_promotion <- downloadHandler(
                filename = function() {
                        paste(as.character(Sys.Date()), "data_promotion_latest.csv", sep = "")
                },
                content = function(file) {
                        write.csv(data_promotion_latest, file, row.names = FALSE)
                }
        )
        output$downloadData_record <- downloadHandler(
                filename = function() {
                        paste(as.character(Sys.Date()), "data_record_latest.csv", sep = "")
                },
                content = function(file) {
                        write.csv(data_record_latest, file, row.names = FALSE)
                }
        )
        output$downloadData_source <- downloadHandler(
                filename = function() {
                        paste(as.character(Sys.Date()), "data_source_latest.csv", sep = "")
                },
                content = function(file) {
                        write.csv(data_source_latest, file, row.names = FALSE)
                }
        )
        
        
        # choose columns to display
        # diamonds2 = diamonds[sample(nrow(diamonds), 1000), ]
        output$promotion <- DT::renderDataTable({
                DT::datatable(data_promotion_latest[drop = FALSE])
        })
        
        # sorted columns are colored now because CSS are attached to them
        output$record <- DT::renderDataTable({
                DT::datatable(data_record_latest[drop = FALSE])        })
        
        # customize the length drop-down menu; display 5 rows per page by default
        output$source <- DT::renderDataTable({
                DT::datatable(data_source_latest[drop = FALSE])        })
        
        output$res_source <- renderPlot({
                # generate bins based on input$bins from ui.R
                # hist(cars$speed)
                t <- ggplot(data_source_latest, aes(x=date, y=freq, group=source_type, color=source_type)) + geom_line(alpha=0.8) +  geom_point() +
                        labs(title="Reservation Amount", 
                             subtitle="By Source Type", 
                             caption="Source: data_source_latest")
                t  
                      # theme(axis.text.x = element_text(angle=65, vjust=0.6))
        })
        output$res_promotion <- renderPlot({
                # generate bins based on input$bins from ui.R
                # hist(cars$speed)
                k <- ggplot(data_promotion_latest, aes(x=date, y=freq, group=promotion_name, color=promotion_name)) + geom_line(alpha=0.8) + geom_point()
                        labs(title="Reservation Amount from Promotion", 
                             subtitle="By Promotion Channel", 
                             caption="Source: data_promotion_latest")
                k
                        # theme(axis.text.x = element_text(angle=65, vjust=0.6))
        })
        
}

shinyApp(ui, server)