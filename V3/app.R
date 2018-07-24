if (1==1) {
        
        if(!require("leaflet")){
                install.packages("leaflet")
                library("leaflet")
        }
        
        if(!require("scales")){
                install.packages("scales")
                library("scales")
        }
        
        if(!require("lattice")){
                install.packages("lattice")
                library("lattice")
        }
        
        if(!require("DT")){
                install.packages("DT")
                library("DT")
        }
        
        if(!require("shiny")){
                install.packages("shiny")
                library("shiny")
        }
        
        if(!require("geosphere")){
                install.packages("geosphere")
                library("geosphere")
        }
        
        if(!require("dplyr")){
                install.packages("dplyr")
                library("dplyr")
        }
        
        if(!require("stringr")){
                install.packages("stringr")
                library("stringr")
        }
        
        if(!require("RbaiduLBS")){
                install.packages("devtools")
                library("devtools")
                install_github("lijian13/RbaiduLBS")
                library("RbaiduLBS")
        }
        
        if(!require("reshape2")){
                install.packages("reshape2")
                library("reshape2")
        }
        
        if(!require("knitr")){
                install.packages("knitr")
                library("knitr")
        }
        
        if(!require("leaflet")){
                install.packages("leaflet")
                library("leaflet")
        }
        
        if(!require("rvest")){
                install.packages("rvest")
                library("rvest")
        }
        
        if(!require("sp")){
                install.packages("sp")
                library("sp")
        }
        
        if(!require("ggplot2")){
                install.packages("ggplot2")
                library("ggplot2")
        }
        
        # if(!require("sf")){
        #         install.packages("sf")
        #         library("sf")
        # }
        # 
        # if(!require("rMaps")){
        #         install.packages("rMaps")
        #         library("rMaps")
        # }
        # 
        # if(!require("Quandl")){
        #         install.packages("Quandl")
        #         library("Quandl")
        # }
        
        if(!require("reshape2")){
                install.packages("reshape2")
                library("reshape2")
        }
        
        if(!require("knitr")){
                install.packages("knitr")
                library("knitr")
        }
        
        if(!require("plyr")){
                install.packages("plyr")
                library("plyr")
        }
        
        if(!require("ggmap")){
                install.packages("ggmap")
                library("ggmap")
        }
        
        if(!require("curl")){
                install.packages("curl")
                library("curl")
        }
        
}

# Define UI for application that draws a histogram
ui <- navbarPage("XMAP", id="nav",
                   
                   tabPanel("Interactive map",
                            div(class="outer",
                                
                                tags$head(
                                        # Include our custom CSS
                                        includeCSS("styles.css"),
                                        includeScript("gomap.js")
                                ),
                                hr(),
                                hr(),
                                
                                # If not using custom CSS, set height of leafletOutput to a number instead of percent
                                leafletOutput("map", width="100%", height="100%"),
                                
                                # Shiny versions prior to 0.11 should use class = "modal" instead.
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 800, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              # Read Input
                                              h2("Space Explorer"),
                                              
                                              fluidRow(
                                                      column(12,
                                                             textInput("caption", "Address", "Input Address"),
                                                             verbatimTextOutput("value"),
                                                             actionButton("button", "Explore Space", class="btn-block")
                                                      )
                                              )
                                             
                                )
                            )
                   ),
                   
                   tabPanel("Grading",
                            hr(),
                            DT::dataTableOutput("grading")
                   ),
         
                   
                   conditionalPanel("false", icon("crosshair"))
        )


# Define server logic required to draw a histogram
server <- function(input, output) {
        
        output$map <- renderLeaflet({
                
                input$button
                # isolate({
                #         target_name <- as.character(input$caption)
                # })
                target_name <- as.character(input$caption)
                ## Set Target Address
                # target_name <- "上海市银城中路488号"
                # target_name <- "上海市江宁路396号"
                # target_name <- "上海市静安区愚园路68号"
                # target_name <- as.character(input$caption)
                
                target_address <- getGeocoding(address = target_name, city = '上海', ak = 'ZxjZ6VnwlxvRarbnVtg38IcCwpiGLZzN')
                target_address_geo <- target_address
                range <- 1000
                
                ## Set seed
                set.seed(100)
                
                ## Process data to get nearby stores
                
                if (1==1) {
                        
                        ## Find nearby stations
                        
                        ### Define Functions 
                        
                        find_nearby_store <- function(target_address,store_name,range) {
                                store <- store_name
                                target <- target_address
                                set_range <- range
                                nearbystores <- searchPlace(store, location = c(target$lat,target$lng), radius = set_range,ak = "ZxjZ6VnwlxvRarbnVtg38IcCwpiGLZzN")
                                return(nearbystores)
                        }
                        
                        calculate_walking_dist <- function(origin_address,dest) {
                                destination <- dest
                                origin <- origin_address
                                
                                destination$walking_distance <- NA
                                destination$walking_duration <- NA
                                
                                for (i in 1:nrow(destination)) {
                                        t <- searchDirection(origin = paste(origin$lat,origin$lng,sep = ","), destination = paste(destination$lat[i],destination$lng[i],sep = ","), mode = "walking", region = "Shanghai", ak = "ZxjZ6VnwlxvRarbnVtg38IcCwpiGLZzN")
                                        a <- t$result$routes
                                        destination$walking_distance[i] <- a[[1]]$distance
                                        destination$walking_duration[i] <- a[[1]]$duration
                                        # print(i)
                                }
                                destination$walking_duration <- destination$walking_duration/60
                                destination <- destination[,c("name","lat","lng","address","distance","walking_distance","walking_duration")]
                                return(destination)
                        }
                        
                        # calculate_walking_dist(target_name, landmark_geo)
                        # wk_dis_to_centerarea <- calculate_walking_dist(target_name, centerarea_geo)
                        
                        find_geo_location <- function(target_list) {
                                target_list <-star_hotel
                                target <- target_list
                                target$lat <- 0
                                target$lng <- 0
                                
                                for (i in 1:nrow(target)) {
                                        # i=1
                                        tem_address <- getGeocoding(address = target$address[i], city = '上海', ak = 'ZxjZ6VnwlxvRarbnVtg38IcCwpiGLZzN')
                                        target$lat[i] <- tem_address$lat
                                        target$lng[i] <- tem_address$lng
                                }
                                return(target)
                        }
                        
                        calculate_straight_dist <- function(target,storelist) {
                                # target <- target_location
                                # storelist <- store_info
                                storelist$distance <- NA
                                for (i in 1:nrow(storelist)) {
                                        storelist$distance[i] <- distm(c(storelist$lng[i], storelist$lat[i]), c(target$lng[1], target$lat[1]), fun = distHaversine)
                                }
                                return(storelist)
                        }
                        
                        ### Apply functions
                        
                        ## station
                        nearby_station <- find_nearby_store(target_address,"地铁站" ,800)
                        nearby_station <- calculate_walking_dist(target_address,nearby_station)
                        # write.csv(nearby_station,"nearby_station.csv")
                        
                        ## star hotels
                        
                        ### get geo first
                        # star_hotel <- read.csv("~/Desktop/Mixpace/ShinyMap/grading/star_hotel.csv",stringsAsFactors = FALSE)
                        # str(star_hotel)
                        # star_hotel_geo <- find_geo_location(star_hotel)
                        # write.csv(star_hotel_geo,"star_hotel_geo.csv")
                        
                        star_hotel_geo <- read.csv("star_hotel_geo.csv",stringsAsFactors = FALSE)
                        str(star_hotel_geo)
                        
                        ### calculate distnce
                        nearby_star_hotel <- calculate_straight_dist(target_address,star_hotel_geo)
                        
                        ### filter distance that are less than 1000m
                        nearby_star_hotel_1000m_str <- nearby_star_hotel[nearby_star_hotel$distance<1001,]
                        
                        ### calculate wk dis for these hotels
                        nearby_star_hotel_1000m_walk <- calculate_walking_dist(target_address,nearby_star_hotel_1000m_str)
                        nearby_star_hotel_1000m_walk <- nearby_star_hotel_1000m_walk[nearby_star_hotel_1000m_walk$walking_distance<1000,]
                        nearby_star_hotel_1000m_walk <- nearby_star_hotel_1000m_walk[nearby_star_hotel_1000m_walk$walking_duration<10,]
                        # write.csv(nearby_star_hotel_1000m_walk,"nearby_star_hotel.csv")
                        
                        ## a level office building
                        
                        ### waiting for data
                        
                        ## massive business
                        
                        ### find data of nearby shopping center
                        
                        ### find data of average price of each shopping center
                        
                        ## Starbucks
                        
                        nearby_stb_store <- find_nearby_store(target_address,"星巴克",1000)
                        nearby_stb_store <- calculate_walking_dist(target_address,nearby_stb_store)
                        nearby_stb_store_1000m_walk <- nearby_stb_store[nearby_stb_store$walking_distance<1000,]
                        nearby_stb_store_1000m_walk <- nearby_stb_store_1000m_walk[nearby_stb_store_1000m_walk$walking_duration<10,]
                        # write.csv(nearby_stb_store_1000m_walk,"nearby_stb_store.csv")
                        
                        ### MDC
                        nearby_mdc_store <- find_nearby_store(target_address,"麦当劳",2000)
                        nearby_mdc_store <- calculate_walking_dist(target_address,nearby_mdc_store)
                        nearby_mdc_store_2000m_walk <- nearby_mdc_store[nearby_mdc_store$walking_distance<2000,]
                        nearby_mdc_store_2000m_walk <- nearby_mdc_store_2000m_walk[nearby_mdc_store_2000m_walk$walking_duration<20,]
                        # write.csv(nearby_mdc_store_2000m_walk,"nearby_mdc_store.csv")
                        
                        ### restaurants
                        ##################### Considering using meituan or dazhongdianping data
                        nearby_restaurant <- find_nearby_store(target_address,"餐厅",1000)
                        restaurant_avg <- mean(as.integer(as.character(nearby_restaurant$price)))
                        nearby_restaurant <- calculate_walking_dist(target_address,nearby_restaurant)
                        nearby_restaurant_1000m_walk <- nearby_restaurant[nearby_restaurant$walking_distance<1000,]
                        # write.csv(nearby_restaurant_1000m_walk,"nearby_restaurant.csv")
                        
                }
                
                ## Process data for geo transfer
                
                if (1==1) {
                        
                        ### Read Data
                        
                        nearby_star_hotel <- nearby_star_hotel_1000m_walk
                        nearby_mdc_store <- nearby_mdc_store_2000m_walk
                        nearby_station <- nearby_station
                        nearby_stb_store <- nearby_stb_store_1000m_walk
                        nearby_restaurant <- nearby_restaurant_1000m_walk
                        
                        ### Clean and Combine data
                        
                        str(nearby_star_hotel)
                        nearby_star_hotel <- nearby_star_hotel[,c("name","lat","lng","address","walking_distance","walking_duration")]
                        if (nrow(nearby_star_hotel)>0) {
                                nearby_star_hotel$cid <- "hotel"       
                        }
                        
                        str(nearby_mdc_store)
                        nearby_mdc_store <- nearby_mdc_store[,c("name","lat","lng","address","walking_distance","walking_duration")]
                        if (nrow(nearby_mdc_store)>0) {
                                nearby_mdc_store$cid <- "mdc"       
                        }
                        
                        str(nearby_station)
                        nearby_station <- nearby_station[,c("name","lat","lng","address","walking_distance","walking_duration")]
                        if (nrow(nearby_station)>0) {
                                nearby_station$cid <- "station"       
                        }
                        
                        str(nearby_stb_store)
                        nearby_stb_store <- nearby_stb_store[,c("name","lat","lng","address","walking_distance","walking_duration")]
                        if (nrow(nearby_stb_store)>0) {
                                nearby_stb_store$cid <- "stb"       
                        }
                        
                        str(nearby_restaurant)
                        nearby_restaurant <- nearby_restaurant[,c("name","lat","lng","address","walking_distance","walking_duration")]
                        if (nrow(nearby_restaurant)>0) {
                                nearby_restaurant$cid <- "restaurant"       
                        }
                        
                        db_nearby_amenty <- rbind(nearby_star_hotel, nearby_mdc_store, nearby_station, nearby_stb_store, nearby_restaurant)
                        # write.csv(db_nearby_amenty,"db_nearby_amenty.csv")
                        str(db_nearby_amenty)
                        
                        ### Define Function
                        
                        ### Define Transform Function
                        
                        a = 6378245.0
                        x_pi = 3.14159265358979324 * 3000.0 / 180.0
                        ee = 0.00669342162296594323
                        pi = 3.1415926535897932384626
                        
                        transformlat <- function(lng, lat) {
                                ret = -100.0 + 2.0 * lng + 3.0 * lat + 0.2 * lat * lat + 0.1 * lng * lat + 0.2 * sqrt(abs(lng))
                                ret <- ret + (20.0 * sin(6.0 * lng * pi) + 20.0 * sin(2.0 * lng * pi)) * 2.0 / 3.0
                                ret <- ret + (20.0 * sin(lat * pi) + 40.0 * sin(lat / 3.0 * pi)) * 2.0 / 3.0
                                ret <- ret + (160.0 * sin(lat / 12.0 * pi) + 320 * sin(lat * pi / 30.0)) * 2.0 / 3.0
                                return(ret)
                        }
                        
                        transformlng <- function(lng,  lat) {
                                ret = 300.0 + lng + 2.0 * lat + 0.1 * lng * lng + 0.1 * lng * lat + 0.1 * sqrt(abs(lng))
                                ret <- ret + (20.0 * sin(6.0 * lng * pi) + 20.0 * sin(2.0 * lng * pi)) * 2.0 / 3.0
                                ret <- ret + (20.0 * sin(lng * pi) + 40.0 * sin(lng / 3.0 * pi)) * 2.0 / 3.0
                                ret <- ret + (150.0 * sin(lng / 12.0 * pi) + 300.0 * sin(lng / 30.0 * pi)) * 2.0 / 3.0
                                return(ret)
                        }
                        
                        bd09togcj02 <- function(bd_lon, bd_lat) {
                                x = bd_lon - 0.0065
                                y = bd_lat - 0.006
                                z = sqrt(x * x + y * y) - 0.00002 * sin(y * x_pi)
                                theta = atan2(y, x) - 0.000003 * cos(x * x_pi)
                                gg_lng = z * cos(theta)
                                gg_lat = z * sin(theta)
                                return(c(gg_lng, gg_lat))
                        }
                        
                        gcj02towgs84 <- function(lng, lat) {
                                dlat = transformlat(lng - 105.0, lat - 35.0)
                                dlng = transformlng(lng - 105.0, lat - 35.0)
                                radlat = lat / 180.0 * pi
                                magic = sin(radlat)
                                magic = 1 - ee * magic * magic
                                sqrtmagic = sqrt(magic)
                                dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
                                dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
                                mglat = lat + dlat
                                mglng = lng + dlng
                                return( c(lng * 2 - mglng, lat * 2 - mglat))
                        }
                        
                        add_wgs_coor <- function(org_data) {
                                data_filter <- org_data
                                data_filter$lat_wgs84 <- 0
                                data_filter$lng_wgs84 <- 0        
                                
                                for (i in 1:nrow(data_filter)) {
                                        tem_lat <- data_filter[i,"lat"]
                                        tem_lng <- data_filter[i,"lng"]
                                        new_gps_1 <- bd09togcj02(tem_lng,tem_lat)
                                        new_gps_1_lat <- new_gps_1[2]
                                        new_gps_1_lng <- new_gps_1[1]
                                        new_gps_2 = gcj02towgs84(new_gps_1_lng, new_gps_1_lat)
                                        data_filter$lat_wgs84[i] <- new_gps_2[2]
                                        data_filter$lng_wgs84[i] <- new_gps_2[1]
                                }
                                return(data_filter)
                        }
                        
                        ### Apply function 
                        
                        target_address_geo <- add_wgs_coor(target_address)
                        target_address_geo <- target_address_geo[,c("address","city","lat_wgs84","lng_wgs84")]
                        colnames(target_address_geo)[3] <-"lat"
                        colnames(target_address_geo)[4] <-"lng"
                        # write.csv(target_address_geo,"target_address_geo.csv")
                        
                        db_nearby_amenty <- add_wgs_coor(db_nearby_amenty)
                        str(db_nearby_amenty)
                        
                        db_nearby_amenty <- db_nearby_amenty[,c("name","lat_wgs84","lng_wgs84","address","walking_distance","walking_duration","cid")]
                        colnames(db_nearby_amenty)[2] <-"lat"
                        colnames(db_nearby_amenty)[3] <-"lng"
                        str(db_nearby_amenty)
                        
                        ### Export 
                        
                        # write.csv(db_nearby_amenty,"db_nearby_amenty.csv")
                        
                }
                
                ## Read data
                if (1==1) {
                        db_nearby_amenty <- db_nearby_amenty
                        db_nearby_amenty <- db_nearby_amenty[,c("name","lat","lng","address","walking_distance","walking_duration","cid")]
                        df_center_road <- read.csv("df_center_road.csv",stringsAsFactors = FALSE)
                        df_landmark <- read.csv("df_landmark.csv",stringsAsFactors = FALSE)
                }
                
                ## Grading
                
                if (1==1) {
                        result <- data.frame(Criteria=character(),
                                             data=numeric(),
                                             minimum_standard=numeric(),
                                             pass_or_not=character(),
                                             stringsAsFactors = FALSE)
                        ### Define each criteria
                        result[1,"Criteria"] <- "800米内地铁站数量"
                        result[2,"Criteria"] <- "800米内最近地铁站步行距离"
                        result[3,"Criteria"] <- "800米内最近地铁站步行时间"
                        result[4,"Criteria"] <- "1000米内4/5星级饭店数量"
                        result[5,"Criteria"] <- "1000米甲级写字楼数量"
                        result[6,"Criteria"] <- "1000米规模商业大众点评人均价格"
                        result[7,"Criteria"] <- "1000米内星巴克数量"
                        result[8,"Criteria"] <- "1000米内最近星巴克步行距离"
                        result[9,"Criteria"] <- "1000米内最近星巴克步行时间"
                        result[10,"Criteria"] <- "2000米内麦当劳数量"
                        result[11,"Criteria"] <- "2000米内最近麦当劳步行距离"
                        result[12,"Criteria"] <- "2000米内最近麦当劳步行时间"
                        result[13,"Criteria"] <- "1000米内餐馆数量"
                        result[14,"Criteria"] <- "1000米内餐馆百度地图人均价格"
                        
                        ### Fill in data
                        tem <- subset(db_nearby_amenty,cid=="station")
                        if (nrow(tem)>0) {
                                result[1,"data"] <- nrow(tem)
                                result[2,"data"] <- min(tem$walking_distance)
                                result[3,"data"] <- round(min(tem$walking_duration),1)
                        }
                        
                        tem <- subset(db_nearby_amenty,cid=="hotel")
                        if (nrow(tem)>0) {
                                result[4,"data"] <- nrow(tem)
                        }
                        
                        tem <- subset(db_nearby_amenty,cid=="stb")
                        if (nrow(tem)>0) {
                                result[7,"data"] <- nrow(tem)
                                result[8,"data"] <- min(tem$walking_distance)
                                result[9,"data"] <- round(min(tem$walking_duration),1)
                        }
                        
                        tem <- subset(db_nearby_amenty,cid=="mdc")
                        if (nrow(tem)>0) {
                                result[10,"data"] <- nrow(tem)
                                result[11,"data"] <- min(tem$walking_distance)
                                result[12,"data"] <- round(min(tem$walking_duration),1)
                        }
                        
                        tem <- subset(db_nearby_amenty,cid=="restaurant")
                        if (nrow(tem)>0) {
                                result[13,"data"] <- nrow(tem)
                                result[14,"data"] <- restaurant_avg
                        }
                        
                        
                        
                        ## Set Minimum Standard
                        
                        result$minimum_standard <- c(1,800,10,1,1,59,1,1000,10,1,2000,20,10,30)
                        result
                        str(result)
                        
                        ## Judging weather pass or not 
                        for (i in 1:nrow(result)) {
                                if (!is.na(result$data)[i]) {
                                        if (is.na(result$data)[i]>=is.na(result$minimum_standard)[i]) {
                                                result$pass_or_not[i] <- "yes"
                                        }
                                        else{
                                                result$pass_or_not[i] <- "no"
                                        }
                                }
                        }
                        # write.csv(result,"result_grading.csv")
                }
                
                
                ## test
                
                # Leaflet bindings are a bit slow; for now we'll just sample to compensate
                set.seed(100)
                # zipdata <- allzips[sample.int(100, 10000),]
                # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
                # will be drawn last and thus be easier to see
                # zipdata <- zipdata[order(zipdata$centile),]
                
                # function(input, output, session) {
                
                ## Interactive Map ###########################################
                
                # Create the map
                # output$map <- renderLeaflet({
                
                m = leaflet()
                m = addTiles(m)
                m = addTiles(m, group = "OSM") 
                m = addProviderTiles(m, "OpenStreetMap.BlackAndWhite", group = "BlackAndWhite(default)")
                m = addProviderTiles(m,"Stamen.Toner", group = "Toner")
                m = addProviderTiles(m, "Stamen.TonerLite", group = "Toner Lite")
                # m = addProviderTiles(m, "Stamen.Watercolor", group = "Watercolor") 
                m = addProviderTiles(m, "Stamen.TonerHybrid", group = "TonerHybrid") 
                m = addProviderTiles(m, "Esri.WorldImagery", group = "WorldImagery")
                # m = addProviderTiles(m, "OpenMapSurfer.Roads", group = "OpenMapSurfer.Roads")
                
                m = addLayersControl(m,
                                     baseGroups = c("BlackAndWhite(default)","OSM","Toner Lite","Toner", "TonerHybrid","WorldImagery"),
                                     overlayGroups = c("1. nearby station", "2. nearby hotel", "3. nearby Mcdonalds", "4. nearby Starbucks", "5. Center Road", "6. City Center", "7. nearby Restaurant", "X1 - Mixpace(500m)", "X2 - Mixpace(seat)", "X3 - All Competitors(500m)", "X4 - All Competitors(seat)")
                                     # layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
                )
                
                # str(db_nearby_amenty$cid)
                
                ### Define Icon
                icon.fa.target <- makeAwesomeIcon(icon = "star", markerColor = "orange", library = "fa", iconColor = "black")
                icon.fa.train <- makeAwesomeIcon(icon = "subway", markerColor = "purple", library = "fa", iconColor = "black")
                icon.fa.stb <- makeAwesomeIcon(icon = "coffee", markerColor = "green", library = "fa", iconColor = "black")
                icon.fa.mdc <- makeAwesomeIcon(icon = "meetup", markerColor = "red", library = "fa", iconColor = "black")
                icon.fa.hotel <- makeAwesomeIcon(icon = "hotel", markerColor = "blue", library = "fa", iconColor = "black")
                icon.landmark <- makeAwesomeIcon(icon = "photo", markerColor = "red", library = "fa", iconColor = "black")
                icon.fa.restaurant <- makeAwesomeIcon(text = "F", markerColor = "lightgreen", library = "fa",iconColor = "black")
                
                ### Add to Maps 
                
                # Target
                m <- addAwesomeMarkers(m, lng = target_address_geo$lng, lat = target_address_geo$lat, label=target_address_geo$address, labelOptions = labelOptions(noHide = F, direction = 'bottom', offset=c(0,15)),icon = icon.fa.target)
                m = addCircles(m, lng = target_address_geo$lng, lat = target_address_geo$lat, weight = 2,fillOpacity=0.1, popup="500m" ,color = "blue", radius = 500)
                m = addCircles(m, lng = target_address_geo$lng, lat = target_address_geo$lat, weight = 1,fillOpacity=0.05, popup="1000m" ,color = "purple", radius = 1000)
                
                # Station
                db_nearby_amenty_station <- db_nearby_amenty[db_nearby_amenty$cid=="station",]
                m <- addAwesomeMarkers(m, lng = db_nearby_amenty_station$lng, lat = db_nearby_amenty_station$lat, popup=db_nearby_amenty_station$name,icon = icon.fa.train, group = ("1. nearby station"))
                
                # Hotel
                db_nearby_amenty_hotel <- db_nearby_amenty[db_nearby_amenty$cid=="hotel",]
                m <- addAwesomeMarkers(m, lng = db_nearby_amenty_hotel$lng, lat = db_nearby_amenty_hotel$lat, popup=db_nearby_amenty_hotel$name,icon = icon.fa.hotel, group = ("2. nearby hotel"))
                
                # MDC
                db_nearby_amenty_mdc <- db_nearby_amenty[db_nearby_amenty$cid=="mdc",]
                m <- addAwesomeMarkers(m, lng = db_nearby_amenty_mdc$lng, lat = db_nearby_amenty_mdc$lat, popup=db_nearby_amenty_mdc$name,icon = icon.fa.mdc, group = ("3. nearby Mcdonalds"))
                
                # Starbucks
                db_nearby_amenty_stb <- db_nearby_amenty[db_nearby_amenty$cid=="stb",]
                m <- addAwesomeMarkers(m, lng = db_nearby_amenty_stb$lng, lat = db_nearby_amenty_stb$lat, popup=db_nearby_amenty_stb$name,icon = icon.fa.stb, group = ("4. nearby Starbucks"))
                
                # Add Restaurants
                db_nearby_amenty_restaurant <- db_nearby_amenty[db_nearby_amenty$cid=="restaurant",]
                m <- addAwesomeMarkers(m, lng = db_nearby_amenty_restaurant$lng, lat = db_nearby_amenty_restaurant$lat, popup=db_nearby_amenty_restaurant$name,icon = icon.fa.restaurant, group = ("7. nearby Restaurant"))
                
                
                
                # Competitor
                
                df_competor <- read.csv("df_competor.csv",stringsAsFactors = FALSE)
                m = addCircles(m, lng = df_competor$lng, lat = df_competor$lat, weight = 1,color = "purple", radius = sqrt(df_competor$seat) * 15, label = df_competor$brand, labelOptions = labelOptions(noHide = F, direction = 'bottom',offset=c(0,15)),group = "X4 - All Competitors(seat)")
                m = addCircles(m, lng = df_competor$lng, lat = df_competor$lat, weight = 1,color = "orange", radius = 500, label = df_competor$brand, labelOptions = labelOptions(noHide = F, direction = 'bottom', offset=c(0,15)), group = "X3 - All Competitors(500m)")
                
                
                # Mixpace
                df_mixpace <- read.csv("df_mixpace.csv",stringsAsFactors = FALSE)
                m = addCircles(m, lng = df_mixpace$lng, lat = df_mixpace$lat, weight = 1, color = "blue", radius = 500, label = df_mixpace$name,labelOptions = labelOptions(noHide = F, direction = 'bottom',offset=c(0,15)), group = "X1 - Mixpace(500m)")
                m = addCircles(m, lng = df_mixpace$lng, lat = df_mixpace$lat, weight = 1, color = "blue", radius = sqrt(df_mixpace$seat) * 15, label = df_mixpace$name, labelOptions = labelOptions(noHide = F, direction = 'bottom', offset=c(0,15)),group = "X2 - Mixpace(seat)")
                
                # Add center road
                
                str(df_center_road)
                for (i in c(2:22,23)) {
                        m = addPolylines(m, lat = c(df_center_road[i,"lat_wgs84_bgn"],df_center_road[i,"lat_wgs84_end"]), lng = c(df_center_road[i,"lng_wgs84_bgn"],df_center_road[i,"lng_wgs84_end"]), color = "red",group = "5. Center Road",label = df_center_road[i,"name"],labelOptions = labelOptions(noHide = F, direction = 'bottom', offset=c(0,15)))
                }
                
                m <- addPolylines(m, lat = c(df_center_road[24,"lat_wgs84_bgn"],df_center_road[25,"lat_wgs84_end"]), lng = c(df_center_road[24,"lng_wgs84_bgn"],df_center_road[25,"lng_wgs84_end"]), color = "red",group = "5. Center Road",label = df_center_road[i,"name"],labelOptions = labelOptions(noHide = F, direction = 'bottom', offset=c(0,15)))
                
                m <- addAwesomeMarkers(m, lng = df_landmark$lng, lat = df_landmark$lat, label=df_landmark$landmark,labelOptions = labelOptions(noHide = F, direction = 'bottom', offset=c(0,15)),icon = icon.landmark, group = ("6. City Center"))
                
                ### Add button
                m<- m %>% addTiles() %>%
                        addEasyButton(easyButton(
                                icon = "fa-globe", title = "Zoom to Level 5",
                                onClick = JS("function(btn, map){ map.setZoom(5);}"))) %>%
                        addEasyButton(easyButton(
                                icon = "fa-crosshairs", title = "Locate Shanghai",
                                onClick = JS("function(btn, map){ map.setView([31.23914,121.4823],12);}")))
                
                ### hide layer 
                m <- m %>% hideGroup(c("5. Center Road", "6. City Center", "X1 - Mixpace(500m)", "X2 - Mixpace(seat)", "X3 - All Competitors(500m)", "X4 - All Competitors(seat)"))
                ### Show the Map
                # m
        })
        
        output$grading <- DT::renderDataTable({
                
                
                input$button
                # isolate({
                #         target_name <- as.character(input$caption)
                # })
                target_name <- as.character(input$caption)
                ## Set Target Address
                # target_name <- "上海市银城中路488号"
                # target_name <- "上海市江宁路396号"
                # target_name <- "上海市静安区愚园路68号"
                # target_name <- as.character(input$caption)
                
                target_address <- getGeocoding(address = target_name, city = '上海', ak = 'ZxjZ6VnwlxvRarbnVtg38IcCwpiGLZzN')
                target_address_geo <- target_address
                range <- 1000
                
                ## Set seed
                set.seed(100)
                
                ## Process data for get nearby stores
                
                if (1==1) {
                        
                        ## Find nearby stations
                        
                        ### Define Functions 
                        
                        find_nearby_store <- function(target_address,store_name,range) {
                                store <- store_name
                                target <- target_address
                                set_range <- range
                                nearbystores <- searchPlace(store, location = c(target$lat,target$lng), radius = set_range,ak = "ZxjZ6VnwlxvRarbnVtg38IcCwpiGLZzN")
                                return(nearbystores)
                        }
                        
                        calculate_walking_dist <- function(origin_address,dest) {
                                destination <- dest
                                origin <- origin_address
                                
                                destination$walking_distance <- NA
                                destination$walking_duration <- NA
                                
                                for (i in 1:nrow(destination)) {
                                        t <- searchDirection(origin = paste(origin$lat,origin$lng,sep = ","), destination = paste(destination$lat[i],destination$lng[i],sep = ","), mode = "walking", region = "Shanghai", ak = "ZxjZ6VnwlxvRarbnVtg38IcCwpiGLZzN")
                                        a <- t$result$routes
                                        destination$walking_distance[i] <- a[[1]]$distance
                                        destination$walking_duration[i] <- a[[1]]$duration
                                        # print(i)
                                }
                                destination$walking_duration <- destination$walking_duration/60
                                destination <- destination[,c("name","lat","lng","address","distance","walking_distance","walking_duration")]
                                return(destination)
                        }
                        
                        # calculate_walking_dist(target_name, landmark_geo)
                        # wk_dis_to_centerarea <- calculate_walking_dist(target_name, centerarea_geo)
                        
                        find_geo_location <- function(target_list) {
                                target_list <-star_hotel
                                target <- target_list
                                target$lat <- 0
                                target$lng <- 0
                                
                                for (i in 1:nrow(target)) {
                                        # i=1
                                        tem_address <- getGeocoding(address = target$address[i], city = '上海', ak = 'ZxjZ6VnwlxvRarbnVtg38IcCwpiGLZzN')
                                        target$lat[i] <- tem_address$lat
                                        target$lng[i] <- tem_address$lng
                                }
                                return(target)
                        }
                        
                        calculate_straight_dist <- function(target,storelist) {
                                # target <- target_location
                                # storelist <- store_info
                                storelist$distance <- NA
                                for (i in 1:nrow(storelist)) {
                                        storelist$distance[i] <- distm(c(storelist$lng[i], storelist$lat[i]), c(target$lng[1], target$lat[1]), fun = distHaversine)
                                }
                                return(storelist)
                        }
                        
                        ### Apply functions
                        
                        ## station
                        nearby_station <- find_nearby_store(target_address,"地铁站" ,800)
                        nearby_station <- calculate_walking_dist(target_address,nearby_station)
                        # write.csv(nearby_station,"nearby_station.csv")
                        
                        ## star hotels
                        
                        ### get geo first
                        # star_hotel <- read.csv("~/Desktop/Mixpace/ShinyMap/grading/star_hotel.csv",stringsAsFactors = FALSE)
                        # str(star_hotel)
                        # star_hotel_geo <- find_geo_location(star_hotel)
                        # write.csv(star_hotel_geo,"star_hotel_geo.csv")
                        
                        star_hotel_geo <- read.csv("star_hotel_geo.csv",stringsAsFactors = FALSE)
                        str(star_hotel_geo)
                        
                        ### calculate distnce
                        nearby_star_hotel <- calculate_straight_dist(target_address,star_hotel_geo)
                        
                        ### filter distance that are less than 1000m
                        nearby_star_hotel_1000m_str <- nearby_star_hotel[nearby_star_hotel$distance<1001,]
                        
                        ### calculate wk dis for these hotels
                        nearby_star_hotel_1000m_walk <- calculate_walking_dist(target_address,nearby_star_hotel_1000m_str)
                        nearby_star_hotel_1000m_walk <- nearby_star_hotel_1000m_walk[nearby_star_hotel_1000m_walk$walking_distance<1000,]
                        nearby_star_hotel_1000m_walk <- nearby_star_hotel_1000m_walk[nearby_star_hotel_1000m_walk$walking_duration<10,]
                        # write.csv(nearby_star_hotel_1000m_walk,"nearby_star_hotel.csv")
                        
                        ## a level office building
                        
                        ### waiting for data
                        
                        ## massive business
                        
                        ### find data of nearby shopping center
                        
                        ### find data of average price of each shopping center
                        
                        ## Starbucks
                        
                        nearby_stb_store <- find_nearby_store(target_address,"星巴克",1000)
                        nearby_stb_store <- calculate_walking_dist(target_address,nearby_stb_store)
                        nearby_stb_store_1000m_walk <- nearby_stb_store[nearby_stb_store$walking_distance<1000,]
                        nearby_stb_store_1000m_walk <- nearby_stb_store_1000m_walk[nearby_stb_store_1000m_walk$walking_duration<10,]
                        # write.csv(nearby_stb_store_1000m_walk,"nearby_stb_store.csv")
                        
                        ### MDC
                        nearby_mdc_store <- find_nearby_store(target_address,"麦当劳",2000)
                        nearby_mdc_store <- calculate_walking_dist(target_address,nearby_mdc_store)
                        nearby_mdc_store_2000m_walk <- nearby_mdc_store[nearby_mdc_store$walking_distance<2000,]
                        nearby_mdc_store_2000m_walk <- nearby_mdc_store_2000m_walk[nearby_mdc_store_2000m_walk$walking_duration<20,]
                        # write.csv(nearby_mdc_store_2000m_walk,"nearby_mdc_store.csv")
                        
                        ### restaurants
                        ##################### Considering using meituan or dazhongdianping data
                        nearby_restaurant <- find_nearby_store(target_address,"餐厅",1000)
                        restaurant_avg <- mean(as.integer(as.character(nearby_restaurant$price)))
                        nearby_restaurant <- calculate_walking_dist(target_address,nearby_restaurant)
                        nearby_restaurant_1000m_walk <- nearby_restaurant[nearby_restaurant$walking_distance<1000,]
                        # write.csv(nearby_restaurant_1000m_walk,"nearby_restaurant.csv")
                        
                }
                
                ## Process data for geo transfer
                
                if (1==1) {
                        
                        ### Read Data
                        
                        nearby_star_hotel <- nearby_star_hotel_1000m_walk
                        nearby_mdc_store <- nearby_mdc_store_2000m_walk
                        nearby_station <- nearby_station
                        nearby_stb_store <- nearby_stb_store_1000m_walk
                        nearby_restaurant <- nearby_restaurant_1000m_walk
                        
                        ### Clean and Combine data
                        
                        str(nearby_star_hotel)
                        nearby_star_hotel <- nearby_star_hotel[,c("name","lat","lng","address","walking_distance","walking_duration")]
                        if (nrow(nearby_star_hotel)>0) {
                                nearby_star_hotel$cid <- "hotel"       
                        }
                        
                        str(nearby_mdc_store)
                        nearby_mdc_store <- nearby_mdc_store[,c("name","lat","lng","address","walking_distance","walking_duration")]
                        if (nrow(nearby_mdc_store)>0) {
                                nearby_mdc_store$cid <- "mdc"       
                        }
                        
                        str(nearby_station)
                        nearby_station <- nearby_station[,c("name","lat","lng","address","walking_distance","walking_duration")]
                        if (nrow(nearby_station)>0) {
                                nearby_station$cid <- "station"       
                        }
                        
                        str(nearby_stb_store)
                        nearby_stb_store <- nearby_stb_store[,c("name","lat","lng","address","walking_distance","walking_duration")]
                        if (nrow(nearby_stb_store)>0) {
                                nearby_stb_store$cid <- "stb"       
                        }
                        
                        str(nearby_restaurant)
                        nearby_restaurant <- nearby_restaurant[,c("name","lat","lng","address","walking_distance","walking_duration")]
                        if (nrow(nearby_restaurant)>0) {
                                nearby_restaurant$cid <- "restaurant"       
                        }
                        
                        db_nearby_amenty <- rbind(nearby_star_hotel, nearby_mdc_store, nearby_station, nearby_stb_store, nearby_restaurant)
                        # write.csv(db_nearby_amenty,"db_nearby_amenty.csv")
                        str(db_nearby_amenty)
                        
                        ### Define Function
                        
                        ### Define Transform Function
                        
                        a = 6378245.0
                        x_pi = 3.14159265358979324 * 3000.0 / 180.0
                        ee = 0.00669342162296594323
                        pi = 3.1415926535897932384626
                        
                        transformlat <- function(lng, lat) {
                                ret = -100.0 + 2.0 * lng + 3.0 * lat + 0.2 * lat * lat + 0.1 * lng * lat + 0.2 * sqrt(abs(lng))
                                ret <- ret + (20.0 * sin(6.0 * lng * pi) + 20.0 * sin(2.0 * lng * pi)) * 2.0 / 3.0
                                ret <- ret + (20.0 * sin(lat * pi) + 40.0 * sin(lat / 3.0 * pi)) * 2.0 / 3.0
                                ret <- ret + (160.0 * sin(lat / 12.0 * pi) + 320 * sin(lat * pi / 30.0)) * 2.0 / 3.0
                                return(ret)
                        }
                        
                        transformlng <- function(lng,  lat) {
                                ret = 300.0 + lng + 2.0 * lat + 0.1 * lng * lng + 0.1 * lng * lat + 0.1 * sqrt(abs(lng))
                                ret <- ret + (20.0 * sin(6.0 * lng * pi) + 20.0 * sin(2.0 * lng * pi)) * 2.0 / 3.0
                                ret <- ret + (20.0 * sin(lng * pi) + 40.0 * sin(lng / 3.0 * pi)) * 2.0 / 3.0
                                ret <- ret + (150.0 * sin(lng / 12.0 * pi) + 300.0 * sin(lng / 30.0 * pi)) * 2.0 / 3.0
                                return(ret)
                        }
                        
                        bd09togcj02 <- function(bd_lon, bd_lat) {
                                x = bd_lon - 0.0065
                                y = bd_lat - 0.006
                                z = sqrt(x * x + y * y) - 0.00002 * sin(y * x_pi)
                                theta = atan2(y, x) - 0.000003 * cos(x * x_pi)
                                gg_lng = z * cos(theta)
                                gg_lat = z * sin(theta)
                                return(c(gg_lng, gg_lat))
                        }
                        
                        gcj02towgs84 <- function(lng, lat) {
                                dlat = transformlat(lng - 105.0, lat - 35.0)
                                dlng = transformlng(lng - 105.0, lat - 35.0)
                                radlat = lat / 180.0 * pi
                                magic = sin(radlat)
                                magic = 1 - ee * magic * magic
                                sqrtmagic = sqrt(magic)
                                dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
                                dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
                                mglat = lat + dlat
                                mglng = lng + dlng
                                return( c(lng * 2 - mglng, lat * 2 - mglat))
                        }
                        
                        add_wgs_coor <- function(org_data) {
                                data_filter <- org_data
                                data_filter$lat_wgs84 <- 0
                                data_filter$lng_wgs84 <- 0        
                                
                                for (i in 1:nrow(data_filter)) {
                                        tem_lat <- data_filter[i,"lat"]
                                        tem_lng <- data_filter[i,"lng"]
                                        new_gps_1 <- bd09togcj02(tem_lng,tem_lat)
                                        new_gps_1_lat <- new_gps_1[2]
                                        new_gps_1_lng <- new_gps_1[1]
                                        new_gps_2 = gcj02towgs84(new_gps_1_lng, new_gps_1_lat)
                                        data_filter$lat_wgs84[i] <- new_gps_2[2]
                                        data_filter$lng_wgs84[i] <- new_gps_2[1]
                                }
                                return(data_filter)
                        }
                        
                        ### Apply function 
                        
                        target_address_geo <- add_wgs_coor(target_address)
                        target_address_geo <- target_address_geo[,c("address","city","lat_wgs84","lng_wgs84")]
                        colnames(target_address_geo)[3] <-"lat"
                        colnames(target_address_geo)[4] <-"lng"
                        # write.csv(target_address_geo,"target_address_geo.csv")
                        
                        db_nearby_amenty <- add_wgs_coor(db_nearby_amenty)
                        str(db_nearby_amenty)
                        
                        db_nearby_amenty <- db_nearby_amenty[,c("name","lat_wgs84","lng_wgs84","address","walking_distance","walking_duration","cid")]
                        colnames(db_nearby_amenty)[2] <-"lat"
                        colnames(db_nearby_amenty)[3] <-"lng"
                        str(db_nearby_amenty)
                        
                }
                
                ## Read data
                if (1==1) {
                        db_nearby_amenty <- db_nearby_amenty
                        db_nearby_amenty <- db_nearby_amenty[,c("name","lat","lng","address","walking_distance","walking_duration","cid")]
                        df_center_road <- read.csv("df_center_road.csv",stringsAsFactors = FALSE)
                        df_landmark <- read.csv("df_landmark.csv",stringsAsFactors = FALSE)
                }
                
                ## Grading
                
                if (1==1) {
                        result <- data.frame(Criteria=character(),
                                             data=numeric(),
                                             minimum_standard=numeric(),
                                             pass_or_not=character(),
                                             stringsAsFactors = FALSE)
                        ### Define each criteria
                        result[1,"Criteria"] <- "800米内地铁站数量"
                        result[2,"Criteria"] <- "800米内最近地铁站步行距离"
                        result[3,"Criteria"] <- "800米内最近地铁站步行时间"
                        result[4,"Criteria"] <- "1000米内4/5星级饭店数量"
                        result[5,"Criteria"] <- "1000米甲级写字楼数量"
                        result[6,"Criteria"] <- "1000米规模商业大众点评人均价格"
                        result[7,"Criteria"] <- "1000米内星巴克数量"
                        result[8,"Criteria"] <- "1000米内最近星巴克步行距离"
                        result[9,"Criteria"] <- "1000米内最近星巴克步行时间"
                        result[10,"Criteria"] <- "2000米内麦当劳数量"
                        result[11,"Criteria"] <- "2000米内最近麦当劳步行距离"
                        result[12,"Criteria"] <- "2000米内最近麦当劳步行时间"
                        result[13,"Criteria"] <- "1000米内餐馆数量"
                        result[14,"Criteria"] <- "1000米内餐馆百度地图人均价格"
                        
                        ### Fill in data
                        tem <- subset(db_nearby_amenty,cid=="station")
                        if (nrow(tem)>0) {
                                result[1,"data"] <- nrow(tem)
                                result[2,"data"] <- min(tem$walking_distance)
                                result[3,"data"] <- round(min(tem$walking_duration),1)
                        }
                        
                        tem <- subset(db_nearby_amenty,cid=="hotel")
                        if (nrow(tem)>0) {
                                result[4,"data"] <- nrow(tem)
                        }
                        
                        tem <- subset(db_nearby_amenty,cid=="stb")
                        if (nrow(tem)>0) {
                                result[7,"data"] <- nrow(tem)
                                result[8,"data"] <- min(tem$walking_distance)
                                result[9,"data"] <- round(min(tem$walking_duration),1)
                        }
                        
                        tem <- subset(db_nearby_amenty,cid=="mdc")
                        if (nrow(tem)>0) {
                                result[10,"data"] <- nrow(tem)
                                result[11,"data"] <- min(tem$walking_distance)
                                result[12,"data"] <- round(min(tem$walking_duration),1)
                        }
                        
                        tem <- subset(db_nearby_amenty,cid=="restaurant")
                        if (nrow(tem)>0) {
                                result[13,"data"] <- nrow(tem)
                                result[14,"data"] <- restaurant_avg
                        }
                        
                        
                        ## Set Minimum Standard
                        
                        result$minimum_standard <- c(1,800,10,1,1,59,1,1000,10,1,2000,20,10,30)
                        result
                        str(result)
                        
                        ## Judging weather pass or not 
                        for (i in 1:nrow(result)) {
                                if (!is.na(result$data)[i]) {
                                        if (is.na(result$data)[i]>=is.na(result$minimum_standard)[i]) {
                                                result$pass_or_not[i] <- "yes"
                                        }
                                        else{
                                                result$pass_or_not[i] <- "no"
                                        }
                                }
                        }
                        # write.csv(result,"result_grading.csv")
                }
                
                
                
                # result <- read.csv("result_grading.csv",stringsAsFactors = FALSE)
                # result <- result[,2:5]
                result_rec <- result
                colnames(result_rec) <- c("维度","数据","最低要求","是否通过")
                datatable(result_rec, filter = 'top', options = list(
                        pageLength = 20, autoWidth = TRUE
                ))
        })
        
}
           
  
# Run the application 
shinyApp(ui = ui, server = server)



