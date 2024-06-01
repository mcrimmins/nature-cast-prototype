#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# required libraries
library(RCurl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(plotly)
library(shinyWidgets)

##### FUNCTIONS #####
# adjusted doy function
adj_date = function(x, start.month = 10L, start.dayMonth = 1L){
  start.doy = lubridate::yday(lubridate::make_date(2001, start.month, start.dayMonth))
  start.yr = lubridate::year(x) - ( lubridate::yday(x) < start.doy)
  start.date = lubridate::make_date(start.yr, start.month, start.dayMonth)
  adj.doy = as.integer(x - start.date + 1L)
  # Year offset
  offset = ifelse(lubridate::yday(x) >= start.doy, 1, 0)
  # Water year
  adj.year = lubridate::year(x) + offset
  df<-data.frame(x,adj.doy,adj.year)
  colnames(df) <- c("date","adjDOY","adjYear")
  return(df)
}

# create function for card
card <- function(title, targClimo, minClimo, maxClimo, targDate, fcstClimo, fcstDiff, prevYr) {
  HTML(
    paste0(
      '<div class="card">
      <div class="container">
      <h4><i>', title, '</i></h4>
      <hr>
      <p>Average Event date: ', targClimo, '</p>
      <p>Event date range (1991-2020): ', minClimo,' to ', maxClimo, '</p>
      <hr>
      <p>Date reached this year: ', targDate, '</p>
      <p>Prediction based on current observation + climo: ', fcstClimo, '</p>
      <p>Prediction difference from average:', fcstDiff, '</p>
      <p>Event date last year: : ', prevYr, '</p>
      </div>
      </div>')
  )
}
######


##### INITIAL VALS ----
# initial point for marker
latIn<-36
lonIn<--109
#####
# https://stackoverflow.com/questions/53370679/r-leaflet-how-to-obtain-lat-long-where-marker-has-been-dragged-to

# species list
# creating a data frame
sppTargets1 <- data.frame(
  sppName = c("Blazing stinkweed - Treat",
              "Itchy footmallow - Treat",
              "Western bearded cat mite - Emergence",
              "Deadly bedworm - Treat"), 
  gddStartDate = c("01-01",
                   "02-15",
                   "03-01",
                   "05-01"),
  baseT = c(32,
            40,
            50,
            45),
  targetGDD = c(1000,
                500,
                75,
                3000), 
  stringsAsFactors = FALSE
)
sppList<-sppTargets1$sppName
#####



# Define UI for application that draws a histogram
ui <- fluidPage(

  # css tags for progress bar
  tags$head(tags$style('.shiny-progress .progress-text {
                          position: absolute;
                          right: 10px;
                          height: 30px;
                          width: 300px;
                          background-color: #FF6633;
                          margin: 0px;
                          padding: 2px 3px;
                          opacity: 0.85;
                          font-size: 18px;
                        }
                       .card {
                         width: 100%;
                       clear: both;
                       /* Add shadows to create the "card" effect */
                       box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
                       transition: 0.3s;
                       }
                       /* On mouse-over, add a deeper shadow */
                       .card:hover {
                       box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
                       }
                       /* Add some padding inside the card container */
                       .container {
                       width: 100%;
                       padding: 2px 16px;
                       }'
                       )),
  
    # Application title
    titlePanel("NatureCast - Phenological Forecasting"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            leafletOutput("MyMap",width = "100%", height = "250px"),
            verbatimTextOutput("latSel"),
            verbatimTextOutput("lonSel"),
            pickerInput("spp","Species Event", choices=sppList, options = list(`actions-box` = TRUE),multiple = T, selected = sppList[1]),
            hr(),
            actionButton("refresh","Download data"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            uiOutput("cards")
          ) 
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # ##### Initial download
  # ##### download and process data for location ---- 
  # start.time <- Sys.time()
  # # set location 
  # lat=latIn
  # lon=lonIn 
  # 
  # #download daily PRISM 
  # endDate<-Sys.Date()-1
  # jsonQuery=paste0('{"loc":"',lon,',',lat,'","sdate":"1981-01-01","edate":"',endDate,'","grid":"21",
  #                                   "meta":"ll,elev","elems":[{"name":"pcpn","units":"in"},{"name":"mint","units":"degreeF"},{"name":"maxt","units":"degreeF"}]}')
  # 
  # outDaily<-postForm("http://data.rcc-acis.org/GridData",.opts = list(postfields = jsonQuery, 
  #                                                                     httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  # # json to datframe
  # outDaily<-fromJSON(outDaily)
  # dataDaily<-data.frame(outDaily$data)
  # #
  # 
  # # character to numeric conversion
  # data <- dataDaily[,2:4] %>% mutate_if(is.character, as.numeric)
  # # bind back together
  # dataDaily<-cbind.data.frame(as.Date(dataDaily$X1,"%Y-%m-%d"),data)
  # # set colnames
  # colnames(dataDaily)<-c("date","precip","minT","maxT")
  # # calculate daily average temperature
  # dataDaily$avgT<-(dataDaily$maxT+dataDaily$minT)/2
  # 
  # # add in date elements
  # dataDaily$month<-as.numeric(format(dataDaily$date, "%m"))
  # dataDaily$year<-as.numeric(format(dataDaily$date, "%Y"))
  # dataDaily$doy<-as.numeric(format(dataDaily$date, "%j"))
  # tempData<-dataDaily
  # #
  # end.time <- Sys.time()
  # end.time - start.time
  # #####
  
    #####
    # original map click get lat/lon
    # add in leaflet map, overlay PRISM avg precip map or DEM grid...
    output$MyMap <- renderLeaflet({
      m <- leaflet() %>% setView(lng = -100.77156166441775, lat = 35.967440759643765, zoom = 3)
      m %>% addProviderTiles("Esri.WorldTopoMap") 
      #%>% addMarkers(lonIn, latIn)
    })

    observeEvent(input$MyMap_click, {
      leafletProxy("MyMap")%>% clearMarkers() %>%
        addMarkers(input$MyMap_click$lng, input$MyMap_click$lat)
      latIn<-input$MyMap_click$lat
      lonIn<-input$MyMap_click$lng
      output$latSel<-renderText({paste("Latitude: ",round(latIn,2))})
      output$lonSel<-renderText({paste("Longitude: ",round(lonIn,2))})
    })
    #####
    
    ######
    # error catch on no map click with download button
      showModal(modalDialog(
        title = "Click on map to add location, select species of interest, and then click 'Download data'",
        easyClose = TRUE,
        footer = NULL
      ))
    #shiny::validate(need(!is.null(input$MyMap_click), message = "Click map to add location"))
    #shiny::validate(need(input$MyMap_click != '', message = "Click map to add location"))
    #req(input$MyMap_click)  
    #####
    
    
    
    observeEvent(input$refresh, {
      
      ######
      # error catch on no map click with download button
      if(is.null(input$MyMap_click))
        showModal(modalDialog(
          title = "Click on map to add location first, then click 'Set location'",
          easyClose = TRUE,
          footer = NULL
        ))
      #shiny::validate(need(!is.null(input$MyMap_click), message = "Click map to add location"))
      shiny::validate(need(input$MyMap_click != '', message = "Click map to add location"))
      #req(input$MyMap_click)  
      #####
      
      
      withProgress(message = 'Downloading data set', style="old",
                   detail = 'Please wait...',{
    
                     # set location 
                     lat=input$MyMap_click$lat # input from map
                     lon=input$MyMap_click$lng # input from map
                     
                     cat(lat)
                     cat(lon)
                     
                     #download daily PRISM 
                     endDate<-Sys.Date()-1
                     jsonQuery=paste0('{"loc":"',lon,',',lat,'","sdate":"1981-01-01","edate":"',endDate,'","grid":"21",
                                    "meta":"ll,elev","elems":[{"name":"pcpn","units":"in"},{"name":"mint","units":"degreeF"},{"name":"maxt","units":"degreeF"}]}')
                     
                     outDaily<-postForm("http://data.rcc-acis.org/GridData",.opts = list(postfields = jsonQuery, 
                                                                                         httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
                     # json to datframe
                     outDaily<-fromJSON(outDaily)
                     dataDaily<-data.frame(outDaily$data)
                     #
                     print("downloading, processing")
                     # character to numeric conversion
                     data <- dataDaily[,2:4] %>% mutate_if(is.character, as.numeric)
                     # bind back together
                     dataDaily<-cbind.data.frame(as.Date(dataDaily$X1,"%Y-%m-%d"),data)
                     # set colnames
                     colnames(dataDaily)<-c("date","precip","minT","maxT")
                     # calculate daily average temperature
                     dataDaily$avgT<-(dataDaily$maxT+dataDaily$minT)/2
                     
                     # add in date elements
                     dataDaily$month<-as.numeric(format(dataDaily$date, "%m"))
                     dataDaily$year<-as.numeric(format(dataDaily$date, "%Y"))
                     dataDaily$doy<-as.numeric(format(dataDaily$date, "%j"))
                     cat(mean(dataDaily$avgT))
                     tempData<-dataDaily
                     print("dailyData processed")
                     #
                   })
      
      #### add in processing list             
      #print(input$spp)
      sppTargets<-subset(sppTargets1, sppName %in% input$spp)
      #print(tempList)
      
      ##### PROCESS GDD DATA
      # results in list
      gddResults<-list()
      gddResultsDF<-list()
      # results in dataframe
      # gddResultsDF <- data.frame(sppName=character(),
      #                  avgDate=as.numeric(),
      #                  minDate=as.numeric(),
      #                  maxDate=as.numeric(),
      #                  fcstDate=as.numeric(),
      #                  prevYrDate=as.numeric(),
      #                  stringsAsFactors=FALSE) 
      
      
      ######### PROCESS GDD in loop based on sppTargets -------
      for(i in 1:nrow(sppTargets)){
        
        tempData<-dataDaily # work with copy of downloaded data
        
        # set external vars
        # startDate
        currYr<-as.numeric(format(Sys.Date(),"%Y"))
        startDate<-as.Date(paste0(format(Sys.Date(),"%Y"),"-",sppTargets$gddStartDate[i]))
        # base temperature
        baseT<-sppTargets$baseT[i]
        # target GDD
        targetGDD<-sppTargets$targetGDD[i]
        
        # extract date vars 
        startMo<-as.numeric(format(startDate,"%m"))
        startDayMo<-as.numeric(format(startDate,"%d"))
        startYr<-as.numeric(format(startDate,"%Y"))
        
        # create adjusted date like water year doy/year
        adjDates<-adj_date(tempData$date, startMo, startDayMo)
        tempData$adjDOY<-adjDates$adjDOY
        tempData$adjYear<-adjDates$adjYear
        
        # drop leap year excess days
        tempData<-subset(tempData, adjDOY!=0)
        
        # calc day avgT with base temp for gdd
        tempData$avgT_base <- unlist(lapply(tempData$avgT, function(x) ifelse(x>=baseT, x-baseT, 0)))
        
        # cumulative doy climate - precip and temp
        cumClim <- tempData %>% 
          group_by(adjYear, adjDOY) %>% # still doesn't quite work adjYear kicks off before adjDOY
          summarise(precip = sum(precip, na.rm = T),
                    temp = sum(avgT_base, na.rm = T)) %>%
          mutate(psum = cumsum(precip),
                 tsum = cumsum(temp))
        tempData$cumPrecip<-cumClim$psum
        tempData$gdd<-cumClim$tsum
        #####
        
        ##### DEVELOP DAILY CLIMO #####
        
        tempClimo<-subset(tempData, adjYear>=1991 & adjYear<=2020)
        
        doyClimo<-tempClimo %>% group_by(adjDOY) %>%
          summarise(min=min(gdd),
                    max=max(gdd),
                    median=median(gdd),
                    mean=mean(gdd))
        doyClimo$date<-doyClimo$adjDOY+(startDate-1)
        
        #####
        

        ##### PLOT GDD
        climoLong<-tidyr::pivot_longer(data = doyClimo, cols = "min":"mean", names_to = "vars", values_to = "values")
        currData<-subset(tempData, date>=startDate)
        # fill currData with NA if empty
        if(nrow(currData)==0){
          currData[1,]<-NA
        }else{}
        
        # climo-based forecast
        bias<-currData$gdd[nrow(currData)]-doyClimo$mean[nrow(currData)]
        climoFcst<-doyClimo[nrow(currData):nrow(doyClimo),]
        bias<-ifelse(length(bias)==0,NA,bias) 
        climoFcst$mean<-climoFcst$mean+bias
        
        # targetGDD dates
        targClimoTxt<-paste0("Mean: ",as.Date(ifelse(which.max(doyClimo$mean>=targetGDD)==1,NA,
                                                     doyClimo$date[which.max(doyClimo$mean>=targetGDD)])))
        targDateTxt<-paste0("This year: ",as.Date(ifelse(which.max(currData$gdd>=targetGDD)==1,NA,
                                                         currData$date[which.max(currData$gdd>=targetGDD)])))
        targFcstTxt<-paste0("Climo Fcst: ",as.Date(ifelse(which.max(climoFcst$mean>=targetGDD)==1,NA,
                                                          climoFcst$date[which.max(climoFcst$mean>=targetGDD)])))
        
        # raw target date nums
        targClimoNum<-as.numeric(ifelse(which.max(doyClimo$mean>=targetGDD)==1,NA,
                                        doyClimo$date[which.max(doyClimo$mean>=targetGDD)]))
        targDateNum<-as.numeric(ifelse(which.max(currData$gdd>=targetGDD)==1,NA,
                                       currData$date[which.max(currData$gdd>=targetGDD)]))
        targDateNum<-ifelse(length(targDateNum)==0,NA,targDateNum)
        targFcstNum<-as.numeric(ifelse(which.max(climoFcst$mean>=targetGDD)==1,NA,
                                       climoFcst$date[which.max(climoFcst$mean>=targetGDD)]))
        targFcstNum<-ifelse(length(targFcstNum)==0,NA,targFcstNum)
        # range of dates
        targMaxClimoNum<-as.numeric(ifelse(which.max(doyClimo$min>=targetGDD)==1,NA,
                                           doyClimo$date[which.max(doyClimo$min>=targetGDD)]))
        targMinClimoNum<-as.numeric(ifelse(which.max(doyClimo$max>=targetGDD)==1,NA,
                                           doyClimo$date[which.max(doyClimo$max>=targetGDD)]))
        # difference from previous year
        prevYrData<-subset(tempData, date>=(startDate-365))
        #prevYrData<-prevYrData[1:365,]
        targDatePrevNum<-as.numeric(ifelse(which.max(prevYrData$gdd>=targetGDD)==1,NA,
                                           prevYrData$date[which.max(prevYrData$gdd>=targetGDD)]))
        
        # render gdd plot
        # p1<-plotly::ggplotly(ggplot(doyClimo, aes(date,mean))+
        #                        geom_line()+
        #                        geom_ribbon(aes(x=date, ymin=min,ymax=max), fill = "grey70", alpha=0.3)+
        #                        geom_line(data=currData, aes(date, gdd), color="red")+
        #                        geom_line(data=climoFcst, aes(date,mean), color="green")+
        #                        # geom_vline(xintercept = (ifelse(which.max(doyClimo$mean>=targetGDD)==1,NA,
        #                        #                                doyClimo$date[which.max(doyClimo$mean>=targetGDD)])),
        #                        #            linetype = "dashed", color="black")+
        #                        # geom_vline(xintercept = (ifelse(which.max(currData$gdd>=targetGDD)==1,NA,
        #                        #                                currData$date[which.max(currData$gdd>=targetGDD)])),
        #                        #            linetype = "solid", color="red")+
        #                        geom_vline(aes(xintercept = targClimoNum, text =targClimoTxt),
        #                                   linetype = "dashed", color="black")+
        #                        geom_vline(aes(xintercept = targDateNum, text =targDateTxt),
        #                                   linetype = "solid", color="red")+
        #                        geom_vline(aes(xintercept = targFcstNum, text =targFcstTxt),
        #                                   linetype = "solid", color="green")+
        #                        ylab("Cumulative GDD")+
        #                        theme_bw()+
        #                        #ggtitle(paste0("GDD through: ", currData$date[nrow(currData)], " (at ",latIn,",",lonIn,")"))
        #                        ggtitle(paste0("GDD through: ", currData$date[nrow(currData)]))
        # )
        #####
        
        # formatted results
        # gddResults[[i]]<-paste0(sppTargets$sppName[i],"\n",
        #                         "Average date: ",format(as.Date(targClimoNum),"%b-%d"),"\n",
        #                         "Range in 1991-2020 period: ",format(as.Date(targMinClimoNum),"%b-%d")," to ",format(as.Date(targMaxClimoNum),"%b-%d"), "\n",
        #                         "Prediction based on current observation + climo: ",format(as.Date(targFcstNum),"%b-%d-%Y"),"\n",
        #                         "Prediction difference from average: ",(targFcstNum)-(targClimoNum),"\n",
        #                         "Threshold date last year: ",format(as.Date(targDatePrevNum),"%b-%d-%Y"))
        # # different formatting
        gddResults[[i]]<-list(sppTargets$sppName[i],
                              paste0("Average date: ",format(as.Date(targClimoNum),"%b-%d")),
                              paste0("Range in 1991-2020 period: ",format(as.Date(targMinClimoNum),"%b-%d")," to ",format(as.Date(targMaxClimoNum),"%b-%d")), 
                              paste0("Prediction based on current observation + climo: ",format(as.Date(targFcstNum),"%b-%d-%Y")),
                              paste0("Prediction difference from average: ",(targFcstNum)-(targClimoNum)),
                              paste0("Threshold date last year: ",format(as.Date(targDatePrevNum),"%b-%d-%Y")))
        

        # results in df
        gddResultsDF[[i]]<-cbind.data.frame(sppTargets$sppName[i],targClimoNum,targMinClimoNum,targMaxClimoNum,targDateNum,targFcstNum,targDatePrevNum)
        
      }
      
      # results list to df
      gddResultsDF<-do.call(rbind,gddResultsDF)
      #####
      
      
      # create cards based on selection
      output$cards <- renderUI({
        # First make the cards
        args <- lapply(1:nrow(gddResultsDF), function(x) card(
          title = gddResultsDF$`sppTargets$sppName[i]`[x],
          targClimo = format(as.Date(gddResultsDF$targClimoNum[x]),"%b-%d"),
          minClimo = format(as.Date(gddResultsDF$targMinClimoNum[x]),"%b-%d"),
          maxClimo = format(as.Date(gddResultsDF$targMaxClimoNum[x]),"%b-%d"),
          targDate = format(as.Date(gddResultsDF$targDateNum[x]),"%b-%d-%Y"),
          fcstClimo = format(as.Date(gddResultsDF$targFcstNum[x]),"%b-%d-%Y"),
          fcstDiff = (gddResultsDF$targFcstNum[x])-(gddResultsDF$targClimoNum[x]),
          prevYr = format(as.Date(gddResultsDF$targDatePrevNum[x]),"%b-%d-%Y")
          ))
        
        # Make sure to add other arguments to the list:
        args$cellArgs <- list(
          style = "
        width: auto;
        height: auto;
        margin: 5px;
        ")
        # basically the same as flowLayout(cards[[1]], cards[[2]],...)
        do.call(shiny::flowLayout, args)
      })
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
