
# Import Library--------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(shinyjs)
library(shinyWidgets)
library(scales)
library(ggplot2)
library(data.table)
library(plotly)
library(rgdal)
library(maptools)
library(shinydashboard)

#read Dataset------------------------------------------------------------------

degrees.salaries <- read.csv("degrees-that-pay-back.csv",header=TRUE, colClasses="character" )
college.salaries <- read.csv("salaries-by-college-type.csv",header=TRUE,colClasses="character")
region.salaries <- read.csv("salaries-by-region.csv",header=TRUE,colClasses="character")

#Data Preparation--------------------------------------------------------------

##Join region.salaries together with college.salaries-------------------------
college <- college.salaries[c("School.Type","School.Name")]
data <- merge(region.salaries,college,by="School.Name", all=TRUE)

##Arrange Column--------------------------------------------------------------
data <- data[c(1,2,9,3,5,6,4,7,8)]
data[c("School.Type")][is.na(data[c("School.Type")])] <- "Unknown"
write.csv(data, "data.csv" )

##Convert college.salaries into numeric----------------------------------------
dim(data)
col2cvt <- 4:9
data[,col2cvt] <- lapply(data[,col2cvt],
                         function(data)
                         {as.numeric(gsub("\\$","",gsub(",", "", data)))})
str(college.salaries)

##convert degrees.salaries into numeric---------------------------------------
dim(degrees.salaries)
col2cvt <- 2:8
degrees.salaries[,col2cvt] <- lapply(degrees.salaries[,col2cvt],
                                     function(degrees.salaries)
                                     {as.numeric(gsub("\\$","",gsub(",", "", degrees.salaries)))})

str(degrees.salaries)


##rename column for data--------------------------------------------------------

names(data)[names(data) == "Starting.Median.Salary"] <- "start_p50"
names(data)[names(data) == "Mid.Career.Median.Salary"] <- "mid_p50"
names(data)[names(data) == "Mid.Career.10th.Percentile.Salary"] <- "mid_p10"
names(data)[names(data) == "Mid.Career.25th.Percentile.Salary"] <- "mid_p25"
names(data)[names(data) == "Mid.Career.75th.Percentile.Salary"] <- "mid_p75"
names(data)[names(data) == "Mid.Career.90th.Percentile.Salary"] <- "mid_p90"

##rename column for degrees.salaries -------------------------------------------
names(degrees.salaries)[names(degrees.salaries) == "Starting.Median.Salary"] <- "start_p50"
names(degrees.salaries)[names(degrees.salaries) == "Mid.Career.Median.Salary"] <- "mid_p50"
names(degrees.salaries)[names(degrees.salaries) == "Mid.Career.10th.Percentile.Salary"] <- "mid_p10"
names(degrees.salaries)[names(degrees.salaries) == "Mid.Career.25th.Percentile.Salary"] <- "mid_p25"
names(degrees.salaries)[names(degrees.salaries) == "Mid.Career.75th.Percentile.Salary"] <- "mid_p75"
names(degrees.salaries)[names(degrees.salaries) == "Mid.Career.90th.Percentile.Salary"] <- "mid_p90"
names(degrees.salaries)[names(degrees.salaries) == "Percent.change.from.Starting.to.Mid.Career.Salary"] <- "increase"


## Arrange and tidy --------------------------------------------------------------------

data <- arrange(data,-start_p50)
head(data)

tidy.data<-gather(data, Stage, Salary, 4:9)
head(tidy.data)
dim(tidy.data)


#degrees.salaries <- degrees.salaries[c(1,4,3,2,5:8)]
tidy.degree.salaries<-gather(degrees.salaries, Stage, Salary, 2:8)
head(tidy.degree.salaries)
dim(tidy.degree.salaries)
tidy.degree.salaries <- tidy.degree.salaries[order(tidy.degree.salaries$Salary),]



##Fill in missing value for data ----------------------------------------------
data <- data[-which(rowSums(is.na(data[,4:9])) >= 3),]

data$mid_p10[is.na(data$mid_p10)] <- subset(data, is.na(data$mid_p10))$start_p50 +((subset(data, is.na(data$mid_p10))$mid_p25 - subset(data, is.na(data$mid_p10))$start_p50)/2)
data$mid_p90[is.na(data$mid_p90)] <- subset(data, is.na(data$mid_p90))$mid_p75 +(subset(data, is.na(data$mid_p90))$mid_p75 - subset(data, is.na(data$mid_p90))$mid_p50)


#data <- data[, -which(rowMeans(is.na(data)) > 0.5)]
#data$mid_p10[is.na(data$mid_p10)] <- data$start_p50 +((data$mid_p25 - data$start_p50)/2)
#data$mid_p90[is.na(data$mid_p90)] <- data$mid_p75 +(data$mid_p75 - data$mid_p50)

#data<-na.omit(data)

##Finding minimum and maximum of salary for data--------------------------------
min.start<-min(data[,4], na.rm=T)
max.start<-max(data[,4], na.rm=T)
min.Mid<-min(data[,7], na.rm=T)
max.Mid<-max(data[,7], na.rm=T)


summary(data)

#---------------------------------------------------------------------------------
# Data Cleaning and Preprocessing

degree_data <- read.csv('degrees-that-pay-back.csv',header = TRUE)
degree_data <- apply(degree_data, 2, function(x) { x <- gsub("\\$", "",x) })
degree_data <- apply(degree_data, 2, function(x) { x <- gsub("\\,", "",x) })
degree_data <- apply(degree_data, 2, function(x) { x <- gsub("\\N/A", NA,x) })
degree_data <- apply(degree_data, 2, function(x) { x <- gsub("\\.00", "",x) })
degree_data <- as.data.frame(degree_data,stringsAsFactors=FALSE)

for(i in 2:ncol(degree_data)){
  degree_data[is.na(degree_data[,i]), i] <- mean(as.numeric(degree_data[,i]), na.rm = TRUE)
}

for(i in 2:ncol(degree_data)){
  degree_data[, i] <- as.numeric(degree_data[, i])
}

degree_data$Starting.Median.Salary = NULL
degree_data$Percent.change.from.Starting.to.Mid.Career.Salary = NULL
names(degree_data) <- gsub(x = names(degree_data), pattern = "\\.",replacement = " ")
names(degree_data)[2] <- 'Mid Career 50th Percentile Salary'
degree_data_means <- aggregate(degree_data[, 2:ncol(degree_data)], list(degree_data$`Undergraduate Major`), mean)
degree_data_means <- melt(degree_data_means, "Group.1")

region_data <- read.csv('region_data.csv',header = TRUE)
markers <- data.frame(lon = region_data$lon, lat = region_data$lat)
region_data <- apply(region_data, 2, function(x) { x <- gsub("\\$", "",x) })
region_data <- apply(region_data, 2, function(x) { x <- gsub("\\,", "",x) })
region_data <- apply(region_data, 2, function(x) { x <- gsub("\\N/A", NA,x) })
region_data <- apply(region_data, 2, function(x) { x <- gsub("\\.00", "",x) })
region_data <- as.data.frame(region_data,stringsAsFactors=FALSE)

for(i in 3:8){
  region_data[is.na(region_data[,i]), i] <- mean(as.numeric(region_data[,i]), na.rm = TRUE)
}

for(i in 3:ncol(region_data)){
  region_data[, i] <- as.numeric(region_data[, i])
}

region_data$Starting.Median.Salary = NULL
names(region_data) <- gsub(x = names(region_data), pattern = "\\.",replacement = " ")
names(region_data)[3] <- 'Mid Career 50th Percentile Salary'
region_data_means <- aggregate(region_data[, 3:7], list(region_data$Region), mean)
head(region_data_means)
region_data_means <- melt(region_data_means, "Group.1")

college_data <- read.csv('salaries-by-college-type.csv',header = TRUE)
college_data <- apply(college_data, 2, function(x) { x <- gsub("\\$", "",x) })
college_data <- apply(college_data, 2, function(x) { x <- gsub("\\,", "",x) })
college_data <- apply(college_data, 2, function(x) { x <- gsub("\\N/A", NA,x) })
college_data <- apply(college_data, 2, function(x) { x <- gsub("\\.00", "",x) })
college_data <- as.data.frame(college_data,stringsAsFactors=FALSE)

for(i in 3:ncol(college_data)){
  college_data[is.na(college_data[,i]), i] <- mean(as.numeric(college_data[,i]), na.rm = TRUE)
}

for(i in 3:ncol(college_data)){
  college_data[, i] <- as.numeric(college_data[, i])
}
college_data$Starting.Median.Salary = NULL
names(college_data) <- gsub(x = names(college_data), pattern = "\\.",replacement = " ")
names(college_data)[3] <- 'Mid Career 50th Percentile Salary'

college_data_means <- aggregate(college_data[, 3:ncol(college_data)], list(college_data$`School Type`), mean)

head(college_data_means)
college_data_means <- melt(college_data_means, "Group.1")

xcol <- c('College type','region','degrees')
a <- unique(college_data$`School Type`)
b <- unique(region_data$Region)
c <- unique(degree_data$`Undergraduate Major`)
mean_c <- college_data %>% 
  mutate(mean = rowMeans(college_data[,3:7]))
mean_r <- region_data %>% 
  mutate(mean = rowMeans(region_data[,3:7]))
mean_d <- degree_data %>% 
  mutate(mean = rowMeans(degree_data[,2:6]))
max <- max(c(max(mean_c$mean),max(mean_r$mean),max(mean_d$mean)))

options <- list(colleges = a, regions = b, degrees = c)
optionname <- 'College type'
option <- options$colleges
percentile_opt <- c('Mid Career 10th Percentile Salary','Mid Career 25th Percentile Salary',
                    'Mid Career 50th Percentile Salary','Mid Career 75th Percentile Salary',
                    'Mid Career 90th Percentile Salary')
states <- readOGR("states-regions/states.shp",layer = "states", GDAL1_integer64_policy = TRUE)

#------------------------------------------------------------------------------
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(dashboardPage(
  dashboardHeader(title = "University Choice Maker",titleWidth=350),
  dashboardSidebar(
    sidebarMenu(id="tabs",
                menuItem("Interactive map", tabName="plot1", icon=icon("globe"), selected=TRUE),
                menuItem("Comparison Plot", tabName = "plot2", icon=icon("line-chart")),
                menuItem("Box Plot", tabName = "plot3", icon=icon("bar-chart")),
                menuItem("Point Plot", tabName = "plot4", icon=icon("line-chart")),
                menuItem("Data Table", tabName = "table5", icon=icon("table")),
                menuItem("Documentation", tabName = "doc", icon=icon("book"))
                
    )
  ),
  
  dashboardBody(
    tags$head(
      
      includeCSS("style.css")
    ),
    
    
    tabItems(
      # First tab content
      tabItem(tabName = "plot1",
              div(class="outer",
                  
                  
                  leafletOutput("mymap", width="100%", height="100%"),
                  
                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,top = 100, left = "auto", right = 20, bottom = "auto",
                                width = 330, height = "auto",
                                
                                h3("Filter"),
                                
                                pickerInput(
                                  inputId = "region", 
                                  label = "Choose region", 
                                  selected = options$region,
                                  choices = options$region, 
                                  options = list(
                                    `actions-box` = TRUE,
                                    `selected-text-format` = "count > 1"
                                  ), 
                                  multiple = TRUE
                                ),
                                
                                sliderInput("sal1","Mean salaries range", min = 0, 
                                            max = max(mean_r$mean), value = c(0, max(mean_r$mean))),
                                
                                checkboxInput("cluster", "Show cluster", FALSE)
                                
                                
                  )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "plot2",
              fluidPage(
                
                titlePanel("Comparison Plot"),
                sidebarLayout(
                  
                  conditionalPanel(
                    condition = NULL,
                    sidebarPanel(
                      
                      selectInput('xcol', 'X Variable', xcol),
                      pickerInput(
                        inputId = "myPicker", 
                        label = "", 
                        selected = NULL,
                        choices = NULL, 
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 3"
                        ), 
                        multiple = TRUE
                      ),
                      pickerInput(
                        inputId = "percentile", 
                        label = "Choose percentiles", 
                        selected = percentile_opt,
                        choices = percentile_opt, 
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 1"
                        ), 
                        multiple = TRUE
                      ),
                      checkboxInput("line", "Lines", FALSE),
                      sliderInput("sal","Mean salaries range", min = 0, 
                                  max = max, value = c(0, max))#,
                      #actionButton("hide", "Hide Sidebar")
                    )),
                  mainPanel(uiOutput("uiplot"))
                  
                )
              )
      ),
      

      # Third tab content
      tabItem(tabName = "plot3",
              fluidPage(

                titlePanel("Box Plot"),
                sidebarLayout(
                  conditionalPanel(
                    condition = NULL,
                    sidebarPanel(
                      width = 3,
                      radioButtons("Region2",
                                   "University Region:",
                                   c("All",
                                     unique(as.character(data$Region)))),
                      radioButtons("sch.typ2",
                                   "School Type:",
                                   c("All",
                                     unique(as.character(data$School.Type)))),
                      selectInput("sch.name2",
                                  "School Name:",
                                  c("All",
                                    unique(as.character(data$School.Name))))
                    )),
                  #mainPanel(uiOutput("uiplot2"))
                  mainPanel(plotOutput(outputId = "boxplot"))
                )
              )
      ),

      #Fourth tab content
      tabItem(tabName = "plot4",
              fluidPage(

                titlePanel("Box Plot"),
                sidebarLayout(
                  conditionalPanel(
                    condition = NULL,
                    sidebarPanel(
                      width = 2,
                      radioButtons(inputId = "Stage",
                                   label = "Stage:",
                                   c(unique(as.character(tidy.degree.salaries$Stage))))
                    )),
                  #mainPanel(uiOutput("uiplot2"))
                  mainPanel(plotOutput(outputId = "box"))
                )
              )
      ),
      
      # Fifth tab content
      tabItem(tabName = "table5",
              fluidPage(
                
                titlePanel("Data Table"),
                sidebarLayout(
                  conditionalPanel(
                    condition = NULL,
                    sidebarPanel(
                      width = 3,
                      radioButtons("Region",
                                   "University Region:",
                                   c("All",
                                   unique(as.character(data$Region)))),
                      radioButtons("sch.typ",
                                   "School Type:",
                                   c("All",
                                     unique(as.character(data$School.Type)))),
                      selectInput("sch.name",
                                  "School Name:",
                                  c("All",
                                    unique(as.character(data$School.Name))))
                    )),
                  mainPanel(tableOutput("tbl"))
                  
                )
              )
      ),
      
    
      
      # fifth tab content
      tabItem(tabName = "doc",
              fluidPage(
                h1("University & Major"),
                h3("Introduction"),
                h4("Nowadays, there are so many university choices that making decision is hard especially at a very young age. Students have to do extra work to research for the best university and even if they did, sometimes the informations gathered are still not enough to make the right decision. A lot of students regretted their choices after they enrolled in the university. Therefore we wanted to solve this problem by creating a shiny app so that prospect student with the help of analytics can make a right decision that will determine their next 5 years of student life."),
                h4("There are 5 pages in this shiny application:"),
                h3("1. Interactive Map"),
                h4("This page shows a map of United States. Across this map there would be markers that represent each university location. Student can use the filter available on the right hand side to choose which region they prefer, or which mean salary they wanted to see.When hover, the marker will show salary information for the specific university. The intensity of the marker's color signify the mean salary."),
                h3("2.Comparison Plot"),
                h4("This page shows a plot where user can choose to compare salary stages based on region, degrees, and college type. After selection, user can also choose to select one or multiple region, degrees and college type to compare with each other. Furthermore there is 'Choose percentile' option that enables the user to only compare the mid 10th,25th,50th,75th and 90th percentile salary. If the user only interested to know based on a mean salaries range, there is a slider bar to filter out the salary range."),
                h3("3.Box Plot"),
                h4("Box Plot that shows the variation of salaries by University Region, School Type or School Name. "),
                h3("4.Point Plot"),
                h4("Point Plot that shows the comparison of salary by Stage for every Undergraduate Major."),
                h3("5.Data Table"),
                h4("The data table will show all universities record and the salary information after graduation. Student can use the filter available on the left hand side to choose the region, school type, or salary range to show all the universities with the specified properties. By using the table, student will have a better look on all the choices that they have.")
              )
      )
    )
    
  )
)
)




#-----------------------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output, session) { 
  
#--------------------------------------------------------------  
  output$mymap <-  renderLeaflet({
  
  if(length(input$sal1) > 0){
    markers = markers[which(rowMeans(region_data[,3:7]) >= input$sal1[1] & rowMeans(region_data[,3:7]) <= input$sal1[2]),]
    
  }
  
  if(length(input$region) > 0){
    states = subset(states, region %in% input$region)
    markers = markers[which(region_data$Region %in% input$region),]
  }
  
  clusteroption = reactive({
    if(input$cluster){
      cluster = markerClusterOptions()
    }else{
      cluster = NULL
    }
    cluster
  })
  
  marker_label <- paste(sep = "<br/>",
                        paste(sep = "", "<b style=\"font-size:14px\">",region_data$`School Name`,"</b>"),
                        paste(sep = ": $", "<b>10th Percentile Salary</b>", region_data$`Mid Career 10th Percentile Salary`),
                        paste(sep = ": $", "<b>25th Percentile Salary</b>", region_data$`Mid Career 25th Percentile Salary`),
                        paste(sep = ": $", "<b>50th Percentile Salary</b>", region_data$`Mid Career 50th Percentile Salary`),
                        paste(sep = ": $", "<b>75th Percentile Salary</b>", region_data$`Mid Career 75th Percentile Salary`),
                        paste(sep = ": $", "<b>90th Percentile Salary</b>", region_data$`Mid Career 90th Percentile Salary`)
  )
  
  getColor <- function(df) {
    sapply(df$mean, function(x) {
      if(x <= 81440) {
        "white"
      } else if(x <= 91330) {
        "lightgreen"
      } else if(x <= 102425){
        "green"
      }else{
        "darkgreen"
      } })
  }
  
  icons <- awesomeIcons(
    icon = 'fa-graduation-cap',
    iconColor = 'black',
    library = 'fa',
    markerColor = getColor(mean_r)
  )
  
  
  leaflet(states) %>%
    addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~colorFactor("YlOrRd", region)(region),
                highlightOptions = highlightOptions(color = "white", weight = 1,bringToFront = TRUE)) %>%
    addTiles() %>%
    addAwesomeMarkers(markers$lon,markers$lat,icon = icons,popup = marker_label,
                      popupOptions = popupOptions(closeButton = FALSE, closeOnClick = TRUE,className = "popup"),clusterOptions = clusteroption())
  
})

outVar = reactive({
  if(input$xcol == xcol[1]){
    choices = options$colleges
  }else if(input$xcol == xcol[2]){
    choices = options$regions
  }else{
    choices = options$degrees
  }
  choices
})


outLab = reactive({
  if(input$xcol == xcol[1]){
    label = names(options[1])
  }else if(input$xcol == xcol[2]){
    label = names(options[2])
  }else{
    label = names(options[3])
  }
  label
})



observe({
  updatePickerInput(session, "myPicker",label = paste('Choose',outLab(),sep=' '), selected = as.character(outVar()),choices = as.character(outVar()))
  
})

height = reactive({
  if(input$xcol == xcol[1] || input$xcol == xcol[2]){
    h = 600
  }else{
    h = 900
  }
  h
})


#---------------------------------------------------------
output$tbl <- renderTable({
      data <- data
      data <- data[rowSums(is.na(data))!=ncol(data),]

      if (input$Region!= "All"){
        data <- data[data$Region == input$Region,]
        data <- data[rowSums(is.na(data))!=ncol(data),]
      }
      if (input$sch.typ != "All"){
        data <- data[data$School.Type == input$sch.typ,]
        data <- data[rowSums(is.na(data))!=ncol(data),]
      }
      if (input$sch.name != "All"){
        data <- data[data$School.Name == input$sch.name,]
        data <- data[rowSums(is.na(data))!=ncol(data),]
      }
      data})


#----------------------------------------------------------
output$uiplot <- renderUI({
  
  
  width = function(){
    # if(input$hide){
    #   w = 12
    # }else{
    #   w = 8
    # }
    # 
    # if(input$show){
    #   w = 8
    # }else{
    #   w = 12
    # }
    # w
    #8
  }
  
  mainPanel(
    wellPanel(
      plotOutput("plot",height = height())
      
    ),
    width = width()
  )
  
  
})


#---------------------------------------------------------------------
output$plot <- renderPlot({

  y = 'Salaries'

  if(input$xcol == xcol[1]){
    ini_data = college_data
    data = college_data_means
    x = 'School Type'
    ind = 3

  }else if(input$xcol == xcol[2]){
    ini_data = region_data
    data = region_data_means
    x = 'Region'
    ind =3

  }else{
    ini_data = degree_data
    data = degree_data_means
    x = 'Undergraduate Major'
    ind = 2

  }


  if(length(input$sal) > 0){
    ini_data = subset(ini_data, rowMeans(ini_data[,ind:ncol(ini_data)]) >= input$sal[1] & rowMeans(ini_data[,ind:ncol(ini_data)]) <= input$sal[2])
    if(x == 'School Type'){
      data = aggregate(ini_data[, ind:ncol(ini_data)], list(ini_data$`School Type`), mean)
    }else if(x == 'Region'){
      data = aggregate(ini_data[, ind:ncol(ini_data)], list(ini_data$Region), mean)
    }else{
      data = aggregate(ini_data[, ind:ncol(ini_data)], list(ini_data$`Undergraduate Major`), mean)
    }

    data = melt(data, "Group.1")
  }

  if(length(input$myPicker) > 0){
    data = subset(data, Group.1 %in% input$myPicker)
  }

  if(length(input$percentile) > 0){
    data = subset(data, variable %in% input$percentile)
  }



  p = ggplot(data, aes(x=Group.1, y=value,group = variable,color=variable)) +
    labs(title = "Mid Career salary comparison for each percentile",x=x,y=y) +
    geom_point(size = 6) +
    theme(legend.position="bottom",
          panel.background = element_rect(fill="white",color="grey"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
          plot.margin=unit(c(1,1,1,1),"cm")) +
    scale_color_brewer(palette = "Spectral")


  if(input$line){
    if(input$xcol == xcol[3]){
      p + geom_line() + coord_flip()
    }else{
      p+geom_line()
    }

  }else{
    if(input$xcol == xcol[3]){
      p + coord_flip()
    }else{
      p
    }


  }

})



#-----------------------------------------------------------------------------

output$box <- renderPlot({
    selected <- input$Stage
    
    
     dd <- tidy.degree.salaries[tidy.degree.salaries$Stage == input$Stage,]
     dd$Undergraduate.Major <- factor(dd$Undergraduate.Major, levels = dd$Undergraduate.Major[order(dd$Salary)])
     ggplot(data=dd, aes(y=Undergraduate.Major,x= Salary)) +
        geom_boxplot() + 
        geom_point(aes(col = "pink", size = 3)) +
        xlab("Salary") +
        ylab("Undergraduate Major") +
        theme(legend.position = "none", 
              text = element_text(size = 15))
    },
    height = 700,
    width = 1200)

#}
#-----------------------------------------------------------------------------
output$boxplot<-renderPlot({ 
  
  if (input$Region2!= "All"){
    
    tidy.data <- tidy.data[tidy.data$Region == input$Region2,]
    tidy.data <- tidy.data[rowSums(is.na(data))!=ncol(data),]
  }
  if (input$sch.typ2 != "All"){
    tidy.data <- tidy.data[tidy.data$School.Type == input$sch.typ2,]
    tidy.data <- tidy.data[rowSums(is.na(tidy.data))!=ncol(tidy.data),]
  }
  if (input$sch.name2 != "All"){
    tidy.data <- tidy.degree.salaries[tidy.degree.salaries$School.Name == input$sch.name2,]
    tidy.data <- tidy.degree.salaries[rowSums(is.na(tidy.degree.salaries))!=ncol(tidy.degree.salaries),]
  }
  

  ggplot(data=tidy.data, aes(y=Salary,x= Stage)) +
    geom_boxplot()+
    geom_point(color="green") +
    xlab("Stage") +
    ylab("Salary Range")},
    height = 700,
    width =1200)

#})
}
#----------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

