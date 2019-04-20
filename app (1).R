#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(wbstats)
library(corrplot)


# Define UI for application that draws a histogram
ui=
    navbarPage("Shiny App",
               tabPanel("Countries",
  # Application title
  titlePanel("Statistical G-20 Analysis"),
  
  # Sidebar with a slider input for number of bins 

  sidebarPanel(
    selectInput("state", "Choose a Parameter:",list("GDP","GDPgrowth","perCapita","lifeExpectancy")),
    sliderInput("bins",
                "Choose Start Years:",
                min = 1960,
                max = 2016,
                value = 1960),
    sliderInput("bin1",
                "Choose End Years:",
                min = 1960,
                max = 2016,
                value = 2016),
    
  
    
    selectInput("state1", "Choose a Country:",
                list("Australia","Argentina","Brazil","Canada","China","European_Union","France","Germany","India","Italy","Indonesia","Japan","Mexico","Russia","Saudi_Arabia","South_Africa","South_Korea","Turkey","Great_Britain","US")
    ),
 
  selectInput("state2", "Choose TimeLine :",
              list("Previous Data(1960-2016)","Future Data(2017-2021)")
  ),
  radioButtons("var1",label="Select File Type",choices=list("png","pdf")),

  downloadButton(
    outputId = "down",label="Download This Plot")
),
    mainPanel(
    plotOutput("plot2")
   
    
)
),
tabPanel("Comaprison",
         sidebarPanel(
         selectInput("state5", "Choose a Country:",
                     list("Australia","Argentina","Brazil","Canada","China","European_Union","France","Germany","India","Italy","Indonesia","Japan","Mexico","Russia","Saudi_Arabia","South_Africa","South_Korea","Turkey","Great_Britain","US")
         ),selectInput("state6", "Choose a Country:",
                       list("Australia","Argentina","Brazil","Canada","China","European_Union","France","Germany","India","Italy","Indonesia","Japan","Mexico","Russia","Saudi_Arabia","South_Africa","South_Korea","Turkey","Great_Britain","US")
         ),
         selectInput("state7", "Choose a Country:",
                     list("Australia","Argentina","Brazil","Canada","China","European_Union","France","Germany","India","Italy","Indonesia","Japan","Mexico","Russia","Saudi_Arabia","South_Africa","South_Korea","Turkey","Great_Britain","US")
         ),
         selectInput("state8", "Choose a Country:",
                     list("Australia","Argentina","Brazil","Canada","China","European_Union","France","Germany","India","Italy","Indonesia","Japan","Mexico","Russia","Saudi_Arabia","South_Africa","South_Korea","Turkey","Great_Britain","US")
         ),
         selectInput("state8", "Choose a Country:",
                     list("Australia","Argentina","Brazil","Canada","China","European_Union","France","Germany","India","Italy","Indonesia","Japan","Mexico","Russia","Saudi_Arabia","South_Africa","South_Korea","Turkey","Great_Britain","US")
         ),
         selectInput("state9", "Choose a Parameter:",list("GDP","GDPgrowth","perCapita","lifeExpectancy"))
         
         ),
         mainPanel(
           plotOutput("plot3")
         )
         
         ),
tabPanel("Corelation",
          sidebarPanel(
           selectInput("state31", "Choose a Country:",
                       list("Australia","Argentina","Brazil","Canada","China","European_Union","France","Germany","India","Italy","Indonesia","Japan","Mexico","Russia","Saudi_Arabia","South_Africa","South_Korea","Turkey","Great_Britain","US")
           ),selectInput("state32", "Choose a Country:",
                         list("Australia","Argentina","Brazil","Canada","China","European_Union","France","Germany","India","Italy","Indonesia","Japan","Mexico","Russia","Saudi_Arabia","South_Africa","South_Korea","Turkey","Great_Britain","US")
           ),
           selectInput("state33", "Choose a Country:",
                       list("Australia","Argentina","Brazil","Canada","China","European_Union","France","Germany","India","Italy","Indonesia","Japan","Mexico","Russia","Saudi_Arabia","South_Africa","South_Korea","Turkey","Great_Britain","US")
           ),
           selectInput("state34", "Choose a Country:",
                       list("Australia","Argentina","Brazil","Canada","China","European_Union","France","Germany","India","Italy","Indonesia","Japan","Mexico","Russia","Saudi_Arabia","South_Africa","South_Korea","Turkey","Great_Britain","US")
           ),
           selectInput("state36", "Choose a Country:",
                       list("Australia","Argentina","Brazil","Canada","China","European_Union","France","Germany","India","Italy","Indonesia","Japan","Mexico","Russia","Saudi_Arabia","South_Africa","South_Korea","Turkey","Great_Britain","US")
           ),
           selectInput("state35", "Choose a Parameter:",list("GDP","GDPgrowth","perCapita","lifeExpectancy"))
           
         ),
         mainPanel(
           plotOutput("plot4")
         )
         
)
)
  server = function(input, output) {
    
  
    
    data<-reactive({    
      parametres=list(GDP='NY.GDP.MKTP.CD',GDPgrowth="NY.GDP.MKTP.KD.ZG",perCapita="NY.GDP.PCAP.CD",lifeExpectancy="SP.DYN.LE00.IN")
      country=list(Australia='AU',Russia='RU',India='IN',China='CN',US='US',Canada='CA',European_Union="EU",Great_Britain='GB',France='FR',Germany='DE',Japan='JP',Argentina='AR',Brazil='BR',Indonesia='ID',Italy='IT',Mexico='MX',South_Africa='ZA',Saudi_Arabia='SA'
                   ,Turkey='TR',South_Korea='KR')


      p=data.frame(wb(indicator=parametres[input$state],country=country[input$state1],startdate=input$bins,enddate = input$bin1))
    p
    })
    
   output$plot2<-renderPlot({
     colorSamp=sample(c("blue","green","red","yellow","magenta","purple"),1)
     
      ggplot(data=data(),aes(x=as.numeric(date),y=value,color=colorSamp))+geom_line()+geom_smooth(fill="blue")+xlab('Date')+geom_point()},height = 600,width = 1000)
   data1<-reactive({    
     parametres=list(GDP='NY.GDP.MKTP.CD',GDPgrowth="NY.GDP.MKTP.KD.ZG",perCapita="NY.GDP.PCAP.CD",lifeExpectancy="SP.DYN.LE00.IN")
     country=list(Australia='AU',Russia='RU',India='IN',China='CN',US='US',Canada='CA',European_Union="EU",Great_Britain='GB',France='FR',Germany='DE',Japan='JP',Argentina='AR',Brazil='BR',Indonesia='ID',Italy='IT',Mexico='MX',South_Africa='ZA',Saudi_Arabia='SA'
                  ,Turkey='TR',South_Korea='KR')
     
     
     p=data.frame(wb(indicator=parametres[input$state9],country=c(country[input$state5],country[input$state6],country[input$state7],country[input$state8]),startdate=input$bins,enddate = input$bin1))
     p
   })
   output$plot3<-renderPlot({
     
     
     ggplot(data=data1(),aes(x=as.numeric(date),y=value,color=country))+geom_line()+xlab('Date')+geom_point()},height = 600,width = 1000)
   
   parametres=list(GDP='NY.GDP.MKTP.CD',GDPgrowth="NY.GDP.MKTP.KD.ZG",perCapita="NY.GDP.PCAP.CD",lifeExpectancy="SP.DYN.LE00.IN")
   country=list(Australia='AU',Russia='RU',India='IN',China='CN',US='US',Canada='CA',European_Union="EU",Great_Britain='GB',France='FR',Germany='DE',Japan='JP',Argentina='AR',Brazil='BR',Indonesia='ID',Italy='IT',Mexico='MX',South_Africa='ZA',Saudi_Arabia='SA'
                ,Turkey='TR',South_Korea='KR')
    
   data3<-reactive({    
     w=wb(indicator=parametres[input$state35],country=c(country[input$state31],country[input$state32],country[input$state33],country[input$state34],country[input$state36]),startdate=1960,enddate =2016)
     w
   })
   output$plot4<-renderPlot({
     
     w=data3()
     e=w
     p1=w[1:57,]
     p2=w[58:114,]
     p3=w[115:171,]
     p4=w[171:228,]
     p5=w[229:286,]
     View(w)
     View(e)
     
View(p1)
names(p1)[names(p1)=="value"]=e[1,6]

          cor_tab=merge(p1,p2,by="date")
          names(cor_tab)[names(cor_tab)=="value.y"]=e[58,6]
          
          
           View(cor_tab)
          cor_tab=merge(cor_tab,p3,by="date")
          names(cor_tab)[names(cor_tab)=="value.y"]=e[115,6]
          names(cor_tab)[names(cor_tab)=="value.x"]=e[58,6]
          
          View(cor_tab)
          
          cor_tab=merge(cor_tab,p4,by="date")
          names(cor_tab)[names(cor_tab)=="value.y"]=e[172,6]
          View(cor_tab)
          
          cor_tab=merge(cor_tab,p5,by="date")
          names(cor_tab)[names(cor_tab)=="value.y"]=e[229,6]
          names(cor_tab)[names(cor_tab)=="value.x"]=e[172,6]
          
          View(cor_tab)
          num <- sapply(cor_tab, is.numeric)
          cor_tab=cor_tab[,num]
          corr=cor(cor_tab)
          corrplot(corr,addCoef.col = "green",order = "AOE",height = 600,width = 1000)
   
       })
   
   output$down <- downloadHandler(
     filename =  function() {
       paste("Data", input$var1, sep=".")
     },
     # content is a function with argument file. content writes the plot to the device
     content = function(file) {
       if(input$var1 == "png")
         png(file) # open the png device
       else
         pdf(file) # open the pdf device
        # draw the plot
       colorSamp=sample(c("blue","green","red","yellow","magenta","purple"),1)
    print(ggplot(data=data(),aes(x=as.numeric(date),y=value,color=colorSamp))+geom_line()+xlab('Date')+geom_point(),height = 600,width = 1000)

   dev.off()  # turn the device off
   
     }) 
  }
  

# Run the application 
shinyApp(ui = ui, server = server)

