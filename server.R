library(shiny)

shinyServer(function(input,output){
  denominator <- function(w,Re,Cdl,Rct,s){
    return(sqrt(w)*(1+2*s*Cdl*sqrt(w)+2*s^2*w*Cdl^2+2*s*w^(3/2)*Cdl^2*Rct+w^2*Cdl^2*Rct^2)) 
  }
  
  ZRe <- function(w,Re,Cdl,Rct,s){
    return(Re + (s+sqrt(w)*Rct)/denominator(w,Re,Cdl,Rct,s))
  }
  
  ZIm <- function(w,Re,Cdl,Rct,s){
    return(-(s+2*s^2*sqrt(w)*Cdl+2*s*w*Cdl*Rct+w^(3/2)*Cdl*Rct^2)
           /denominator(w,Re,Cdl,Rct,s))
  }
  
  Re <- reactive({input$Re})
  Cdl <- reactive({input$Cdl})
  Rct <- reactive({input$Rct})
  s <- reactive({input$s})
  
  ReZ <- function(w) ZRe(w,Re(),Cdl()*1e-9,Rct(),s())
  ImZ <- function(w) ZIm(w,Re(),Cdl()*1e-9,Rct(),s())
  
  wmin <- reactive({input$wmin})
  wmax <- reactive({input$wmax})
  obs <- reactive({input$obs})
  w <- reactive({exp(log(wmin())+(log(wmax())-log(wmin()))/obs()*(1:obs()))})
  
  x <- reactive({ReZ(w())})
  y <- reactive({-ImZ(w())})
  
  output$impPlot <- renderPlot(plot(x(),y(), ylim=c(0,8000), xlim=c(0,20000),
                                    xlab = "ReZ [Ohm]", ylab="-ImZ [Ohm]"))
})
