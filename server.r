#  (C) 2021  Bernhard Klingenberg bklingenberg@ncf.edu
library(ggplot2)
library(shiny)
#library(gridExtra)
#library(grid)

mycol <- c("#669900", "#669900")

# Builds a graph for T-distribution
tgraph <- function(df, normal=FALSE){
  plot = ggplot(data = df, aes(x = x, y = y)) + theme_classic() + #theme modified later
    theme(text=element_text(size=16),
          plot.title=element_text(size=17, hjust = 0.5, vjust=2, face="bold"),
          plot.subtitle=element_text(size=16, hjust = 0.5, vjust=3, color="#FF6600"),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size=14),
          plot.margin=unit(c(0.2,0.1,1,0.1),"cm")
    ) +
    scale_y_continuous(expand=c(0,0), limits=c(0, .42)) 
  #+ scale_x_continuous(breaks=seq(-4,4,1))
  if(normal){
    plot <- plot + 
      #geom_line(data=data.frame(x=df$x, y=dnorm(df$x)), aes(color="Normal"), size=1.1) +  
      geom_area(data=data.frame(x=df$x, y=dnorm(df$x)), aes(x=x, y=y, fill="Standard Normal"), alpha=0.25) +
      geom_line(data=df, aes(x=x, y=y), color="black", linewidth=1.1, alpha=0.8) + 
      geom_area(data=df, aes(x=x, y=y, fill="t-Distribution"), alpha=0.25) +
      scale_fill_manual(name="Distribution:", limits=c("t-Distribution", "Standard Normal"), values=c(mycol[1], "blue")) +
      theme(legend.position="inside", legend.position.inside=c(0.15, 0.75), legend.key.size = unit(0.8,"cm"), legend.text = element_text(size=15))
  }
  plot <-  plot + geom_line(data=df, aes(x=x, y=y), color="black", linewidth=1.0, alpha=0.8) + 
    geom_area(data=df, aes(x=x, y=y), fill=mycol[1], alpha=0.25)
  return(plot)
}

qRound <- function(x, df) { #adjusted to work for t-distribution
  ## returns rounded percentile as a string, depending on magnitude of sigma
  if(any(is.na(x)) | any(!is.finite(x))) return(NA)
  if (df <= 2) sig <- 1 else sig <- sqrt(df/(df-2))
  if (sig >= 1000) return(format(round(x), nsmall=0, scientific = FALSE))
  if (sig >=  100) return(format(round(x, 1), nsmall=1, scientific = FALSE))
  if (sig >=   10) return(format(round(x, 2), nsmall=2, scientific = FALSE))
  if (sig >=    1) return(format(round(x, 3), nsmall=3, scientific = FALSE))
  myDigits = 2 - floor(log10(abs(sig)))
  if(!is.finite(myDigits)) myDigits = 2
  return(format(round(x, myDigits), nsmall=myDigits, scientific = FALSE))
}

pRound <- function(x) {
  ## returns rounded probability as a percentage
  if(any(is.na(x)) | any(!is.finite(x))) return(NA)
  if (x >= 1) return(paste0(format(round(x,2), nsmall=2, scientific = FALSE),"%"))
  if (x >= 0.1) return(paste0(format(round(x,3), nsmall=3, scientific = FALSE),"%"))
  if (x >= 0.01) return(paste0(format(round(x,4), nsmall=4, scientific = FALSE),"%"))
  myDigits = 2 - floor(log10(abs(x)))
  if(!is.finite(myDigits)) myDigits = 2
  return(paste0(format(round(x, myDigits), nsmall=myDigits, scientific = FALSE),"%"))
}


shinyServer(function(input, output, session){
  
  observeEvent(list(input$ownParam, input$df0), {
    if (input$ownParam) {
      df0 <- input$df0  
      updateSliderInput(session, "df", value=df0, min=floor(0.2*df0), max=ceiling(2*df0))
    } else {
      updateSliderInput(session, "df", value=4, min=1, max=40)
    }
  })
  
  observeEvent(input$df, {
    updateNumericInput(session,"df1", value=input$df)
  })
  
  observeEvent(input$df1, {
    updateNumericInput(session,"df2", value=input$df)
  })
  
  plots <- reactiveValues(x=seq(-5.4,5.4,length.out=350), plot=NULL)
  
  # reactive plot for Explore tab
  output$graph <- renderPlot({
    #if(is.null(input$df) | is.na(input$df) | (input$df<1)) return(NULL)
    Tdf <- data.frame(x=plots$x, y=dt(plots$x,input$df))
    p <- tgraph(Tdf, normal=input$addNorm)
    main <- substitute(t~Distribution~with~df~""==""~b, list(b = input$df))
    plot <- p  + scale_x_continuous(breaks=seq(-5,5,1)) + labs(title=main)
    #gt <- ggplot_gtable(ggplot_build(plot))
    ##gt$layout$clip[gt$layout$name == "panel"] <- "off"
    plots$plot <- plot
    #grid.draw(gt)
    return(plot)
  })
  
  results <- reactiveValues(df=NULL, x=NULL, x1=NULL, x2=NULL, prob=NULL, quan=NULL)
  
  # reactive graph for Finding Probabilities tab
  output$graph1 <- renderPlot({
    #if(is.null(input$df1) | is.na(input$df) | (input$df<1) | is.na(input$xval)) return(NULL)
    #get a base graph
    q <- req(input$x)
    df <- req(input$df1)
    if(df<4){
      plots$x <- seq(-7.2,7.2,length.out=400)
      breaks1 <- seq(-7,7,1)
    } else{
      plots$x <- seq(-5.4,5.4,length.out=400)
      breaks1 <- seq(-5,5,1)
    }
    Tdf <- data.frame(x=plots$x, y=dt(plots$x,df))
    basic.plot <- tgraph(Tdf, normal=input$addNorm1)
    #makes the colored-in boundaries
    switch(input$prob,
           "lower" = {
             q <- req(input$x, cancelOutput = FALSE); results$x <- q
             b <- 100*pt(q, df, lower.tail = TRUE); results$prob <- b
             basic.plot <- basic.plot +
               geom_area(data = subset(Tdf, x<=q), aes(x, y), fill = mycol[2], alpha = 0.3) +
               geom_vline(xintercept=q, size=1.4, linetype="dashed") +
               geom_point(x=q, y=0, size=6, pch="X", color="sienna2") +
               annotate("text", x = q, y = Inf, label = paste(round(b,2),"%", sep=""), size=6, vjust=1, hjust=1.1, color=mycol[2], alpha=0.6, fontface="bold") +
               annotate("text", x = q, y = Inf, label = paste(round(100-b,2),"%", sep=""), size=6, vjust=1, hjust=-0.2, color=mycol[2], alpha=0.3, fontface="bold") +
               annotate("text", label=q, x=q, y=0, vjust=2.9, size=6, color="sienna2", alpha=1)
             subtitle = substitute(P(X~""<""~a)~""==""~b, list(a=q, b=pRound(b)))  #bquote(P(X <= .(round(q, 3))) == .(round(input$p,3)))
           },
           "int" = {
             q <- sort(c(req(input$a, cancelOutput = FALSE), req(input$b, cancelOutput = FALSE))); results$x <- q
             b <- 100*(pt(q[2],df) - pt(q[1], df)); results$prob <- b
             basic.plot <- basic.plot +
               geom_area(data = subset(Tdf, (x>=q[1])&(x<=q[2])), aes(x, y), fill = mycol[2], alpha = 0.3) +
               geom_vline(xintercept=q, size=1.4, linetype="dashed") +
               geom_point(x=q[1], y=0, size=6, pch="X", color="sienna2") +
               geom_point(x=q[2], y=0, size=6, pch="X", color="sienna2") +
               annotate("text", x = sum(q)/2, y = Inf, label = paste(round(b,2),"%",sep=""), size=6, vjust=0.5, hjust=0.5, color=mycol[2], fontface="bold") +
               annotate("text", x = q[1], y = Inf, label = paste(round(100*pt(q[1], df),2),"%",sep=""), size=6, vjust=1, hjust=1.1, color=mycol[1], fontface="bold") +
               annotate("text", x = q[2], y = Inf, label = paste(round(100*pt(q[2], df, lower.tail=FALSE),2),"%",sep=""), size=6, vjust=1, hjust=-0.2, color=mycol[1], fontface="bold") +
               annotate("text", label=q[1], x=q[1], y=0, vjust=2.9, size=6, color="sienna2") +
               annotate("text", label=q[2], x=q[2], y=0, vjust=2.9, size=6, color="sienna2")
             subtitle = substitute(P(a~""<""~X~""<""~b)~""==""~c, list(a=q[1], b=q[2], c=pRound(b)))
           },
           "upper" = {
             q <- req(input$x, cancelOutput = FALSE); results$x <- q
             b <- 100*pt(q, df, lower.tail = FALSE); results$prob <- b
             basic.plot <- basic.plot +
               geom_area(data = subset(Tdf, x>=q), aes(x, y), fill = mycol[2], alpha = 0.3) +
               geom_vline(xintercept=q, size=1.4, linetype="dashed") +
               geom_point(x=q, y=0, size=6, pch="X", color="sienna2") +
               annotate("text", x = q, y = Inf, label = paste(round(b,2),"%", sep=""), size=6, vjust=1, hjust=-0.2, color=mycol[2], fontface="bold") +
               annotate("text", x = q, y = Inf, label = paste(round(100-b,2),"%", sep=""), size=6, vjust=1, hjust=1.1, color=mycol[1], fontface="bold") +
               annotate("text", label=q, x=q, y=0, vjust=2.9, size=6, color="sienna2", alpha=1)
             subtitle = substitute(P(X~"">""~a)~""==""~b, list(a=q, b=pRound(b)))
           },
           "two-tailed" = {
             q <- sort(c(req(input$a, cancelOutput = FALSE), req(input$b, cancelOutput = FALSE))); results$x <- q
             b1 <- 100*pt(q[1], df)
             b2 <- 100*pt(q[2], df, lower.tail = FALSE)
             b <- b1 + b2; results$prob <- b
             x1 = q[1]
             x2 = q[2]
             basic.plot <- basic.plot +
               geom_area(data = subset(Tdf, x<=x1), aes(x, y), fill = mycol[2], alpha = 0.3) +
               geom_area(data = subset(Tdf, x>=x2), aes(x, y), fill = mycol[2], alpha = 0.3) +
               geom_vline(xintercept=c(x1,x2), size=1.4, linetype="dashed") +
               geom_point(x=x1, y=0, size=6, pch="X", color="sienna2") +
               geom_point(x=x2, y=0, size=6, pch="X", color="sienna2") +
               annotate("text", x = x1, y = Inf, label = paste(round(b1,2),"%",sep=""), size=6, vjust=1, hjust=1.1, color=mycol[2], alpha=0.9, fontface="bold") +
               annotate("text", x = x2, y = Inf, label = paste(round(b2,2),"%",sep=""), size=6, vjust=1, hjust=-0.2, color=mycol[2], alpha=0.9, fontface="bold") +
               annotate("text", x = (x1+x2)/2, y = Inf, label = paste(round(100-b,2),"%",sep=""), size=6, vjust=0.2, hjust=0.5, color=mycol[1], alpha=0.4, fontface="bold") +
               annotate("text", label=x1, x=x1, y=0, vjust=2.9, color="sienna2", size=6) +
               annotate("text", label=x2, x=x2, y=0, vjust=2.9, color="sienna2", size=6)
             subtitle = substitute(P(X~""<""~a)~" + "~P(X~"">""~b)~""==""~c, list(a=q[1], b=q[2], c=pRound(b)))
           }
    )
    
    main <- substitute(t~Distribution~with~df~""==""~b, list(b = df))
    plot <- basic.plot + ggtitle(label=main, subtitle=subtitle) + 
      scale_x_continuous(breaks=breaks1) +
      coord_cartesian(clip = "off")
    plots$plot <- plot
    return(plot)
  })
  
  ## Include box with results below graph
  output$caption1 <- renderUI(HTML("<b> <u> <span style='color:#000000'> Probability for t-Distribution (Lower Tail): </u> </b>"))
  
  output$probtable1 <- renderTable({
    df <- data.frame(df=input$df1, x=format(results$x), 
                     prob=pRound(results$prob))
    colnames(df) <- c("Degrees of Freedom", "Value of x", HTML(paste0("Probability<br>P(X < ", results$x, ")")))
    return(df)
  }, border=FALSE, striped=FALSE, hover=TRUE, digits=4, align = "c",
  sanitize.text.function = function(x) x,
  )
  
  
  output$caption2 <- renderUI(HTML("<b> <u> <span style='color:#000000'> Probability for t-Distribution (Upper Tail): </u> </b>"))
  
  output$probtable2 <- renderTable({
    df <- data.frame(df=input$df1, x=format(results$x), 
                     prob=pRound(results$prob))
    colnames(df) <- c("Degrees of Freedom", "Value of x", HTML(paste0("Probability<br>P(X > ", results$x, ")")))
    return(df)
  }, border=FALSE, striped=FALSE, hover=TRUE, digits=4, align = "c",
  sanitize.text.function = function(x) x,
  )
  
  output$caption3 <- renderUI(HTML("<b> <u> <span style='color:#000000'> Probability for t-Distribution (Interval): </u> </b>"))
  
  output$probtable3 <- renderTable({
    df <- data.frame(df=input$df1, x1=format(results$x[1]), x2=format(results$x[2]), 
                     prob=pRound(results$prob))
    colnames(df) <- c("Degrees of Freedom", "Value of x1", "Value of x2", 
                      HTML(paste0("Probability<br>P(", results$x[1], " < X < ", results$x[2], ")")))
    return(df)
  }, border=FALSE, striped=FALSE, hover=TRUE, digits=4, align = "c",
  sanitize.text.function = function(x) x,
  )
  
  output$caption4 <- renderUI(HTML("<b> <u> <span style='color:#000000'> Probability for t-Distribution (Tail Probability): </u> </b>"))
  
  output$probtable4 <- renderTable({
    df <- data.frame(df=input$df1, x1=format(results$x[1]), x2=format(results$x[2]),
                     prob=pRound(results$prob))
    colnames(df) <- c("Degrees of Freedom", "Value of x1", "Value of x2", 
                      HTML(paste0("Probability<br>P(X < ", results$x[1], ") + P(X > ", results$x[2], ")"))) 
    return(df)
  }, border=FALSE, striped=FALSE, hover=TRUE, digits=4, align = "c",
  sanitize.text.function = function(x) x,
  )
  
  
  # reactive graph for Finding Percentiles tab
  # this needs to be rewritten just like the normal app!!! DONE! ?
  output$graph2 <- renderPlot({
    #if(is.null(input$df) | is.na(input$df) | (input$df<1) | any(is.na(c(input$pl, input$pu, input$pm))) | any(c(input$pl, input$pu, input$pm)<0) | any(c(input$pl, input$pu, input$pm)>100)) return(NULL)
    df <- req(input$df2)
    if(df<4){
      lb <- -7
      ub <- 7
      plots$x <- seq(-7.2,7.2,length.out=400)
      breaks1 <- seq(-7,7,1)
    } else{
      lb <- -5
      ub <- 5
      plots$x <- seq(-5.4,5.4,length.out=400)
      breaks1 <- seq(-5,5,1)
    }
    Tdf <- data.frame(x=plots$x, y=dt(plots$x,df))
    basic.plot <- tgraph(Tdf, normal=input$addNorm2)
    switch(input$perc,
           "lower" = {
             b <- req(input$pl, cancelOutput = FALSE); results$prob <- paste0(b,"%")
             q <- qt(b/100,df)
             qLabel <- qRound(q, df); results$quan <- qLabel
             basic.plot <- basic.plot + 
               geom_area(data = subset(Tdf, x<=q), aes(x, y), fill = mycol[2], alpha = 0.3) + 
               geom_vline(xintercept=q, size=1.4, linetype="dashed") +
               geom_point(x=q, y=0, size=6, pch="X", color="sienna2") + 
               annotate("text", x = q, y = Inf, label = paste(round(b,2),"%",sep=""), size=6, vjust=1, hjust=1.1, color=mycol[2], alpha=0.6) +
               annotate("text", x = q, y = Inf, label = paste(round(100-b,2),"%",sep=""), size=6, vjust=1, hjust=-0.2, color=mycol[2], alpha=0.3) +
               annotate("text", label=qLabel, x=q, y=0, vjust=2.9, size=6, color="sienna2", alpha=1, fontface="bold")
             subtitle <- substitute(P(X~""<""~a)~""==""~b*"%", list(a=qLabel, b=b))  #bquote(P(X <= .(qLabel)) == .(round(input$p,3)))
           },
           "int" = {
             b <- req(input$pm, cancelOutput = FALSE); results$prob <- paste0(b,"%")
             q <- qt(sort(c((100-b)/200, 1 - (100-b)/200)), df)
             qLabel <- qRound(q, df); results$quan <- qLabel
             basic.plot <- basic.plot + 
               geom_area(data = subset(Tdf, (x>=q[1])&(x<=q[2])), aes(x, y), fill = mycol[2], alpha = 0.3) +
               geom_vline(xintercept=q, size=1.4, linetype="dashed") +
               geom_point(x=q[1], y=0, size=6, pch="X", color="sienna2") + 
               geom_point(x=q[2], y=0, size=6, pch="X", color="sienna2") + 
               annotate("text", x = 0, y = Inf, label = paste(round(b,2),"%",sep=""), size=6, vjust=0.5, hjust=0.5, color=mycol[2], alpha=0.6) +        
               annotate("text", x = q[1], y = Inf, label = paste(round((100-b)/2,2),"%",sep=""), size=6, vjust=1, hjust=1.1, color=mycol[2], alpha=0.3) +        
               annotate("text", x = q[2], y = Inf, label = paste(round((100-b)/2,2),"%",sep=""), size=6, vjust=1, hjust=-0.2, color=mycol[2], alpha=0.3) +        
               annotate("text", label=qLabel[1], x=q[1], y=0, vjust=2.9, size=6, color="sienna2", alpha=1, fontface="bold") +
               annotate("text", label=qLabel[2], x=q[2], y=0, vjust=2.9, size=6, color="sienna2", alpha=1, fontface="bold")
             subtitle = substitute(P(a~""<""~X~""<""~b)~""==""~c*"%", list(a=qLabel[1], b=qLabel[2], c=b))
           },
           "upper" = {
             b <- req(input$pu, cancelOutput = FALSE); results$prob <- paste0(b,"%")
             q <- qt(1-b/100,df)
             qLabel <- qRound(q, df); results$quan <- qLabel
             basic.plot <- basic.plot + 
               geom_area(data = subset(Tdf, x>=q), aes(x, y), fill = mycol[2], alpha = 0.3) +
               geom_vline(xintercept=q, size=1.4, linetype="dashed") +
               geom_point(x=q, y=0, size=6, pch="X", color="sienna2") + 
               annotate("text", x = q, y = Inf, label = paste(round(b,2),"%",sep=""), size=6, vjust=1, hjust=-0.2, color=mycol[2], alpha=0.6) +
               annotate("text", x = q, y = Inf, label = paste(round(100-b,2),"%",sep=""), size=6, vjust=1, hjust=1.1, color=mycol[2], alpha=0.3) +
               annotate("text", label=qLabel, x=q, y=0, vjust=2.9, size=6, color="sienna2", alpha=1, fontface="bold")
             subtitle = substitute(P(X~"">""~a)~""==""~b*"%", list(a=qLabel, b=input$pu)) #bquote(P(X <= .(round(q, 3))) == .(round(qsl,3)))
           }      
    )
    
    main <- substitute(t~Distribution~with~df~""==""~b, list(b = df))
    plot <- basic.plot + labs(title=main, subtitle=subtitle) +
      scale_x_continuous(breaks=breaks1) + #expand=expansion()
      coord_cartesian(clip = "off")
    plots$plot <- plot
    return(plot)
  })
  
  ## Include box with results below graph
  output$qcaption1 <- renderUI(HTML("<b> <u> <span style='color:#000000'> t-Score for t-Distribution (Lower Tail): </u> </b>"))
  
  output$qprobtable1 <- renderTable({
    
    df <- data.frame(df=input$df2, prob=format(req(results$prob)),
                     x=req(results$quan)) 
    colnames(df) <- c("Degrees of Freedom", "Percent in Lower Tail", "Value of x")
    return(df)
  }, border=FALSE, striped=FALSE, hover=TRUE, align = "c",
  sanitize.text.function = function(x) x
  )
  
  
  output$qcaption2 <- renderUI(HTML("<b> <u> <span style='color:#000000'> t-score for t-Distribution (Upper Tail): </u> </b>"))
  
  output$qprobtable2 <- renderTable({
    df <- data.frame(df=input$df2, prob=format(results$prob),
                     x=results$quan) 
    colnames(df) <- c("Degrees of Freedom", "Percent in Upper Tail", "Value of x")
    return(df)
  }, border=FALSE, striped=FALSE, hover=TRUE, align = "c",
  sanitize.text.function = function(x) x
  )
  
  output$qcaption3 <- renderUI(HTML("<b> <u> <span style='color:#000000'> t-score for t-Distribution (Two-Tailed):: </u> </b>"))
  
  output$qprobtable3 <- renderTable({
    df <- data.frame(df=input$df2, prob=format(results$prob),
                     x1=results$quan[1], x2=results$quan[2]) 
    colnames(df) <- c("Degrees of Freedom", "Central Percent", "Value of x1", "Value of x2")
    return(df)
  }, border=FALSE, striped=FALSE, hover=TRUE, align = "c",
  sanitize.text.function = function(x) x
  )
  
  
  
  # download function, bare min necessary
  output$save <- downloadHandler(
    filename = paste('tDist.png'),
    content = function(file) {
      png(file, height=350, width=750)
      print(plots$plot)
      dev.off()
    },
    contentType = "image/png"
  )
  
  # download function, bare min necessary
  output$save1 <- downloadHandler(
    filename = paste('tDist_Probability.png'),
    content = function(file) {
      png(file, height=350, width=750)
      print(plots$plot)
      dev.off()
    },
    contentType = "image/png"
  )
  
  # download function, bare min necessary
  output$save2 <- downloadHandler(
    filename = paste('tDist_Percentile.png'),
    content = function(file) {
      png(file, height=350, width=750)
      print(plots$plot)
      dev.off()
    },
    contentType = "image/png"
  )
  
})