#  (C) 2021  Bernhard Klingenberg bklingenberg@ncf.edu
library(ggplot2)
library(shiny)
#library(gridExtra)
#library(grid)

mycol <- "#669900"

# Builds a graph for T-distribution
tgraph <- function(df, normal=FALSE){
  plot = ggplot(data = df, aes(x = x, y = y)) + theme_classic() + #theme modified later
    theme(text=element_text(size=16),
          plot.title=element_text(hjust = 0.5, vjust=2, face="bold"),
          plot.subtitle=element_text(size=17, hjust = 0.5, vjust=3, color="#FF6600"),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size=16),
          plot.margin=unit(c(0.2,0.1,1,0.1),"cm")
    ) +
    scale_y_continuous(expand=c(0,0), limits=c(0, .42)) 
  #+ scale_x_continuous(breaks=seq(-4,4,1))
  if(normal){
    plot <- plot + 
      #geom_line(data=data.frame(x=df$x, y=dnorm(df$x)), aes(color="Normal"), size=1.1) +  
      geom_area(data=data.frame(x=df$x, y=dnorm(df$x)), aes(x=x, y=y, fill="Standard Normal"), alpha=0.25) +
      geom_line(data=df, aes(x=x, y=y), color="black", size=1.1, alpha=0.8) + 
      geom_area(data=df, aes(x=x, y=y, fill="t-Distribution"), alpha=0.25) +
      scale_fill_manual(name="Distribution:", limits=c("t-Distribution", "Standard Normal"), values=c(mycol, "blue")) +
      theme(legend.position=c(0.15, 0.75), legend.key.size = unit(0.8,"cm"), legend.text = element_text(size=15))
  }
  plot <-  plot + geom_line(data=df, aes(x=x, y=y), color="black", size=1.0, alpha=0.8) + 
    geom_area(data=df, aes(x=x, y=y), fill=mycol, alpha=0.25)
  return(plot)
}


shinyServer(function(input, output, session){
  
  observeEvent(input$df, {
    updateNumericInput(session,"df1", value=input$df)
  })
  
  observeEvent(input$df1, {
    updateNumericInput(session,"df2", value=input$df)
  })
  
  plots <- reactiveValues(x=seq(-5,5,length.out=350), plot=NULL)
  
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
    q <- req(input$xval)
    df <- req(input$df1)
    if(df<4){
      lb <- -7
      ub <- 7
      plots$x <- seq(-7,7,length.out=400)
      breaks1 <- seq(-7,7,1)
    } else{
      lb <- -5
      ub <- 5
      plots$x <- seq(-5,5,length.out=400)
      breaks1 <- seq(-5,5,1)
    }
    Tdf <- data.frame(x=plots$x, y=dt(plots$x,df))
    basic.plot <- tgraph(Tdf, normal=input$addNorm1)
    upper <- 100*round(pt(q,df),5)
    #makes the colored-in boundaries
    switch(input$probabilities,
           "bound1" = {
             limit1 <- c(min(lb,q-1),max(ub,q+1))
             plot <- basic.plot +
               geom_area(data=subset(Tdf, x <= q), stat="identity", fill=mycol, alpha=0.4) +
               geom_vline(xintercept=q, size=1.4, linetype="dashed") +
               geom_point(x=q, y=0, size=6, pch="X", color="#FF6600") +
               annotate("text", x = q, y = Inf, label = paste(round(upper,2),"%",sep=""), size=6, vjust=1, hjust=1.1, color=mycol, alpha=0.9, fontface="bold") +
               annotate("text", x = q, y = Inf, label = paste(round(100-upper,2),"%",sep=""), size=6, vjust=1, hjust=-0.2, color=mycol, alpha=0.4, fontface="bold") +
               annotate("text", label=q, x=q, y=0, color="#FF6600", vjust=2.9, size=6) +
               scale_x_continuous(limits=limit1, breaks=breaks1)
             subtitle <- substitute(P(X ~""<""~a)~""==""~b *"%", list(a=q, b=round(upper,2)))
           },
           "bound2" = {
             limit1 <- c(min(lb,q-1),max(ub,q+1))
             plot <- basic.plot +
               geom_area(data=subset(Tdf, x >= q), stat="identity", fill=mycol, alpha=0.4) +
               geom_vline(xintercept=q, size=1.4, linetype="dashed") +
               geom_point(x=q, y=0, size=6, pch="X", color="#FF6600") +
               annotate("text", x = q, y = Inf, label = paste(round(100-upper,2),"%",sep=""), size=6, vjust=1, hjust=-0.2, color=mycol, alpha=0.9, fontface="bold") +
               annotate("text", x = q, y = Inf, label = paste(round(upper,2),"%",sep=""), size=6, vjust=1, hjust=1.1, color=mycol, alpha=0.4, fontface="bold") +
               annotate("text", label=q, x=q, y=0, color="#FF6600", vjust=2.9, size=6) +
               scale_x_continuous(limits=limit1, breaks=breaks1)
             subtitle <- substitute(P(X ~"">""~a)~""==""~b *"%", list(a=q, b=round(100-upper,2)))
           },
           "bound3" = {
             ql <- req(input$xval1)
             qu <- req(input$xval2)
             limit1 <- c(min(lb,-abs(ql)-1),max(ub,abs(qu)+1))
             d <- round(100*(pt(abs(qu),df) - pt(-abs(ql),df)),2)
             plot <- basic.plot +
               geom_area(data=subset(Tdf, x <= abs(qu) & x >= -abs(ql)), stat="identity", fill=mycol, alpha=0.4) +
               geom_vline(xintercept=-abs(ql), size=1.4, linetype="dashed") +
               geom_point(x=-abs(ql), y=0, size=6, pch="X", color="#FF6600") +
               geom_vline(xintercept=abs(qu), size=1.4, linetype="dashed") +
               geom_point(x=abs(qu), y=0, size=6, pch="X", color="#FF6600") +
               annotate("text", x = 0, y = Inf, label = paste(round(d,2),"%",sep=""), size=6, vjust=0.2, hjust=0.5, color=mycol, alpha=0.9, fontface="bold") +
               annotate("text", x = -abs(ql), y = Inf, label = paste(round((100-d)/2,2),"%",sep=""), size=6, vjust=1, hjust=1.1, color=mycol, alpha=0.4, fontface="bold") +
               annotate("text", x = abs(qu), y = Inf, label = paste(round((100-d)/2,2),"%",sep=""), size=6, vjust=1, hjust=-0.2, color=mycol, alpha=0.4, fontface="bold") +
               annotate("text", label=-abs(ql), x=-abs(ql), y=0, color="#FF6600", vjust=2.9, size=6) +
               annotate("text", label=abs(qu), x=abs(qu), y=0, color="#FF6600", vjust=2.9, size=6) +
               scale_x_continuous(limits=limit1, breaks=breaks1)
             subtitle <- substitute(P(b1~""<""~ X ~ ""<""~b2)~""==""~d *"%", list(b1=-abs(ql),b2=abs(qu), d=d))
           },
           "bound4" = {
             limit1 <- c(min(lb,-abs(q)-1),max(ub,abs(q)+1))
             if(q <= 0) {
               d=100
               x1=0
               x2=0
             } else {
               d <- round(100-(100*(pt(abs(q),df) - pt(-abs(q),df))),2)
               x1=-abs(q)
               x2=abs(q)
             }
             plot <- basic.plot +
               geom_area(data=subset(Tdf, x <= x1), stat="identity", fill=mycol, alpha=0.4) +
               geom_area(data=subset(Tdf, x >= x2), stat="identity", fill=mycol, alpha=0.4) +
               geom_vline(xintercept=x1, size=1.4, linetype="dashed") +
               geom_point(x=x1, y=0, size=6, pch="X", color="#FF6600") +
               geom_vline(xintercept=x2, size=1.4, linetype="dashed") +
               geom_point(x=x2, y=0, size=6, pch="X", color="#FF6600") +
               annotate("text", x = x1, y = Inf, label = paste(round(d/2,2),"%",sep=""), size=6, vjust=1, hjust=1.1, color=mycol, alpha=0.9, fontface="bold") +
               annotate("text", x = x2, y = Inf, label = paste(round(d/2,2),"%",sep=""), size=6, vjust=1, hjust=-0.2, color=mycol, alpha=0.9, fontface="bold") +
               annotate("text", x = 0, y = Inf, label = paste(round(100-d,2),"%",sep=""), size=6, vjust=0.2, hjust=0.5, color=mycol, alpha=0.4, fontface="bold") +
               annotate("text", label=x1, x=x1, y=0, vjust=2.9, color="#FF6600", size=6) +
               annotate("text", label=x2, x=x2, y=0, vjust=2.9, color="#FF6600", size=6) +
               scale_x_continuous(limits=limit1, breaks=breaks1)
             subtitle <- substitute(P(abs(X)~"">""~b)~""==""~ d * "%", list(b=q, d=d))
           }
    )
    
    main <- substitute(t~Distribution~with~df~""==""~b, list(b = df))
    plot <- plot + labs(title=main, subtitle=subtitle) + coord_cartesian(clip = "off")
    plots$plot <- plot
    return(plot)
  })
  
  ## Include box with results below graph
  output$caption1 <- renderUI(HTML("<b> <u> <span style='color:#000000'> Probability for t-Distribution (Lower Tail): </u> </b>"))
  
  output$probtable1 <- renderTable({
    df <- data.frame(df=input$df1, x=input$xval, prob=pt(input$xval, df=input$df1))
    colnames(df) <- c("df", "x", HTML("P(X < x)"))
    return(df)
  }, border=TRUE, striped=TRUE, hover=TRUE, digits=4,align = "c",
  sanitize.text.function = function(x) x
  #include.colnames=FALSE,
  #add.to.row = list(pos = list(0), command = " <tr> <th> df </th>  <th> x </th> <th> P(X < x) </th> </tr>" )
  )
  
  output$caption2 <- renderUI(HTML("<b> <u> <span style='color:#000000'> Probability for t-Distribution (Upper Tail): </u> </b>"))
  
  output$probtable2 <- renderTable({
    df <- data.frame(df=input$df1, x=input$xval, prob=pt(input$xval, df=input$df1, lower.tail=FALSE))
    colnames(df) <- c("df", "x", HTML("P(X > x)"))
    return(df)
  }, border=TRUE, striped=TRUE, hover=TRUE, digits=4,align = "c",
  sanitize.text.function = function(x) x
  #include.colnames=FALSE,
  #add.to.row = list(pos = list(0), command = " <tr> <th> df </th>  <th> x </th> <th> P(X > x) </th> </tr>")
  )
  
  output$caption3 <- renderUI(HTML("<b> <u> <span style='color:#000000'> Probability for t-Distribution (Interval): </u> </b>"))
  
  output$probtable3 <- renderTable({
    df <- data.frame(df=input$df1, a=input$xval1, b=input$xval2, prob=pt(input$xval2, df=input$df1) - pt(input$xval1, df=input$df1))
    colnames(df) <- c("df", "a", "B", HTML("P(a < X < b)"))
    return(df)
  }, border=TRUE, striped=TRUE, hover=TRUE, digits=4,align = "c",
  sanitize.text.function = function(x) x
  #include.colnames=FALSE,
  #add.to.row = list(pos = list(0), command = " <tr> <th> df </th> <th> a </th> <th> b </th> <th> P(a < X < b) </th> </tr>" )
  )
  
  output$caption4 <- renderUI(HTML("<b> <u> <span style='color:#000000'> Probability for t-Distribution (Tail Probability): </u> </b>"))
  
  output$probtable4 <- renderTable({
    df <- data.frame(df=input$df1, x=input$xval, prob=2*pt(-abs(input$xval), df=input$df1))
    colnames(df) <- c("df", "x", HTML("P(X < -|x|  or  X > |x|)"))
    return(df)
  }, border=TRUE, striped=TRUE, hover=TRUE, digits=4,align = "c",
  sanitize.text.function = function(x) x
  #include.colnames=TRUE
  #add.to.row = list(pos = list(0), command = " <tr> <th> df </th> <th> x </th> <th> P(X < -|x|  or  X > |x|) </th> </tr>" )
  )
  
  
  
  
  # reactive graph for Finding Percentiles tab
  # this needs to be rewritten just like the normal app!!! DONE! ?
  output$graph2 <- renderPlot({
    #if(is.null(input$df) | is.na(input$df) | (input$df<1) | any(is.na(c(input$pl, input$pu, input$pm))) | any(c(input$pl, input$pu, input$pm)<0) | any(c(input$pl, input$pu, input$pm)>100)) return(NULL)
    df <- req(input$df2)
    if(df<4){
      lb <- -7
      ub <- 7
      plots$x <- seq(-7,7,length.out=400)
      breaks1 <- seq(-7,7,1)
    } else{
      lb <- -5
      ub <- 5
      plots$x <- seq(-5,5,length.out=400)
      breaks1 <- seq(-5,5,1)
    }
    Tdf <- data.frame(x=plots$x, y=dt(plots$x,df))
    basic.plot <- tgraph(Tdf, normal=input$addNorm2)
    switch(input$quan,
           "bound1" = {
             b <- req(input$pl)
             q <- qt(b/100, df = df)
             limit1 <- c(min(lb,q-1),max(ub,q+1))
             plot <- basic.plot + geom_area(data=subset(Tdf, x <= q), stat="identity", fill=mycol, alpha=0.4) +
               geom_vline(xintercept=q, size=1.4, linetype="dashed") +
               geom_point(x=q, y=0, size=6, pch="X", color="#FF6600") +
               annotate("text", x = q, y = Inf, label = paste(round(b,2),"%",sep=""), size=6, vjust=1, hjust=1.1, color=mycol, alpha=0.9) +
               annotate("text", x = q, y = Inf, label = paste(round(100-b,2),"%",sep=""), size=6, vjust=1, hjust=-0.2, color=mycol, alpha=0.4) +
               annotate("text", label=round(q,3), x=q, y=0, vjust=2.9, size=6, color="#FF6600", alpha=1, fontface="bold") +
               scale_x_continuous(limits=limit1, breaks=breaks1)
             subtitle <- substitute(P(X ~""<""~a)~""==""~b *"%", list(a=round(q,3), b=b))
           },
           "bound2" = {
             b <- req(input$pu)
             q <- qt(b/100, df=df, lower.tail=FALSE)
             limit1 <- c(min(lb,q-1),max(ub,q+1))
             plot <- basic.plot +
               geom_area(data=subset(Tdf, x >= q), stat="identity", fill=mycol, alpha=0.4) +
               geom_vline(xintercept=q, size=1.4, linetype="dashed") +
               geom_point(x=q, y=0, size=6, pch="X", color="#FF6600") +
               annotate("text", x = q, y = Inf, label = paste(round(b,2),"%",sep=""), size=6, vjust=1, hjust=-0.2, color=mycol, alpha=0.9) +
               annotate("text", x = q, y = Inf, label = paste(round(100-b,2),"%",sep=""), size=6, vjust=1, hjust=1.1, color=mycol, alpha=0.4) +
               annotate("text", label=round(q,3), x=q, y=0, vjust=2.9, size=6, color="#FF6600", alpha=1, fontface="bold") +
               scale_x_continuous(limits=limit1, breaks=breaks1)
             subtitle <- substitute(P(X ~"">""~a)~""==""~b *"%", list(a=round(q,3), b=b))
           },
           "bound3" = {
             b <- req(input$pm)
             q <- qt(sort(c((100-b)/200, 1 - (100-b)/200)), df=df)
             limit1 <- c(min(lb,q[1]-1),max(ub,q[2]+1))
             plot <- basic.plot +
               geom_area(data=subset(Tdf, x >= q[1] & x<= q[2]), stat="identity", fill=mycol, alpha=0.4) +
               geom_vline(xintercept=q[1], size=1.4, linetype="dashed") +
               geom_point(x=q[1], y=0, size=6, pch="X", color="#FF6600") +
               geom_vline(xintercept=q[2], size=1.4, linetype="dashed") +
               geom_point(x=q[2], y=0, size=6, pch="X", color="#FF6600") +
               annotate("text", x = 0, y = Inf, label = paste(round(b,2),"%",sep=""), size=6, vjust=0.2, hjust=0.5, color=mycol, alpha=0.9) +
               annotate("text", x = q[1], y = Inf, label = paste(round((100-b)/2,2),"%",sep=""), size=6, vjust=1, hjust=1.1, color=mycol, alpha=0.4) +
               annotate("text", x = q[2], y = Inf, label = paste(round((100-b)/2,2),"%",sep=""), size=6, vjust=1, hjust=-0.2, color=mycol, alpha=0.4) +
               annotate("text", label=round(q[1],3), x=q[1], y=0, vjust=2.9, size=6, color="#FF6600", alpha=1, fontface="bold") +
               annotate("text", label=round(q[2],3), x=q[2], y=0, vjust=2.9, size=6, color="#FF6600", alpha=1, fontface="bold") +
               scale_x_continuous(limits=limit1, breaks=breaks1)
             subtitle <- substitute(P(b1~""<""~ X ~ ""<""~b2)~""==""~d *"%", list(b1=round(q[1],3), b2=round(q[2],3), d=b))
           }
    )
    
    main <- substitute(t~Distribution~with~df~""==""~b, list(b = df))
    main <- substitute(t~Distribution~with~df~""==""~b, list(b = df))
    plot <- plot + labs(title=main, subtitle=subtitle) + coord_cartesian(clip = "off")
    plots$plot <- plot
    return(plot)
  })
  
  ## Include box with results below graph
  output$qcaption1 <- renderUI(HTML("<b> <u> <span style='color:#000000'> t-Score for t-Distribution (Lower Tail): </u> </b>"))
  
  output$qprobtable1 <- renderTable({
    df <- data.frame(df=input$df2, q=paste0(input$pl,"%"), quan=qt(input$pl/100, df=input$df2))
    colnames(df) <- c("df", "Percent in Lower Tail", "Percentile")
    return(df)
  }, border=TRUE, striped=TRUE, hover=TRUE, digits=4, align = "c",
  sanitize.text.function = function(x) x
  #include.colnames=FALSE,
  #add.to.row = list(pos = list(0), command = " <tr> <th> df </th>  <th> x </th> <th> P(X < x) </th> </tr>" )
  )
  
  output$qcaption2 <- renderUI(HTML("<b> <u> <span style='color:#000000'> t-score for t-Distribution (Upper Tail): </u> </b>"))
  
  output$qprobtable2 <- renderTable({
    df <- data.frame(df=input$df2, q=paste0(input$pu,"%"), quan=qt(input$pu/100, df=input$df2, lower.tail = FALSE))
    colnames(df) <- c("df", "Percent in Upper Tail", "Percentile")
    return(df)
  }, border=TRUE, striped=TRUE, hover=TRUE, digits=4, align = "c",
  sanitize.text.function = function(x) x
  #include.colnames=FALSE,
  #add.to.row = list(pos = list(0), command = " <tr> <th> df </th>  <th> x </th> <th> P(X > x) </th> </tr>")
  )
  
  output$qcaption3 <- renderUI(HTML("<b> <u> <span style='color:#000000'> t-score for t-Distribution (Two-Tailed):: </u> </b>"))
  
  output$qprobtable3 <- renderTable({
    df <- data.frame(df=input$df2, q=paste0(input$pm,"%"), quan=paste(HTML("&pm;"), format(abs(qt((1-input$pm/100)/2, df=input$df2)),digits=4)))
    colnames(df) <- c("df", "Central Percent", "Percentiles")
    return(df)
  }, border=TRUE, striped=TRUE, hover=TRUE, digits=4, align = "c",
  sanitize.text.function = function(x) x
  #include.colnames=FALSE,
  #add.to.row = list(pos = list(0), command = " <tr> <th> df </th> <th> a </th> <th> b </th> <th> P(a < X < b) </th> </tr>" )
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