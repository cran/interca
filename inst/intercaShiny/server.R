#library(FactoMineR)
#library(factoextra)
#library(writexl)
#library(shiny)
#library(waiter)
#library(readr)
#library(readxl)

shinyServer(function(input, output,session) {

  data <- reactive({
    infile <- input$file1

    if (is.null(infile))
      return(NULL)
    print(str(infile))

    if(infile$type == "text/csv") {
      read_csv(infile$datapath,col_names = input$header)->data
    } else if(infile$type == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
      read_xlsx(infile$datapath,col_names = input$header)->data
    }

    index <- 1:ncol(data)
    data[ , index] <- lapply(data[ , index], as.factor)
    return(data)

  })
  output$contents<-DT::renderDataTable(data(),options=list(pageLength=10,scrollX = TRUE,scrollY = TRUE))



  run_mca<-reactive({
    data()
    #  source('interca.R')
    interca(data(),10)->res
    return(res)
  })

  output$scree<-renderPlot({
    if(input$show_scree==TRUE){
      run_mca()->result
      result$plot

    }
  })


  scree_plot<-reactive({
    if(input$show_scree==TRUE){
      run_mca()->result
      result$plot

    }
  })
  plot_pdf <- reactive({
    pdf("plot.pdf", width = 8, height = 6)
    print(scree_plot())
    dev.off()
  })

  plot_axis <- reactive({
    pdf("plot.pdf", width = 8, height = 6)
    print(axis())
    dev.off()
  })
  plot_plane <- reactive({
    pdf("plot.pdf", width = 8, height = 6)
    print(plane())
    dev.off()
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      "plot.pdf"
    },
    content = function(file) {
      plot_pdf()
      file.copy("plot.pdf", file)
    }
  )
  output$download_axis <- downloadHandler(
    filename = function() {
      "plot.pdf"
    },
    content = function(file) {
      plot_axis()
      file.copy("plot.pdf", file)
    }
  )
  output$download_plane <- downloadHandler(
    filename = function() {
      "plot.pdf"
    },
    content = function(file) {
      plot_plane()
      file.copy("plot.pdf", file)
    }
  )

  list_results<-eventReactive(input$run,{

    interca(data(),input$num_axes)->res
    return(res)

  })

  table_results_coords<-reactive({
    if(input$show_coords==TRUE){
      list_results()->tmp
      round(tmp[[1]],2)
    }

  })
  table_results_ecoords<-reactive({
    if(input$show_ecoords==TRUE){
      list_results()->tmp
      round(tmp[[2]],2)
    }

  })
  table_results_ctr<-reactive({
    if(input$show_ctr==TRUE){
      list_results()->tmp
      round(tmp[[3]],2)
    }

  })
  table_results_cor<-reactive({
    if(input$show_cor==TRUE){
      list_results()->tmp
      round(tmp[[4]],2)
    }

  })



  output$results_coords<-DT::renderDataTable(table_results_coords(),
                                             options=list(pageLength=5,scrollX = TRUE,scrollY = TRUE))
  output$results_ecoords<-DT::renderDataTable(table_results_ecoords(),
                                              options=list(pageLength=5,scrollX = TRUE,scrollY = TRUE))
  output$results_ctr<-DT::renderDataTable(table_results_ctr(),
                                          options=list(pageLength=5,scrollX = TRUE,scrollY = TRUE))
  output$results_cor<-DT::renderDataTable(table_results_cor(),
                                          options=list(pageLength=5,scrollX = TRUE,scrollY = TRUE))

  output$show_coords_label<-renderText({
    if(input$show_coords==TRUE){
      return(" Coordinates")}

  })
  output$show_ecoords_label<-renderText({
    if(input$show_ecoords==TRUE){
      return("interpretive Coordinates")}
  })
  output$show_ctr_label<-renderText({
    if(input$show_ctr==TRUE){
      return("ctr index")}
  })
  output$show_cor_label<-renderText({
    if(input$show_cor==TRUE){
      return("Coordinates")}
  })

  slider_axis_table<-eventReactive(input$do_axis,{
    if(input$cb_slider_axis==TRUE){

      list_results()->etable
      etable[[2]]->df

      df[,input$which.axis]->df
      avg=mean(abs(df))
      threshold=as.numeric(input$slider_axis)
      which(abs(df)>threshold)->pos
      df[pos]->df
      mylist=list(df,avg)

      return(mylist)
    }


  })

  slider_plane_table<-eventReactive(input$do_plane,{
    if(input$cb_slider_plane==TRUE){
      list_results()->eplane
      eplane[[2]]->df
      df[,input$which.xaxis]->first
      df[,input$which.yaxis]->second
      avg=mean(abs(first)+abs(second))
      threshold=as.numeric(input$slider_plane)
      which(abs(first)+abs(second)>threshold)->pos
      first[pos]->first
      second[pos]->second
      mylist=list(first,second,avg)
      return(mylist)
    }


  })

  axis_table<-eventReactive(input$do_axis,{
    as.data.frame(round(slider_axis_table()[[1]],2))->axis_table
    axis_table=as.data.frame(axis_table)
    colnames(axis_table)=c("Interpretive coordinates")
    return(axis_table)
  })

  output$download_axis_table <- downloadHandler(
    filename = function() {
      "axis.xlsx"
    },
    content = function(file) {
      openxlsx::write.xlsx(axis_table(), file,row.names=T)
    }
  )

  plane_table<-eventReactive(input$do_plane,{
    cbind(as.data.frame(round(slider_plane_table()[[1]],2)),as.data.frame(round(slider_plane_table()[[2]],2)))->plane_table
    plane_table=as.data.frame(plane_table)
    colnames(plane_table)=c("x-axis interpretive coordinates","y-axis interpretive coordinates")
    return(plane_table)
  })

  output$download_plane_table <- downloadHandler(
    filename = function() {
      "plane.xlsx"
    },
    content = function(file) {
      openxlsx::write.xlsx(plane_table(), file,row.names=T)
    }
  )


  output$slider_axis_table<-DT::renderDataTable( axis_table(),
                                                 options=list(pageLength=5,scrollX = TRUE,scrollY = TRUE))

  output$slider_plane_table<-DT::renderDataTable( plane_table(),
                                                  options=list(pageLength=5,scrollX = TRUE,scrollY = TRUE))

  axis<-eventReactive(input$do_axis,{
    # source('plot1d.R')
    # source('plot1dslider.R')
    if(input$cb_slider_axis==TRUE){
      plot1dslider(slider_axis_table()[[1]],slider_axis_table()[[2]])->axis
    }

    return(axis)
  })

  output$axis<-renderPlot({
    axis()
  })

  plane<-eventReactive(input$do_plane,{
    #  source('plot2d.R')
    #   source('plot2dslider.R')
    if(input$cb_slider_plane==TRUE){

      plot2dslider(slider_plane_table()[[1]],slider_plane_table()[[2]],slider_plane_table()[[3]])->plane
    }else{
      plot2d(list_results(),input$which.xaxis,input$which.yaxis)->plane
    }
    return(plane)
  })


  output$plane<-renderPlot({
    plane()

  })

  observeEvent(req(input$which.xaxis<=input$num_axes & input$which.yaxis<=input$num_axes),{
    list_results()->df
    df[[2]]->thedata1
    thedata1[,input$which.xaxis]->first1
    thedata1[,input$which.yaxis]->second1
    print(first1)
    print(second1)
    themax1=round(max(abs(first1)+abs(second1)),2)
    themin1=round(min(abs(first1)+abs(second1)),2)
    ave1=round(mean(abs(first1)+abs(second1)),2)
    print(themax1)
    print(themin1)
    print(ave1)
    updateSliderInput(session,"slider_plane",value=ave1,min=themin1,max=themax1,step=0.05)
  })

  observe({
    condition<-as.integer(input$which.axis)<=5
    req(condition)
    list_results()->myd
    myd[[2]]->thedata

    thedata[,input$which.axis]->thedata
    themin=round(min(abs(thedata)),2)
    themax=round(max(abs(thedata)),2)
    ave=round(mean(abs(thedata)),2)
    updateSliderInput(session,"slider_axis",value=ave,min=themin,max=themax,step=0.05)


  })

  observeEvent(input$do_axis, {

    req(input$which.axis>=input$num_axes | input$which.axis<=0)
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "
      please specify a value <= number of selected axes!",
      type = "error"
    )

  })

  observeEvent(input$do_plane, {

    req(input$which.xaxis>input$num_axes | input$which.yaxis>input$num_axes | input$which.xaxis<=0 | input$which.yaxis<=0)
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "
      please specify a value for  x-axis and y-axis in interval [1, number of selected axes]",
      type = "error"
    )

  })


  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      #e=new.env()
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(axis=axis(),axis_table=axis_table())

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  output$report_plane <- downloadHandler(

    filename = "report_plane.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report_plane.Rmd")
      file.copy("report_plane.Rmd", tempReport, overwrite = TRUE)
      params <- list(plane=plane(),plane_table=plane_table())
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )



  observeEvent(input$run, {
    withProgressWaitress({
      for (i in 1:100) {
        incProgressWaitress(5)
        Sys.sleep(0.001)
      }
    }, selector = "#run", max = 100, theme = "overlay-percent")
  })



  plot2dslider <- function(first,second,ave) {
    e1 <- first
    e2 <- second
    avge1 <- mean(abs(e1))
    avge2 <- mean(abs(e2))
    avepl1 <- ave
    lbl <- names(first)
    c=abs(e1)+abs(e2)
    auto_mca_table <- data.frame(cbind(e1,e2,lbl,c))
    auto_mca_table$e1 <- as.numeric(auto_mca_table$e1)
    auto_mca_table$e2 <- as.numeric(auto_mca_table$e2)
    auto_mca_table$lbl <- as.factor(auto_mca_table$lbl)
    auto_mca_table$c <- as.numeric(auto_mca_table$c)
    averageccc = avepl1
    avexx=c(averageccc,0,-averageccc,0,averageccc)
    aveyy=c(0,-averageccc,0,averageccc,0)
    averagesquaree=round(as.data.frame(cbind(avexx,aveyy)),2)

    ggplot()->interpretive_plane1
    interpretive_plane1+geom_path(data=averagesquaree,aes(x=avexx,y=aveyy),color='red')->interpretive_plane1

    for (i in 1:nrow(auto_mca_table)){

      thexx=c(auto_mca_table$c[i],0,-auto_mca_table$c[i],0,auto_mca_table$c[i])
      theyy=c(0,-auto_mca_table$c[i],0,auto_mca_table$c[i],0)
      ccc=cbind(thexx,theyy)
      ccc=as.data.frame(ccc)

      interpretive_plane1 +geom_path(data=ccc,aes(x=thexx,y=theyy),show.legend = T,size=0.1)->interpretive_plane1

    }
    interpretive_plane1+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+coord_fixed()+
      geom_point(data=auto_mca_table,aes(x = e1, y = e2),shape = "circle", size = 1.5, colour = "black")->interpretive_plane1

    interpretive_plane1+geom_label_repel(data=auto_mca_table,
                                         mapping=aes(x=e1,y=e2,label = paste(lbl)), # data point size
                                         size = 3.5,
                                         max.overlaps = Inf,
                                         point.padding = 0.7,
                                         min.segment.length = 0.9,
                                         box.padding = 1.2

    )+
      labs(x = paste("Interpretive x-axis"),y=paste("Interpretive y-axis"))->interpretive_plane1

    interpretive_plane1
  }



  plot1dslider <- function(df,avg) {
    e1 <- as.vector(df)
    n <- length(e1)
    avge <- avg
    ye1 <- rep(0,length(e1))
    lbl<-names(df)
    auto_mca_tablee1 <- data.frame(cbind(e1,ye1,lbl))
    auto_mca_tablee1$e1 <- as.numeric(auto_mca_tablee1$e1)
    auto_mca_tablee1$ye1 <- as.numeric(auto_mca_tablee1$ye1)
    auto_mca_tablee1$lbl <- as.factor(auto_mca_tablee1$lbl)
    interpretive1 <- auto_mca_tablee1%>%
      ggplot() +
      aes(x = e1, y = ye1) +
      geom_point(shape = "circle", size = 1, colour = "#B22222") +
      labs(x = paste("Interpretive axis"), y="")+theme(axis.text.y=element_blank(),
                                                       axis.ticks.y=element_blank())+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
      geom_label_repel(
        aes(label = lbl),
        size = 6.5,max.overlaps = Inf,
        point.padding = 0.9,
        min.segment.length = 0.6,
        box.padding = 0.1
      )

    interpretive1<- interpretive1+geom_vline(color="red",xintercept = avge)+
      geom_vline(xintercept = -avge,color='red')

    interpretive1
  }






})
