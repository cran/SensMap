library(FactoMineR)
library(factoextra)
library(MASS)
library(fields)
library(plotly)
library(mgcv)
library(MCMCpack)
library(glmulti)
library(plyr)
library(lattice)
library(ggplot2)
library(reshape2)
library(shiny)
library(googleVis)
library(multcomp)
library(sjPlot)
library(scales)
library(doBy)
library(gridExtra)
library(agricolae)
library(devtools)
library(magrittr)
library(ggbiplot)
library(ggdendro)
library(dendextend)
library(grid)
#devtools::install_github("vqv/ggbiplot")
#install.packages("ggdendro")
#install.packages("pastecs")
library(pastecs)
library(shinydashboard);
source('global.R',local = TRUE)


shinyServer(function(input,output,session)
{
 #### download data
  rawInputData0<- reactive({
    rawData = input$rawInputFile
    headerTag = input$headerUI;
    sepTag = input$sepUI;
    quoteTag = input$quoteUI;
    decTag = input$decUI;
    if(!is.null(rawData)) {
      data = read.csv(rawData$datapath,header=headerTag,dec=decTag ,sep=sepTag,quote=quoteTag);
    } else {
      return(NULL);
    }
  });


  output$product <- renderUI({
    data = rawInputData0();
      if (is.null(data)) return(NULL)
      items=names(data)
      names(items)=items
      selectInput("product","Select product:",items);
    })


  output$assessor <- renderUI({
    data = rawInputData0();
      if (is.null(data)) return(NULL)
      items=names(data)
      names(items)=items
      selectInput("assessor","Select assessor:",items,multiple = T)
  })


  output$replication <- renderUI({
    data = rawInputData0();
      if (is.null(data)) return(NULL)
      items=names(data)
      names(items)=items
      selectInput("replication","Select replication:",items,multiple = T)
  })

  col=reactive({
    dat=rawInputData0()
    s=0
    n=which(colnames(dat) %in% c(paste(input$product),paste(input$replication),paste(input$assessor)))
    for(i in 1:ncol(dat)){
      if (i %in% n)
        s[i]="factor"
      else s[i]=class(dat[,i])
    }
    return(s)
   })


  rawInputData<- reactive({
    rawData = input$rawInputFile
    headerTag = input$headerUI;
    sepTag = input$sepUI;
    quoteTag = input$quoteUI;
    decTag = input$decUI;
    if(!is.null(rawData)) {
      data = read.csv(rawData$datapath,header=headerTag,sep=sepTag,quote=quoteTag,dec=decTag,colClasses = col());
    } else {
      return(NULL);
    }
  });


  consInputData = reactive({
    rd = input$consInputFile
    ht = input$headUI;
    st = input$sepaUI;
    qt = input$quotUI;
    decc = input$deccUI;
    if(!is.null(rd)) {
      data = read.csv(rd$datapath,header=ht,sep=st,dec=decc,quote=qt,row.names = 1);
    } else {
      return(NULL);
    }
  });



  output$stat_sum <- renderDataTable({
    data = rawInputData()
    df.data = stat.desc(data)
    format(df.data,digits=2)
  })


  output$pre.data <- renderDataTable({
    data = rawInputData()
    df.data = data.frame(data)
    df.data
  })


  output$str <- renderPrint({
    dat <- rawInputData()
    str(dat)
  })


  moy=reactive({
    data=rawInputData()
    base=summaryBy(as.formula(paste(". ~ ",paste(input$product,collapse="+"))),data=data,FUN=c(mean),id=paste(input$origine),na.rm=T,keep.names=T)
    base=data.frame(base,row.names =1)
    base
  })

  moy1=reactive({
    data=rawInputData()
    base=summaryBy(as.formula(paste(". ~ ",paste(input$product,collapse="+"))),data=data,FUN=c(mean),na.rm=T,keep.names=T)
    base
  })

  moy2=reactive({
    data=rawInputData()
    if(!is.null(data)){
    base=summaryBy(as.formula(paste(". ~ ",paste(input$product,collapse="+"))),data=data,FUN=c(mean),na.rm=T,keep.names=T)
    base=data.frame(base,row.names =1)
    if(!is.null(physicalData())){
     base=cbind.data.frame(base,physicalData())
     base}
    else{base}}
    else{base=data.frame(physicalData())
    base}
  })

  mo=reactive({
    data=rawInputData()
    if(!is.null(data)){
    base=summaryBy(as.formula(paste(". ~ ",paste(input$product,collapse="+"))),data=data,FUN=c(mean),na.rm=T,keep.names=T)
    base=data.frame(base,row.names =1)
    if(!is.null(physicalData())){
    base=cbind.data.frame(base,physicalData())
    av=format(base,digits = 3,decimal.mark = ".")}
    else{av=format(base,digits = 3,decimal.mark = ".")}}
    else{base=data.frame(physicalData())
    av=format(base,digits = 3,decimal.mark = ".")}
  })

  moy3=reactive({
    data=rawInputData()
    nums <- sapply(data, is.factor)
    items=names(nums[nums])
    df=summaryBy(as.formula(paste(". ~ ",paste(input$product,collapse="+"))),data=data,FUN=c(mean),na.rm=T,keep.names=T,id= items )
    df=data.frame(df)
  })


  output$av<- renderDataTable({
    mo()
  })

  output$downloadav <- downloadHandler(
    filename = paste0("average_", Sys.Date(),".csv"),
    content = function(file) {
      write.csv(mo(),file,row.names=F)
    }
  )

  output$av.table<- renderDataTable({
    moy2()
  })


  moy4=reactive({
    if(!is.null(input$assessor)){
    data=rawInputData()
    base=summaryBy(as.formula(paste(". ~ ",paste(input$product,"+",input$assessor))),data=data,FUN=c(mean),na.rm=T,keep.names=T)
    base=base[,-3]}
     else if (is.null(input$assessor)) return(NULL)
  })

  output$av.table2<- renderDataTable({
    moy4()
  })

  output$downloadavt <- downloadHandler(
    filename = paste0("averagejuge_", Sys.Date(),".csv"),
    content = function(file) {
      write.csv(moy4(),file,row.names=T)
    }
  )


  cercle <- reactive({
    base=moy2()
    res <-PCA(base,ncp=2)
    fviz_pca_var(res)+
      scale_color_gradient2(low="grey", mid="blue",
                            high="red", midpoint=0.6, space = "Lab")+theme_minimal()
  })

  output$cercle <- renderPlot({
    cercle()
  })

  output$downloadPlot1 <- downloadHandler(
    filename = function() {
      paste("cercle", downloadPlotType(), sep=".")
    },

    content = function(con) {

      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(cercle())
      dev.off(which=dev.cur())}
  )

  scree <- reactive({
    base=moy2()
    res <-PCA(base,ncp=2)
    fviz_eig(res, addlabels=TRUE, hjust = -0.3,
     linecolor ="red") +
      theme_minimal()
  })

  output$scree <- renderPlot({
   scree()
  })

  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      paste("scree", downloadPlotType(), sep=".")
    },

    content = function(con) {

      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(scree())
      dev.off(which=dev.cur())}
  )

  stable1 <- reactive({
    base=moy2()
    res <-PCA(base,ncp=2)
    fviz_screeplot(res, linecolor = "red",cex=0.7)
    res$eig
  })

  output$stable <- renderTable({
    stable1()
  })
  output$downloadstab <- downloadHandler(
    filename = paste0("Eigen_", Sys.Date(),".csv"),
    content = function(file) {
      write.csv(stable1(),file,row.names=T)
    }
  )

  ind <- reactive({
    base=moy2()
    res <-PCA(base,ncp=2)
 fviz_pca_ind(res)
  })

     output$ind <- renderPlot({
     ind()
        })

   output$downloadPlot3 <- downloadHandler(
    filename = function() {
      paste("ind", downloadPlotType(), sep=".")
    },

    content = function(con) {

      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(ind())
      dev.off(which=dev.cur())}
  )

  bi <- reactive({
    X=moy2()
    res.pca = PCA(X,ncp = 3, graph = FALSE, scale.unit=1)
    library(devtools)
    library(ggbiplot)
    pca <- prcomp(X, scale. = TRUE)
    g <- ggbiplot(pca)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal',
                   legend.position = 'top')
    print(g)
    biplot=fviz_pca_biplot(res.pca,col.ind = "blue", col.var = "black", title="") + theme_bw()+ggtitle( "Biplot of variables and individuals on first PCA factor map")
    biplot + theme(plot.title = element_text(size=16, hjust = 0.5,face="bold"))
  })


  output$bi <- renderPlot({
   bi()
  })

  output$downloadPlot4 <- downloadHandler(
    filename = function() {
      paste("bi", downloadPlotType(), sep=".")
    },

    content = function(con) {

      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(bi())
      dev.off(which=dev.cur())}
  )

  output$cons.data <- renderDataTable({
    data = consInputData()
    df.data = data.frame(data)
    df.data
  })

  cerclec <- reactive({
    base=consInputData()
    res <-PCA(base,ncp=2)
    fviz_pca_var(res)+theme_minimal()
  })

  output$cerclec <- renderPlot({
    cerclec()
  })

  output$downloadPlot5 <- downloadHandler(
    filename = function() {
      paste("cerclec", downloadPlotType(), sep=".")
    },

    content = function(con) {

      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(cerclec())
      dev.off(which=dev.cur())}
  )

  output$cerclec1 <- renderPlot({
    base=consInputData()
    res <-PCA(base,ncp=2)
    fviz_pca_var(res)+
      scale_color_gradient2(low="grey", mid="navy",
                            high="red", midpoint=96, space = "Lab")
  })

  screec <- reactive({
    base=consInputData()
    res <-PCA(base,ncp=2)
    fviz_eig(res, addlabels=TRUE, hjust = -0.3,
                  linecolor ="red") +
      theme_minimal()

  })

  output$screec <- renderPlot({
    screec()
  })

  output$downloadPlot6 <- downloadHandler(
    filename = function() {
      paste("screec", downloadPlotType(), sep=".")
    },

    content = function(con) {

      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(screec())
      dev.off(which=dev.cur())}
  )
  stablec1 <- reactive({
    base=consInputData()
    res <-PCA(base,ncp=2)
    fviz_screeplot(res, linecolor = "red",cex=0.7)
    res$eig
  })

  output$stablec <- renderTable({
    stablec1()
  })
  output$downloadstab1 <- downloadHandler(
    filename = paste0("Eigen_", Sys.Date(),".csv"),
    content = function(file) {
      write.csv(stablec1(),file,row.names=T)
    }
  )
  indc <- reactive({
    base=consInputData()
    res <-PCA(base,ncp=2)
    fviz_pca_ind(res)
  })

  output$indc <- renderPlot({
    indc()
  })

  output$downloadPlot7 <- downloadHandler(
    filename = function() {
      paste("indc", downloadPlotType(), sep=".")
    },

    content = function(con) {

      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(indc())
      dev.off(which=dev.cur())}
  )

  bic <- reactive({
    X=consInputData()
    res.pca = PCA(X,ncp = 3, graph = FALSE, scale.unit=1)
    library(devtools)
    library(ggbiplot)
    pca <- prcomp(X, scale. = TRUE)
    g <- ggbiplot(pca)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal',
                   legend.position = 'top')
    print(g)
    biplot=fviz_pca_biplot(res.pca,col.ind = "blue", col.var = "black", title="") + theme_bw()+ggtitle( "Biplot of variables and individuals on first PCA factor map")
    biplot + theme(plot.title = element_text(size=16, hjust = 0.5,face="bold"))
  })

  output$bic <- renderPlot({
   bic()
  })

  output$downloadPlot8 <- downloadHandler(
    filename = function() {
      paste("bic", downloadPlotType(), sep=".")
    },

    content = function(con) {

      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(bic())
      dev.off(which=dev.cur())}
  )
  output$summary.data <- renderTable({
    data = rawInputData()
    summary(data)
  })

  output$variablee= renderUI({
    df = rawInputData();
    if (is.null(df)) return(NULL)
    nums <- sapply(df, is.numeric)
    items=names(nums[nums])
    names(items)=items
    selectInput("variablee","Select  variable from:",items)
  })

  output$Min=renderValueBox({
    df=rawInputData()
    a=summary(df)
    c=grep(input$variablee, colnames(df))
    c=c[1]
    valueBox(a[[1,c]],
             "Minimum",
             color = "red",
             icon = icon("thumbs-o-down")
             )
  })

  output$first=renderValueBox({
    df=rawInputData()
    a=summary(df)
    c=grep(input$variablee, colnames(df))
    c=c[1]
    valueBox(a[[2,c]],
             "1st Quartile",
             color = "maroon",
             icon = icon("list-ol")
    )
  })

  output$Median=renderValueBox({
    df=rawInputData()
    a=summary(df)
    c=grep(input$variablee, colnames(df))
    c=c[1]
    valueBox(a[[3,c]],
             "Median",
             color = "olive",
             icon = icon("align-center")
    )
  })

  output$Mean=renderValueBox({
    df=rawInputData()
    a=summary(df)
    c=grep(input$variablee, colnames(df))
    c=c[1]
    valueBox(a[[4,c]],
             "Mean",
             color = "teal",
             icon = icon("align-center")
    )
  })

  output$third=renderValueBox({
    df=rawInputData()
    a=summary(df)
    c=grep(input$variablee, colnames(df))
    c=c[1]
    valueBox(a[[5,c]],
             "3rd Quartile",
             color = "orange",
             icon = icon("list-ol")
    )
  })

  output$Max=renderValueBox({
    df=rawInputData()
    a=summary(df)
    c=grep(input$variablee, colnames(df))
    c=c[1]
    valueBox(a[[6,c]],
             "Maximum",
             color = "purple",
             icon = icon("thumbs-o-up")
    )
  })

  output$var_qual <- renderUI({
    df<- rawInputData()
    if (is.null(df)) return(NULL)
    #Let's only show factor columns
    nums <- sapply(df, is.factor)
    items=names(nums[nums])
    names(items)=items
    selectInput("var_qual", "Select a variable :",items)
  })

    output$pp<- renderPrint({
    df=rawInputData()
    c=grep(input$var_qual, colnames(df))
    data=df[,c]
    a=sjt.frq(data)
    return(HTML(a$knitr))
  })

    output$var_qual<- renderUI({
      df<- rawInputData()
      if (is.null(df)) return(NULL)
      nums <- sapply(df, is.factor)
      items=names(nums[nums])
      names(items)=items
      selectInput("var_qual", "Select a variable :",items)
    })

    output$pp_qual<- renderPrint({
      df=rawInputData()
      c=grep(input$var_qual, colnames(df))
      data=df[,c]
      a=sjt.frq(data)
      return(HTML(a$knitr))
    })

  output$summary.data1 <- renderTable({
    data = consInputData()
    summary(data)
  })

  output$variablee1= renderUI({
    df = consInputData();
    if (is.null(df)) return(NULL)
    nums <- sapply(df, is.numeric)
    items=names(nums[nums])
    names(items)=items
    selectInput("variablee1","Select  variable from:",items)
  })

  output$Min1=renderValueBox({
    df=consInputData()
    a=summary(df)
    c=grep(input$variablee1, colnames(df))
    c=c[1]
    valueBox(a[[1,c]],
             "Minimum",
             color = "red",
             icon = icon("thumbs-o-down")
    )
  })

  output$first1=renderValueBox({
    df=consInputData()
    a=summary(df)
    c=grep(input$variablee1, colnames(df))
    c=c[1]
    valueBox(a[[2,c]],
             "1st Quartile",
             color = "maroon",
             icon = icon("list-ol")
    )
  })

  output$Median1=renderValueBox({
    df=consInputData()
    a=summary(df)
    c=grep(input$variablee1, colnames(df))
    c=c[1]
    valueBox(a[[3,c]],
             "Median",
             color = "olive",
             icon = icon("align-center")
    )
  })

  output$Mean1=renderValueBox({
    df=consInputData()
    a=summary(df)
    c=grep(input$variablee1, colnames(df))
    c=c[1]
    valueBox(a[[4,c]],
             "Mean",
             color = "teal",
             icon = icon("align-center")
    )
  })

  output$third1=renderValueBox({
    df=consInputData()
    a=summary(df)
    c=grep(input$variablee1, colnames(df))
    c=c[1]
    valueBox(a[[5,c]],
             "3rd Quartile",
             color = "orange",
             icon = icon("list-ol")
    )
  })

  output$Max1=renderValueBox({
    df=consInputData()
    a=summary(df)
    c=grep(input$variablee1, colnames(df))
    c=c[1]
    valueBox(a[[6,c]],
             "Maximum",
             color = "purple",
             icon = icon("thumbs-o-up")
    )
  })


  ###############################################



  #simple datatable of the data
  output$rawDataView = renderDataTable({
    newData = rawInputData();
    if(is.null(newData))
      return();
    newData;
  });


  output$labelSelectUI = renderUI({
    data = rawInputData();
      return(selectInput("modelLabelUI","List of variables",colnames(data),colnames(data)[1]));
  });

  output$labelconsUI = renderUI({
    data = consInputData();
      return(selectInput("modelLabelUI","List of variables",colnames(data),colnames(data)[1]));
  });

  physicalData = reactive({
    rd_phy = input$physicalInputFile
    ht_phy = input$headUI_phy;
    st_phy = input$sepaUI_phy;
    qt_phy = input$quotUI_phy;
    dec_phy = input$decUI_phy;
    if(!is.null(rd_phy)) {
      data = read.csv(rd_phy$datapath,header=ht_phy,sep=st_phy,quote=qt_phy,dec=dec_phy,row.names = 1);
    } else {
      return(NULL);
    }
  });

  output$phy.data <- renderDataTable({
    data = physicalData()
    df.data = data.frame(data)
    df.data
  })




  output$labelphyUI = renderUI({
    data = physicalData();
    return(selectInput("modelLabelUI","List of variables",colnames(data),colnames(data)[1]));
  });


  output$jitter <- renderUI({
    if (identical(rawInputData()$input$product, '') || identical(rawInputData()$input$product,data.frame())) return(NULL)
    checkboxInput('jitter', 'Jitter')
  })

  output$gdesc <- renderUI({
    df=moy1()
    df=data.frame(df,row.names = 1)
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("gdesc","Select descriptor variable:",items,multiple = T)
  })


  output$gfactor <- renderUI({
    df<- rawInputData()
    if (is.null(df)) return(NULL)
    #Let's only show factor columns
    nums <- sapply(df, is.factor)
    items=names(nums[nums])
    names(items)=items
    selectInput("gfactor", "Select a factor :",items)
  })

  plotAreac <- reactive({
    data = rawInputData()
    if(input$jitter){
      ggplot(data=data, aes(x=data[,input$gfactor],y=data[,input$gdesc],fill=data[,input$gfactor])) +
        geom_boxplot() +geom_jitter()+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("") + ylab("") + labs(fill = "")
    }
    else if(!input$jitter){
      ggplot(data=data, aes(x=data[,input$gfactor],y=data[,input$gdesc],fill=data[,input$gfactor])) +
        geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("") + ylab("") + labs(fill = "")
    }
  })


  output$plotA <- renderPlot({
    plotAreac()
  })


  output$dependent <- renderUI({
    df <- rawInputData()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("dependent","Select descriptor variable from:",items)
  });


  output$independents <- renderUI({
    df <- rawInputData()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items =items*levels(items)
    selectInput("independents","Select variable(s) from:",items,multiple=TRUE)
  });

  runRegression <- reactive({
    lm(as.formula(paste(input$dependent," ~ ",paste(input$independents,collapse="+"))),data=rawInputData())
  })

  output$regTab <- renderPrint({
    if(!is.null(input$independents)){
      summary(runRegression())
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
    }
  });

  output$dependant <- renderUI({
    df=moy1()
    df=data.frame(df,row.names = 1)
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("dependant","Select response variable : ",items)
  });


  output$independants <- renderUI({
    df<- rawInputData()
    if (is.null(df)) return(NULL)
    #Let's only show factor columns
    nums <- sapply(df, is.factor)
    items=names(nums[nums])
    names(items)=items
    selectInput("independants", "Select explicative variables :",items,multiple = T)
  });


  runRegression <- reactive({
    lm(as.formula(paste(input$dependent," ~ ",paste(input$independents,collapse="+"))),data=rawInputData())
  })

  output$regTab <- renderPrint({
    if(!is.null(input$independents)){
      summary(runRegression())
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
    }
  })

  output$dependant <- renderUI({
    df=moy1()
    df=data.frame(df,row.names = 1)
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("dependant","Select response variable : ",items)
  })


  output$independants <- renderUI({
    df<- rawInputData()
    if (is.null(df)) return(NULL)
    #Let's only show factor columns
    nums <- sapply(df, is.factor)
    items=names(nums[nums])
    names(items)=items
    selectInput("independants", "Select explicative variables :",items,multiple = T)
  })


  runAnova <- reactive({
    aov(as.formula(paste(input$dependant," ~ ",paste(input$independants,collapse="*"))),data=rawInputData())
  })

  output$anov <- renderPrint({
   summary.aov(runAnova())
  })

  get_factors <- reactive ({

    factorstr <- as.character(formula(paste(input$dependant," ~ ",paste(input$independants,collapse="*"))))[3]
    return(sub("\\s","",unlist(strsplit(factorstr,"[*+:]"))))

  })

  tukeyplot1 <- reactive({

    dataset<-rawInputData()
    aov.model<-runAnova()
    out<-HSD.test(aov.model, get_factors())
    # par(cex=1, mar=c(3,8,1,1))
    # bar.group(out$groups,horiz=T,col="blue",
    #           xlim=c(0,max(out$means[,1]*1.2)),las=1)
    data=data.frame(out$groups)
    ggplot(data,aes(x=trt,y=means,fill=M,colour=M))+
      geom_bar(stat = "identity",color="black")+
      geom_text(aes(label=paste0(M),
                                   y=means+0.3), size=5)+ ggtitle("tukey HSD")+ xlab("treatment groups")

  })

  output$tukeyplot <- renderPlot({
    tukeyplot1()
  })
  output$downloadtuk <- downloadHandler(
    filename = function() {
      paste("tukeyPlot", downloadPlotType(), sep=".")
    },

    content = function(con) {

      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(tukeyplot1())
      dev.off(which=dev.cur())}
  )

  output$tukey <- renderPrint({
    dataset<-rawInputData()
    aov.model<-runAnova()
    out <-HSD.test(aov.model, get_factors(), " ")
    print(out)
  })

  anovdd <-reactive(
    as.data.frame(anova(runAnova()))
  )
  output$anov.d <-renderDataTable(
    as.data.frame(anova(runAnova())), server = TRUE, filter = 'top', escape = FALSE, selection = 'none'
  )

  output$downloadtable <- downloadHandler(
    filename = paste0("ANOVA_", Sys.Date(),".csv"),
    content = function(file) {
      write.csv(anovdd(),file,row.names=T)
    }
  )


  output$variab <- renderUI({
    df=moy1()
    df=data.frame(df,row.names = 1)
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("variab","Select descriptor variable from:",items,multiple = T)
  })

  output$Vfactor <- renderUI({
    df<- rawInputData()
    if (is.null(df)) return(NULL)
    #Let's only show factor columns
    nums <- sapply(df, is.factor)
    items=names(nums[nums])
    names(items)=items
    selectInput("Vfactor", "Select a factor :",items)
  })

  plotBreac <- reactive({
    df=rawInputData()
    attach(df)
    h <- list(
      title = ""
    )
    l <- list(
      title = "")
    plot_ly(data=df,x=df[[input$variab]],color=df[[input$Vfactor]],colors = "Set2",type="box")%>%
      layout(xaxis = h, yaxis = l)

  })

  output$plotB<-renderPlotly ({
    plotBreac()

  })







  ##Lines
  output$vlines <- renderUI({
    df=moy1()
    df=data.frame(df,row.names = 1)

    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("vlines","Select descriptor variable from:",items,multiple = T)
  })

  output$flines <- renderUI({
    df<- rawInputData()
    if (is.null(df)) return(NULL)
    #Let's only show factor columns
    nums <- sapply(df, is.factor)
    items=names(nums[nums])
    names(items)=items
    selectInput("flines", "Select a factor :",items)
  })

  output$plotD<-renderGvis ({
    df=rawInputData()
    attach(df)
    return(gvisLineChart(df,xvar=input$flines,yvar=input$vlines, options=list(hAxis="{title:''}",vAxis="{title:''}",width=700,height=600)))
  })
  ###Bubbles :

  output$vbubble <- renderUI({
    data<- rawInputData()
    library(doBy)
    df=moy1()
    df=data.frame(df,row.names = 1)
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("vbubble","Select descriptor variable from:",items,multiple = T)
  })

  output$vbubble1 <- renderUI({
    df=moy1()
    df=data.frame(df,row.names = 1)
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("vbubble1","Select descriptor variable from:",items,multiple = T)
  })

  output$f <- renderUI({
    df<- rawInputData()
    if (is.null(df)) return(NULL)
    #Let's only show factor columns
    nums <- sapply(df, is.factor)
    items=names(nums[nums])
    names(items)=items
    selectInput("f", "Choose your id var :",items)
  })

  output$f1 <- renderUI({
    df<- rawInputData()
    if (is.null(df)) return(NULL)
    #Let's only show factor columns
    nums <- sapply(df, is.factor)
    items=names(nums[nums])
    names(items)=items
    selectInput("f1", "Choose your size var :",items)
  })

  output$f2 <- renderUI({
    df<- rawInputData()
    if (is.null(df)) return(NULL)
    #Let's only show factor columns
    nums <- sapply(df, is.factor)
    items=names(nums[nums])
    names(items)=items
    selectInput("f2", "Choose your color var :",items)
  })


  output$plotEr<-reactive ({
    data=rawInputData()
    return(gvisBubbleChart(data,idvar=input$f,xvar=input$vbubble,yvar=input$vbubble1,colorvar=input$f2,sizevar=input$f1,options=list(hAxis="{title:''}",vAxis="{title:''}",width=700,height=600)))
  })

  output$plotE<-renderGvis ({
    data=rawInputData()
    return(gvisBubbleChart(data,idvar=input$f,xvar=input$vbubble,yvar=input$vbubble1,colorvar=input$f2,sizevar=input$f1,options=list(hAxis="{title:''}",vAxis="{title:''}",width=700,height=600)))


  })


  output$plotF<-renderGvis({
    data=rawInputData()
    cdata=summaryBy(as.formula(paste(". ~ ",paste(input$product,"+",input$replication))),data=data,FUN=c(mean),na.rm=T,keep.names=T)
    transform(cdata, time2 = paste(2010, cdata[,input$replication], sep = "")) -> df1
    df1$time2 <- as.Date(df1$time2 ,"%Y-%m-%d")
         return(gvisMotionChart(df1,idvar=df1[,input$product],timevar="time2",options=list(height=600,width=600,vAxes="[{title:''}",hAxes="[{title:''}")))
  })




  downloadPlotType <- reactive({
    input$downloadPlotType
  })

  observe({
    plotType    <- input$downloadPlotType
    plotTypePDF <- plotType == "pdf"
    plotUnit    <- ifelse(plotTypePDF, "inches", "pixels")
    plotUnitDef <- ifelse(plotTypePDF, 9, 480)

    updateNumericInput(
      session,
      inputId = "downloadPlotHeight",
      label = sprintf("Height (%s)", plotUnit),
      value = plotUnitDef)

    updateNumericInput(
      session,
      inputId = "downloadPlotWidth",
      label = sprintf("Width (%s)", plotUnit),
      value = plotUnitDef)

  })


  # Get the download dimensions.
  downloadPlotHeight <- reactive({
    input$downloadPlotHeight
  })

  downloadPlotWidth <- reactive({
    input$downloadPlotWidth
  })

  # Get the download file name.
  downloadPlotFileName <- reactive({
    input$downloadPlotFileName
  })

  # Include a downloadable file of the plot in the output list.
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(downloadPlotFileName(), downloadPlotType(), sep=".")
    },

    content = function(con) {

      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
        print(plotAreac())
        dev.off(which=dev.cur())}
  )






  output$interval=renderUI({
    Y=consInputData()
    sliderInput("interval", "Interval of Consumers",
                min = 1, max = ncol(Y), value = c(1,40),width = '60%')

  })
  output$cart<-renderPlot({cart() })
  cart=eventReactive(input$actioncarto,{

    Y=consInputData()
    X=moy2()
    hedo_senso=cbind.data.frame(Y[,input$interval[1]:input$interval[2]],X)

    res.inter=PCA(hedo_senso ,scale.unit=T,quanti.sup = (ncol(Y[,input$interval[1]:input$interval[2]])+1):ncol(hedo_senso),graph=FALSE)
    c1=fviz_pca_ind(res.inter, axes = c(1, 2), geom = c("point","text"),title="Plot of individuals" )

    c3=fviz_pca_var(res.inter,invisible = "quanti.sup",geom=c( "arrow", "text"),title="Homogeniety of consumers")

    c2=fviz_pca_var(res.inter,geom = c("arrow","text"),invisible ="var",title="Projection of sensory variables")
   p=grid.arrange(c1,c2,c3,padding = unit(1, "line"),heights = 700)
   p
         })

  output$downloadPlotinternal <- downloadHandler(
    filename = function() {
      paste("plot", downloadPlotType(), sep=".")
    },

    content = function(con) {

      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = 15, height = downloadPlotHeight())
      plot(cart())
      dev.off(which=dev.cur())}
  )

  ################# External preference mapping

  output$interval1=renderUI({
    Y=consInputData()
    sliderInput("interval1", "Interval of Consumers",
                min = 1, max = ncol(Y), value = c(1,40))

  })

  output$modelch=renderUI({
    selectInput("modelch", "Choose type of model :",choices = c("Vector model", "Circular model","Elliptic model",
                                       "Quadratic model"))
  })


  output$Dimension_var<-renderUI({
    selectInput("Dimension_var","Choose dimension reduction method:",choices = c("Principal Component Analysis", "Multiple Factor Analysis", "Canonical Analysis"))
  })

  output$Prediction_var<-renderUI({
    selectInput("Prediction_var","Choose prediction model:",choices = c("Vector model", "Circular model","Elliptic model",
                                                "Quadratic model", "Generalized Additive Model" , "Generalized Linear Model", "Bayesian model","Others"))
  })

  output$par_var<-renderUI({
    if(input$Prediction_var != "Bayesian model"){
    selectInput("par_var","If rejection of predictions outside liking scores:",choices = c("NO","YES"))}
    else{selectInput("par_var","Choose Rejection of predictions outside of [0:10]:",choices = c("NO"))}
  })

  output$mod1<-renderUI({
    if(input$Prediction_var == "Others"){
    selectInput("mod1","Choose type of model:",choices = c("Polynomial regression","Generalized Additive Model" , "Generalized Linear Model", "Bayesian Model"))}
    else return(NULL)
  })

  output$formula_lm1=renderUI({
    if(input$Prediction_var == "Others"){
    if(input$mod1 == "Polynomial regression"){
    textInput("formula_lm1","Input formula for polynomial model :")}
    else if(input$mod1 == "Generalized Additive Model"){
    textInput("formula_lm1","Input formula for GAM :")}
    else if(input$mod1 == "Generalized Linear Model"){
    textInput("formula_lm1","Input formula for GLM :")}
    else if(input$mod1 == "Bayesian Model"){
    textInput("formula_lm1","Input formula for Bayesian model :")}}
  })


  output$interval2=renderUI({
    Y=consInputData()
    sliderInput("interval2", "Interval of Consumers",
                min = 1, max = ncol(Y), value = c(1,40))

  })

  output$epmplot=renderPlot({cart13()})
    cart13=eventReactive(input$actioncarto13,{
    Y=consInputData()
    X=moy2()
    S=moy4()
    nbpoints=50

    if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Vector model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
              dimredumethod=1, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Circular model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
              dimredumethod=1, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
              dimredumethod=1, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=1, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="YES")
   {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
              dimredumethod=1, predmodel=2, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=1, predmodel=3, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}

    ###############
   else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Vector model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
              dimredumethod=1, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Circular model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
              dimredumethod=1, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
              dimredumethod=1, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=1, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
              dimredumethod=1, predmodel=2, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=1, predmodel=3, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Bayesian model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=1, predmodel=4, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}

    ################

    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Vector model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
              dimredumethod=2, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Circular model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
              dimredumethod=2, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
              dimredumethod=2, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=2, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
              dimredumethod=2, predmodel=2, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=2, predmodel=3, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}


    #######################

    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Vector model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
              dimredumethod=2, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Circular model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
              dimredumethod=2, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
              dimredumethod=2, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=2, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
              dimredumethod=2, predmodel=2, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=2, predmodel=3, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Bayesian model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=2, predmodel=4, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}

    #################################
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Vector model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
              dimredumethod=3, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Circular model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
              dimredumethod=3, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
              dimredumethod=3, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=3, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
              dimredumethod=3, predmodel=2, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="YES")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=3, predmodel=3, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}

    #################################
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Vector model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
              dimredumethod=3, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Circular model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
              dimredumethod=3, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
              dimredumethod=3, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=3, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
              dimredumethod=3, predmodel=2, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=3, predmodel=3, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Bayesian model" && input$par_var=="NO")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
              dimredumethod=3, predmodel=4, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    ###############
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
            &&input$mod1=="Polynomial regression")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=1, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
            &&input$mod1=="Polynomial regression")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=1, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
            &&input$mod1=="Generalized Additive Model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=1, predmodel=2, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
            &&input$mod1=="Generalized Additive Model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=1, predmodel=2, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
            &&input$mod1=="Generalized Linear Model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=1, predmodel=3, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
            &&input$mod1=="Generalized Linear Model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=1, predmodel=3, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
            &&input$mod1=="Bayesian model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=1, predmodel=4, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
            &&input$mod1=="Bayesian model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=1, predmodel=4, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    #######################
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
            &&input$mod1=="Polynomial regression")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=2, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
            &&input$mod1=="Polynomial regression")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=2, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
            &&input$mod1=="Generalized Additive Model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=2, predmodel=2, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
            &&input$mod1=="Generalized Additive Model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=2, predmodel=2, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
            &&input$mod1=="Generalized Linear Model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=2, predmodel=3, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
            &&input$mod1=="Generalized Linear Model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=2, predmodel=3, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
            &&input$mod1=="Bayesian model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=2, predmodel=4, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
            &&input$mod1=="Bayesian model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=2, predmodel=4, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    ######################
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
            &&input$mod1=="Polynomial regression")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=3, predmodel=1, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
            &&input$mod1=="Polynomial regression")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=3, predmodel=1, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
            &&input$mod1=="Generalized Additive Model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=3, predmodel=2, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
            &&input$mod1=="Generalized Additive Model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=3, predmodel=2, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
            &&input$mod1=="Generalized Linear Model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=3, predmodel=3, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
            &&input$mod1=="Generalized Linear Model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=3, predmodel=3, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
            &&input$mod1=="Bayesian model")
    {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=3, predmodel=4, nbpoints=50,
              pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )}
    else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
            &&input$mod1=="Bayesian model")
    {
      drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
              dimredumethod=3, predmodel=4, nbpoints=50,
              pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
              graph.map.3D =FALSE )
      }

                       })


    output$epmplot2=renderPlot({cart132()})
    cart132=eventReactive(input$actioncarto13,{
      Y=consInputData()
      X=moy2()
      S=moy4()
      nbpoints=50

      if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Vector model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Circular model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=1, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}

      ###############
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Vector model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Circular model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=1, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Bayesian model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}

      ################

      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Vector model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Circular model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=2, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}


      #######################

      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Vector model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Circular model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=2, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Bayesian model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}

      #################################
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Vector model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Circular model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=3, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}

      #################################
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Vector model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Circular model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=3, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Bayesian model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}

      ##################
      #################
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Polynomial regression")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =TRUE, graph.map =FALSE,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Polynomial regression")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =TRUE, graph.map =FALSE,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Generalized Additive Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =TRUE, graph.map =FALSE,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Generalized Additive Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =TRUE, graph.map =FALSE,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Generalized Linear Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Generalized Linear Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Bayesian model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=4, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Bayesian model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      #######################
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Polynomial regression")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Polynomial regression")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Generalized Additive Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Generalized Additive Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Generalized Linear Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Generalized Linear Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Bayesian model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=4, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Bayesian model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      ######################
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Polynomial regression")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Polynomial regression")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Generalized Additive Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Generalized Additive Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Generalized Linear Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Generalized Linear Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Bayesian model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=4, nbpoints=50,
                pred.na =TRUE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Bayesian model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =T, graph.map =F,
                graph.map.3D =FALSE )}

    })



    output$epmplot3=renderPlotly({cart133()})
    cart133=eventReactive(input$actioncarto13,{
      Y=consInputData()
      X=moy2()
      S=moy4()
      nbpoints=50

      if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Vector model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Circular model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )
       }
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=1, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}

      ###############
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Vector model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Circular model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=1, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Bayesian model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}

      ################

      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Vector model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Circular model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=2, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}


      #######################

      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Vector model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Circular model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=2, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Bayesian model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}

      #################################
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Vector model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Circular model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=3, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="YES")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}

      #################################
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Vector model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Circular model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Elliptic model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Quadratic model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Generalized Additive Model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=3, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Generalized Linear Model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Bayesian model" && input$par_var=="NO")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      ##################
      #################
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Polynomial regression")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =FALSE,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Polynomial regression")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =FALSE,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Generalized Additive Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =FALSE,
                graph.map.3D =T)}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Generalized Additive Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =FALSE,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Generalized Linear Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Generalized Linear Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Bayesian model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=4, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Principal Component Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Bayesian model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=1, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      #######################
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Polynomial regression")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Polynomial regression")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Generalized Additive Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Generalized Additive Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Generalized Linear Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Generalized Linear Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Bayesian model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=4, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Multiple Factor Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Bayesian model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=2, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      ######################
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Polynomial regression")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Polynomial regression")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Generalized Additive Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Generalized Additive Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Generalized Linear Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Generalized Linear Model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="YES"
              &&input$mod1=="Bayesian model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=4, nbpoints=50,
                pred.na =TRUE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}
      else if(input$Dimension_var=="Canonical Analysis" && input$Prediction_var=="Others" && input$par_var=="NO"
              &&input$mod1=="Bayesian model")
      {drawmap (Y[,input$interval2[1]:input$interval2[2]],X,S,axis=c(1,2),formula=input$formula_lm1,
                dimredumethod=3, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =F, graph.map =F,
                graph.map.3D =T )}

    })



    ####### Results of models selection

    output$Dimension_var1<-renderUI({
      selectInput("Dimension_var1","Choose dimension reduction method:",choices = c("Principal Component Analysis", "Multiple Factor Analysis", "Canonical Analysis"))
    })

    output$Prediction_var1<-renderUI({
      selectInput("Prediction_var1","Choose prediction model:",choices = c("Vector model", "Circular model","Elliptic model",
                                                                          "Quadratic model", "Generalized Additive Model" , "Generalized Linear Model"))
    })

    output$aic<-renderUI({
      selectInput("aic","Choose parameter of comparison:",choices = c("AIC","R2", "Fstat","nb-NA"))
    })



    output$model_selection=renderPlot({cart14()})
    cart14=eventReactive(input$actioncarto14,{
      Y=consInputData()
      X=moy2()
      S=moy4()
      nbpoints=50
      if(input$aic=="AIC" && (input$Prediction_var1=="Vector model" ||input$Prediction_var1=="Circular model"||input$Prediction_var1=="Elliptic model"
                              ||input$Prediction_var1=="Quadratic model"||input$Prediction_var1=="Generalized Additive Model"||input$Prediction_var1=="Generalized Linear Model") &&
         input$Dimension_var1== "Principal Component Analysis"){
        map=map.with.pca(X)
        maps=cbind.data.frame(map$F1,map$F2)
        colnames(maps)=c("F1","F2")
        attach(maps )
        discretspace=discrete.function(map = maps)
        x.lm=predict.scores.lm(Y = Y,discretspace = discretspace,map = maps)
        x.vect=predict.scores.lm(Y = Y,formula="~F1+F2",discretspace = discretspace,map = maps)
        x.circ=predict.scores.lm(Y = Y,formula="~F1 + F2 + (F1*F1 + F2*F2)",discretspace = discretspace,map = maps)
        x.ellip=predict.scores.lm(Y = Y,formula="~I(F1*F1)+I(F2*F2)",discretspace = discretspace,map = maps)
        x.gam=predict.scores.gam(Y = Y,discretspace = discretspace,map = maps)
        x.glm=predict.scores.glmulti(Y = Y,discretspace = discretspace,map = maps)

        aic.lm=extract.lm(x.lm,what=c("aic"))
        aic.vect=extract.lm(x.vect,what=c("aic"))
        aic.circ=extract.lm(x.circ,what=c("aic"))
        aic.ellip=extract.lm(x.ellip,what=c("aic"))
        aic.gam=extract.gam(x.gam,what=c("aic"))
        aic.glm=extract.glm(x.glm,what=c("aic"))## ???

        x1=c(1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y))
        x2=c(aic.lm,aic.vect,aic.circ,aic.ellip,aic.gam,aic.glm)
        x3=c(rep("QR",ncol(Y)),rep("Vect",ncol(Y)),rep("Circ",ncol(Y)),rep("Ellip",ncol(Y)),rep("GAM",ncol(Y)),rep("GLM",ncol(Y)))

        library(ggplot2)

        bas=data.frame(x3,x2)
        g2<-ggplot(bas,aes(x=x3,y=x2,fill=x3))+geom_boxplot()+xlab("Models")+
          ylab("AIC")+ggtitle( "Comparison of models with AIC ")
        g2<-g2+theme_bw()+theme(legend.position = "none",
                                axis.text.x = element_text(angle = 90),
                                plot.title = element_text(size=10, hjust = 0.5,face="bold"))
        g2


      }

      else if(input$aic=="AIC" && (input$Prediction_var1=="Vector model" ||input$Prediction_var1=="Circular model"||input$Prediction_var1=="Elliptic model"
                              ||input$Prediction_var1=="Quadratic model"||input$Prediction_var1=="Generalized Additive Model"||input$Prediction_var1=="Generalized Linear Model") &&
         input$Dimension_var1== "Multiple Factor Analysis"){
        map=map.with.mfa(X,Y,axis=c(1,2))
        maps=cbind.data.frame(map$F1,map$F2)
        colnames(maps)=c("F1","F2")
        attach(maps)
        discretspace=discrete.function(map = maps)
        x.lm=predict.scores.lm(Y = Y,discretspace = discretspace,map = maps)
        x.vect=predict.scores.lm(Y = Y,formula="~F1+F2",discretspace = discretspace,map = maps)
        x.circ=predict.scores.lm(Y = Y,formula="~ F1 + F2 + (F1*F1 + F2*F2)",discretspace = discretspace,map = maps)
        x.ellip=predict.scores.lm(Y = Y,formula="~I(F1*F1)+I(F2*F2)",discretspace = discretspace,map = maps)
        x.gam=predict.scores.gam(Y = Y,discretspace = discretspace,map = maps)
        x.glm=predict.scores.glmulti(Y = Y,discretspace = discretspace,map = maps)

        aic.lm=extract.lm(x.lm,what=c("aic"))
        aic.vect=extract.lm(x.vect,what=c("aic"))
        aic.circ=extract.lm(x.circ,what=c("aic"))
        aic.ellip=extract.lm(x.ellip,what=c("aic"))
        aic.gam=extract.gam(x.gam,what=c("aic"))
        aic.glm=extract.glm(x.glm,what=c("aic"))## ???
        x1=c(1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y))
        x2=c(aic.lm,aic.vect,aic.circ,aic.ellip,aic.gam,aic.glm)
        x3=c(rep("QR",ncol(Y)),rep("Vect",ncol(Y)),rep("Circ",ncol(Y)),rep("Ellip",ncol(Y)),rep("GAM",ncol(Y)),rep("GLM",ncol(Y)))

        bas1=data.frame(x3,x2)
        g21<-ggplot(bas1,aes(x=x3,y=x2,fill=x3))+geom_boxplot()+xlab("Models")+
          ylab("AIC")+ggtitle( "Comparison of models with AIC ")
        g21<-g21+theme_bw()+theme(legend.position = "none",
                                axis.text.x = element_text(angle = 90),
                                plot.title = element_text(size=10, hjust = 0.5,face="bold"))
        g21

      }
      else if(input$aic=="AIC" && (input$Prediction_var1=="Vector model" ||input$Prediction_var1=="Circular model"||input$Prediction_var1=="Elliptic model"
                                   ||input$Prediction_var1=="Quadratic model"||input$Prediction_var1=="Generalized Additive Model"||input$Prediction_var1=="Generalized Linear Model") &&
              input$Dimension_var1== "Canonical Analysis"){

        map=map.with.ca(X,S,Y)
        maps=cbind.data.frame(map$F1,map$F2)
        colnames(maps)=c("F1","F2")
        attach(maps )
        discretspace=discrete.function(map = maps)
        x.lm=predict.scores.lm(Y = Y,discretspace = discretspace,map = maps)
        x.vect=predict.scores.lm(Y = Y,formula="~F1+F2",discretspace = discretspace,map = maps)
        x.circ=predict.scores.lm(Y = Y,formula="~ F1 + F2 + (F1*F1 + F2*F2)",discretspace = discretspace,map = maps)
        x.ellip=predict.scores.lm(Y = Y,formula="~I(F1*F1)+I(F2*F2)",discretspace = discretspace,map = maps)
        x.gam=predict.scores.gam(Y = Y,discretspace = discretspace,map = maps)
        x.glm=predict.scores.glmulti(Y = Y,discretspace = discretspace,map = maps)

        aic.lm=extract.lm(x.lm,what=c("aic"))
        aic.vect=extract.lm(x.vect,what=c("aic"))
        aic.circ=extract.lm(x.circ,what=c("aic"))
        aic.ellip=extract.lm(x.ellip,what=c("aic"))
        aic.gam=extract.gam(x.gam,what=c("aic"))
        aic.glm=extract.glm(x.glm,what=c("aic"))## ???
        x1=c(1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y))
        x2=c(aic.lm,aic.vect,aic.circ,aic.ellip,aic.gam,aic.glm)
        x3=c(rep("QR",ncol(Y)),rep("Vect",ncol(Y)),rep("Circ",ncol(Y)),rep("Ellip",ncol(Y)),rep("GAM",ncol(Y)),rep("GLM",ncol(Y)))


        bas2=data.frame(x3,x2)
        g23<-ggplot(bas2,aes(x=x3,y=x2,fill=x3))+geom_boxplot()+xlab("Models")+
          ylab("AIC")+ggtitle( "Comparison of models with AIC ")
        g23<-g23+theme_bw()+theme(legend.position = "none",
                                axis.text.x = element_text(angle = 90),
                                plot.title = element_text(size=10, hjust = 0.5,face="bold"))
        g23

      }

      else if(input$aic=="R2" && (input$Prediction_var1=="Vector model" ||input$Prediction_var1=="Circular model"||input$Prediction_var1=="Elliptic model"
                                   ||input$Prediction_var1=="Quadratic model"||input$Prediction_var1=="Generalized Additive Model"||input$Prediction_var1=="Generalized Linear Model") &&
              input$Dimension_var1== "Principal Component Analysis"){
        map=map.with.pca(X)
        maps=cbind.data.frame(map$F1,map$F2)
        colnames(maps)=c("F1","F2")
        attach(maps )
        discretspace=discrete.function(map = maps)
        x.lm=predict.scores.lm(Y = Y,discretspace = discretspace,map = maps)
        x.vect=predict.scores.lm(Y = Y,formula="~F1+F2",discretspace = discretspace,map = maps)
        x.circ=predict.scores.lm(Y = Y,formula="~ F1 + F2 + (F1*F1 + F2*F2)",discretspace = discretspace,map = maps)
        x.ellip=predict.scores.lm(Y = Y,formula="~I(F1*F1)+I(F2*F2)",discretspace = discretspace,map = maps)
        x.glm=predict.scores.glmulti(Y = Y,discretspace = discretspace,map = maps)
        x.gam=predict.scores.gam(Y = Y,discretspace = discretspace,map = maps)


        r2.lm=extract.lm(x.lm,what=c("rsquare"))
        r2.vect=extract.lm(x.vect,what=c("rsquare"))
        r2.circ=extract.lm(x.circ,what=c("rsquare"))
        r2.ellip=extract.lm(x.ellip,what=c("rsquare"))
        r2.gam=extract.gam(x.gam,what=c("rsquare"))
        r2.glm=extract.gam(x.glm,what=c("rsquare"))
        x1=c(1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y))
        x2=c(r2.lm,r2.vect,r2.circ,r2.ellip,r2.gam,r2.glm)
        x3=c(rep("QR",ncol(Y)),rep("Vect",ncol(Y)),rep("Circ",ncol(Y)),rep("Ellip",ncol(Y)),rep("GAM",ncol(Y)),rep("GLM",ncol(Y)))


        bas3=data.frame(x3,x2)
        g22<-ggplot(bas3,aes(x=x3,y=x2,fill=x3))+geom_boxplot()+xlab("Models")+
          ylab("R2")+ggtitle( "Comparison of models with R-squared ")
        g22<-g22+theme_bw()+theme(legend.position = "none",
                                axis.text.x = element_text(angle = 90),
                                plot.title = element_text(size=10, hjust = 0.5,face="bold"))
        g22

      }
      else if(input$aic=="R2" && (input$Prediction_var1=="Vector model" ||input$Prediction_var1=="Circular model"||input$Prediction_var1=="Elliptic model"
                                   ||input$Prediction_var1=="Quadratic model"||input$Prediction_var1=="Generalized Additive Model"||input$Prediction_var1=="Generalized Linear Model") &&
              input$Dimension_var1== "Multiple Factor Analysis"){
        ## If R2 and  MFA
        map=map.with.mfa(X,Y,axis=c(1,2))
        maps=cbind.data.frame(map$F1,map$F2)
        colnames(maps)=c("F1","F2")
        attach(maps )
        discretspace=discrete.function(map = maps)
        x.lm=predict.scores.lm(Y = Y,discretspace = discretspace,map = maps)
        x.vect=predict.scores.lm(Y = Y,formula="~F1+F2",discretspace = discretspace,map = maps)
        x.circ=predict.scores.lm(Y = Y,formula="~ F1 + F2 + (F1*F1 + F2*F2)",discretspace = discretspace,map = maps)
        x.ellip=predict.scores.lm(Y = Y,formula="~I(F1*F1)+I(F2*F2)",discretspace = discretspace,map = maps)
        x.glm=predict.scores.glmulti(Y = Y,discretspace = discretspace,map = maps)
        x.gam=predict.scores.gam(Y = Y,discretspace = discretspace,map = maps)


        r2.lm=extract.lm(x.lm,what=c("rsquare"))
        r2.vect=extract.lm(x.vect,what=c("rsquare"))
        r2.circ=extract.lm(x.circ,what=c("rsquare"))
        r2.ellip=extract.lm(x.ellip,what=c("rsquare"))
        r2.gam=extract.gam(x.gam,what=c("rsquare"))
        r2.glm=extract.gam(x.glm,what=c("rsquare"))
        x1=c(1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y))
        x2=c(r2.lm,r2.vect,r2.circ,r2.ellip,r2.gam,r2.glm)
        x3=c(rep("QR",ncol(Y)),rep("Vect",ncol(Y)),rep("Circ",ncol(Y)),rep("Ellip",ncol(Y)),rep("Generalized Additive Model",ncol(Y)),rep("Generalized Linear Model",ncol(Y)))


        bas4=data.frame(x3,x2)
        g22<-ggplot(bas4,aes(x=x3,y=x2,fill=x3))+geom_boxplot()+xlab("Models")+
          ylab("R2")+ggtitle( "Comparison of models with R-squared ")
        g23<-g23+theme_bw()+theme(legend.position = "none",
                                  axis.text.x = element_text(angle = 90),
                                  plot.title = element_text(size=10, hjust = 0.5,face="bold"))
        g23
      }
      else if(input$aic=="R2" && (input$Prediction_var1=="Vector model" ||input$Prediction_var1=="Circular model"||input$Prediction_var1=="Elliptic model"
                                   ||input$Prediction_var1=="Quadratic model"||input$Prediction_var1=="Generalized Additive Model"||input$Prediction_var1=="Generalized Linear Model") &&
              input$Dimension_var1== "Canonical Analysis"){
        ## If R2 and  CA
        map=map.with.ca(X,S,Y)
        maps=cbind.data.frame(map$F1,map$F2)
        colnames(maps)=c("F1","F2")
        attach(maps )
        discretspace=discrete.function(map = maps)
        x.lm=predict.scores.lm(Y = Y,discretspace = discretspace,map = maps)
        x.vect=predict.scores.lm(Y = Y,formula="~F1+F2",discretspace = discretspace,map = maps)
        x.circ=predict.scores.lm(Y = Y,formula="~ F1 + F2 + (F1*F1 + F2*F2)",discretspace = discretspace,map = maps)
        x.ellip=predict.scores.lm(Y = Y,formula="~I(F1*F1)+I(F2*F2)",discretspace = discretspace,map = maps)
        x.glm=predict.scores.glmulti(Y = Y,discretspace = discretspace,map = maps)
        x.gam=predict.scores.gam(Y = Y,discretspace = discretspace,map = maps)


        r2.lm=extract.lm(x.lm,what=c("rsquare"))
        r2.vect=extract.lm(x.vect,what=c("rsquare"))
        r2.circ=extract.lm(x.circ,what=c("rsquare"))
        r2.ellip=extract.lm(x.ellip,what=c("rsquare"))
        r2.gam=extract.gam(x.gam,what=c("rsquare"))
        r2.glm=extract.gam(x.glm,what=c("rsquare"))
        x1=c(1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y))
        x2=c(r2.lm,r2.vect,r2.circ,r2.ellip,r2.gam,r2.glm)
        x3=c(rep("QR",ncol(Y)),rep("Vect",ncol(Y)),rep("Circ",ncol(Y)),rep("Ellip",ncol(Y)),rep("GAM",ncol(Y)),rep("GLM",ncol(Y)))

        bas4=data.frame(x3,x2)
        g24<-ggplot(bas4,aes(x=x3,y=x2,fill=x3))+geom_boxplot()+xlab("Models")+
          ylab("R2")+ggtitle( "Comparison of models with R-squared ")
        g24<-g24+theme_bw()+theme(legend.position = "none",
                                  axis.text.x = element_text(angle = 90),
                                  plot.title = element_text(size=10, hjust = 0.5,face="bold"))
        g24
      }

      else if(input$aic=="Fstat" && (input$Prediction_var1=="Vector model" ||input$Prediction_var1=="Circular model"||input$Prediction_var1=="Elliptic model"
                                  ||input$Prediction_var1=="Quadratic model"||input$Prediction_var1=="Generalized Additive Model"||input$Prediction_var1=="Generalized Linear Model") &&
              input$Dimension_var1== "Principal Component Analysis"){
        map=map.with.pca(X)
        maps=cbind.data.frame(map$F1,map$F2)
        colnames(maps)=c("F1","F2")
        attach(maps )
        discretspace=discrete.function(map = maps)
        x.lm=predict.scores.lm(Y = Y,discretspace = discretspace,map = maps)
        x.vect=predict.scores.lm(Y = Y,formula="~F1+F2",discretspace = discretspace,map = maps)
        x.circ=predict.scores.lm(Y = Y,formula="~F1 + F2 + (F1*F1 + F2*F2)",discretspace = discretspace,map = maps)
        x.ellip=predict.scores.lm(Y = Y,formula="~I(F1*F1)+I(F2*F2)",discretspace = discretspace,map = maps)
        x.glm=predict.scores.glmulti(Y = Y,discretspace = discretspace,map = maps)
        x.gam=predict.scores.gam(Y = Y,discretspace = discretspace,map = maps)


        f.lm=extract.lm(x.lm,what=c("fstastic"))
        f.vect=extract.lm(x.vect,what=c("fstastic"))
        f.circ=extract.lm(x.circ,what=c("fstastic"))
        f.ellip=extract.lm(x.ellip,what=c("fstastic"))
        f.gam=extract.gam(x.gam,what=c("fstastic"))

        f.glm=extract.glm(x.glm,what=c("fstastic"))
        for (i in 1:length(f.glm)){ if (is.null(f.glm[[i]] )) f.glm[[i]]=0}

        x1=c(1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y))
        x2=c(f.lm,f.vect,f.circ,f.ellip,f.gam,f.glm)
        x3=c(rep("QR",ncol(Y)),rep("Vect",ncol(Y)),rep("Circ",ncol(Y)),rep("Ellip",ncol(Y)),rep("GAM",ncol(Y)),rep("GLM",ncol(Y)))
        x2=unlist(x2)

        bas5=data.frame(x3,x2)
        g25<-ggplot(bas5,aes(x=x3,y=x2,fill=x3))+geom_boxplot()+xlab("Models")+
          ylab("F-statistic")+ ggtitle( "Comparison of models with F-statistic ")
        g25<-g25+theme_bw()+theme(legend.position = "none",
                                axis.text.x = element_text(angle = 90),
                                plot.title = element_text(size=10, hjust = 0.5,face="bold"))
        g25+ylim(min=0, max=100)
      }
      else if(input$aic=="Fstat" && (input$Prediction_var1=="Vector model" ||input$Prediction_var1=="Circular model"||input$Prediction_var1=="Elliptic model"
                                    ||input$Prediction_var1=="Quadratic model"||input$Prediction_var1=="Generalized Additive Model"||input$Prediction_var1=="Generalized Linear Model") &&
                input$Dimension_var1== "Multiple Factor Analysis"){
        map=map.with.mfa(X,Y,axis=c(1,2))
        maps=cbind.data.frame(map$F1,map$F2)
        colnames(maps)=c("F1","F2")
        attach(maps )
        discretspace=discrete.function(map = maps)
        x.lm=predict.scores.lm(Y = Y,discretspace = discretspace,map = maps)
        x.vect=predict.scores.lm(Y = Y,formula="~F1+F2",discretspace = discretspace,map = maps)
        x.circ=predict.scores.lm(Y = Y,formula="~ F1 + F2 + (F1*F1 + F2*F2)",discretspace = discretspace,map = maps)
        x.ellip=predict.scores.lm(Y = Y,formula="~I(F1*F1)+I(F2*F2)",discretspace = discretspace,map = maps)
        x.glm=predict.scores.glmulti(Y = Y,discretspace = discretspace,map = maps)
        x.gam=predict.scores.gam(Y = Y,discretspace = discretspace,map = maps)


        f.lm=extract.lm(x.lm,what=c("fstastic"))
        f.vect=extract.lm(x.vect,what=c("fstastic"))
        f.circ=extract.lm(x.circ,what=c("fstastic"))
        f.ellip=extract.lm(x.ellip,what=c("fstastic"))
        f.gam=extract.gam(x.gam,what=c("fstastic"))

        f.glm=extract.glm(x.glm,what=c("fstastic"))
        for (i in 1:length(f.glm)){ if (is.null(f.glm[[i]] )) {f.glm[[i]]=0} }


        x2=c(f.lm,f.vect,f.circ,f.ellip,f.gam,f.glm)
        x3=c(rep("QR",ncol(Y)),rep("Vect",ncol(Y)),rep("Circ",ncol(Y)),rep("Ellip",ncol(Y)),rep("GAM",ncol(Y)),rep("GLM",ncol(Y)))
        x2=unlist(x2)


        bas6=data.frame(x3,x2)
        g26<-ggplot(bas6,aes(x=x3,y=x2,fill=x3))+geom_boxplot()+xlab("Models")+
          ylab("F-statistic")+ ggtitle( "Comparison of models with F-statistic ")
        g26<-g26+theme_bw()+theme(legend.position = "none",
                                  axis.text.x = element_text(angle = 90),
                                  plot.title = element_text(size=10, hjust = 0.5,face="bold"))
        g26+ylim(min=0, max=100)
      }
      else if(input$aic=="Fstat" && (input$Prediction_var1=="Vector model" ||input$Prediction_var1=="Circular model"||input$Prediction_var1=="Elliptic model"
                                    ||input$Prediction_var1=="Quadratic model"||input$Prediction_var1=="Generalized Additive Model"||input$Prediction_var1=="Generalized Linear Model") &&
                input$Dimension_var1== "Canonical Analysis"){
        ## If fstat and CA
        map=map.with.ca(X,S,Y)
        maps=cbind.data.frame(map$F1,map$F2)
        colnames(maps)=c("F1","F2")
        attach(maps )
        discretspace=discrete.function(map = maps)
        x.lm=predict.scores.lm(Y = Y,discretspace = discretspace,map = maps)
        x.vect=predict.scores.lm(Y = Y,formula="~F1+F2",discretspace = discretspace,map = maps)
        x.circ=predict.scores.lm(Y = Y,formula="~ F1 + F2 + (F1*F1 + F2*F2)",discretspace = discretspace,map = maps)
        x.ellip=predict.scores.lm(Y = Y,formula="~I(F1*F1)+I(F2*F2)",discretspace = discretspace,map = maps)
        x.glm=predict.scores.glmulti(Y = Y,discretspace = discretspace,map = maps)
        x.gam=predict.scores.gam(Y = Y,discretspace = discretspace,map = maps)



        f.lm=extract.lm(x.lm,what=c("fstastic"))
        f.vect=extract.lm(x.vect,what=c("fstastic"))
        f.circ=extract.lm(x.circ,what=c("fstastic"))
        f.ellip=extract.lm(x.ellip,what=c("fstastic"))
        f.gam=extract.gam(x.gam,what=c("fstastic"))

        f.glm=extract.glm(x.glm,what=c("fstastic"))
        for (i in 1:length(f.glm)){ if (is.null(f.glm[[i]] )) {f.glm[[i]]=0} }


        x2=c(f.lm,f.vect,f.circ,f.ellip,f.gam,f.glm)
        x3=c(rep("QR",ncol(Y)),rep("Vect",ncol(Y)),rep("Circ",ncol(Y)),rep("Ellip",ncol(Y)),rep("GAM",ncol(Y)),rep("GLM",ncol(Y)))
        x2=unlist(x2)



        bas7=data.frame(x3,x2)
        g27<-ggplot(bas7,aes(x=x3,y=x2,fill=x3))+geom_boxplot()+xlab("Models")+
          ylab("F-statistic")+ ggtitle( "Comparison of models with F-statistic ")
        g27<-g27+theme_bw()+theme(legend.position = "none",
                                  axis.text.x = element_text(angle = 90),
                                  plot.title = element_text(size=10, hjust = 0.5,face="bold"))
        g27+ylim(min=0, max=100)
      }

      ###########

      else if(input$aic=="nb-NA" && (input$Prediction_var1=="Vector model" ||input$Prediction_var1=="Circular model"||input$Prediction_var1=="Elliptic model"
                                  ||input$Prediction_var1=="Quadratic model"||input$Prediction_var1=="Generalized Additive Model"||input$Prediction_var1=="Generalized Linear Model") &&
              input$Dimension_var1== "Principal Component Analysis"){
        map=map.with.pca(X) # pour mfa et ca tu changes cette commande
        maps=cbind.data.frame(map$F1,map$F2)
        colnames(maps)=c("F1","F2")
        discretspace=discrete.function(map = maps)
        x.quad=predict.scores.lm(Y = Y,discretspace = discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", map = maps
                                 ,pred.na=TRUE)

        nb.quad=x.quad$nb.NA
        nb.quad=unlist(nb.quad)
        nb.quad=as.data.frame(nb.quad)



        #vect
        x.vect=predict.scores.lm(Y = Y,discretspace = discretspace,
                                 formula="~ F1 + F2", map = maps
                                 ,pred.na=TRUE)
        nb.vect=x.vect$nb.NA
        nb.vect=unlist(nb.vect)
        nb.vect=as.data.frame(nb.vect)

        #circ
        x.circ=predict.scores.lm(Y = Y,discretspace = discretspace,
                                 formula="~ F1 + F2 + (F1*F1 + F2*F2)", map = maps,
                                 pred.na=TRUE)
        nb.circ=x.circ$nb.NA
        nb.circ=unlist(nb.circ)
        nb.circ=as.data.frame(nb.circ)


        #ellip
        x.ellip=predict.scores.lm(Y = Y,discretspace = discretspace,
                                  formula="~I(F1*F1)+I(F2*F2)", map = maps,
                                  pred.na=TRUE)
        nb.ellip=x.ellip$nb.NA
        nb.ellip=unlist(nb.ellip)
        nb.ellip=as.data.frame(nb.ellip)

        # gam

        x.gam=predict.scores.gam(Y = Y,discretspace = discretspace,
                                 map = maps,
                                 pred.na=TRUE)


        nb.gam=x.gam$nb.NA
        nb.gam=unlist(nb.gam)
        nb.gam=as.data.frame(nb.gam)



        # glm
        x.glm=predict.scores.glmulti(Y = Y,discretspace = discretspace,
                                     map = maps,
                                     pred.na=TRUE)

        nb.glm=x.glm$nb.NA
        nb.glm=unlist(nb.glm)
        nb.glm=as.data.frame(nb.glm)



        x1=c(1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y))
        x2=c(nb.quad,nb.vect,nb.circ,nb.ellip,nb.gam,nb.glm)
        x2=melt(x2)
        x3=c(rep("QR",ncol(Y)),rep("Vect",ncol(Y)),rep("Circ",ncol(Y)),rep("Ellip",ncol(Y)),rep("GAM",ncol(Y)),rep("GLM",ncol(Y)))
        dt=cbind.data.frame(x1,x2[,1],x3)
        colnames(dt)=c("consumer","occur.na","model")

        gr2<-ggplot(dt,aes
                    (x=model,y=occur.na ,fill=model))+geom_boxplot()+xlab("Consumers")+ylab("Occurence of NA")+
          ggtitle( "Comparison of nb-NA of prediction models from PCA ")
        gr2+theme_bw()+ theme(plot.title = element_text(size=16, hjust = 0.5,face="bold"))

      }
      ##################

      else if(input$aic=="nb-NA" && (input$Prediction_var1=="Vector model" ||input$Prediction_var1=="Circular model"||input$Prediction_var1=="Elliptic model"
                                     ||input$Prediction_var1=="Quadratic model"||input$Prediction_var1=="Generalized Additive Model"||input$Prediction_var1=="Generalized Linear Model") &&
              input$Dimension_var1== "Multiple Factor Analysis"){
        map=map.with.mfa(X,Y,axis=c(1,2)) # pour mfa et ca tu changes cette commande
        maps=cbind.data.frame(map$F1,map$F2)
        colnames(maps)=c("F1","F2")
        discretspace=discrete.function(map = maps)
        x.quad=predict.scores.lm(Y = Y,discretspace = discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", map = maps
                                 ,pred.na=TRUE)

        nb.quad=x.quad$nb.NA
        nb.quad=unlist(nb.quad)
        nb.quad=as.data.frame(nb.quad)



        #vect
        x.vect=predict.scores.lm(Y = Y,discretspace = discretspace,
                                 formula="~ F1 + F2", map = maps
                                 ,pred.na=TRUE)
        nb.vect=x.vect$nb.NA
        nb.vect=unlist(nb.vect)
        nb.vect=as.data.frame(nb.vect)

        #circ
        x.circ=predict.scores.lm(Y = Y,discretspace = discretspace,
                                 formula="~ F1 + F2 + (F1*F1 + F2*F2)", map = maps,
                                 pred.na=TRUE)
        nb.circ=x.circ$nb.NA
        nb.circ=unlist(nb.circ)
        nb.circ=as.data.frame(nb.circ)


        #ellip
        x.ellip=predict.scores.lm(Y = Y,discretspace = discretspace,
                                  formula="~I(F1*F1)+I(F2*F2)", map = maps,
                                  pred.na=TRUE)
        nb.ellip=x.ellip$nb.NA
        nb.ellip=unlist(nb.ellip)
        nb.ellip=as.data.frame(nb.ellip)

        # gam

        x.gam=predict.scores.gam(Y = Y,discretspace = discretspace,
                                 map = maps,
                                 pred.na=TRUE)


        nb.gam=x.gam$nb.NA
        nb.gam=unlist(nb.gam)
        nb.gam=as.data.frame(nb.gam)



        # glm
        x.glm=predict.scores.glmulti(Y = Y,discretspace = discretspace,
                                     map = maps,
                                     pred.na=TRUE)

        nb.glm=x.glm$nb.NA
        nb.glm=unlist(nb.glm)
        nb.glm=as.data.frame(nb.glm)



        x1=c(1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y))
        x2=c(nb.quad,nb.vect,nb.circ,nb.ellip,nb.gam,nb.glm)
        x2=melt(x2)
        x3=c(rep("QR",ncol(Y)),rep("Vect",ncol(Y)),rep("Circ",ncol(Y)),rep("Ellip",ncol(Y)),rep("GAM",ncol(Y)),rep("GLM",ncol(Y)))
        dt=cbind.data.frame(x1,x2[,1],x3)
        colnames(dt)=c("consumer","occur.na","model")

        gr2<-ggplot(dt,aes
                    (x=model,y=occur.na ,fill=model))+geom_boxplot()+xlab("Consumers")+ylab("Occurence of NA")+
          ggtitle( "Comparison of nb-NA of prediction models from MFA ")
        gr2+theme_bw()+ theme(plot.title = element_text(size=16, hjust = 0.5,face="bold"))

      }

      ############

      else if(input$aic=="nb-NA" && (input$Prediction_var1=="Vector model" ||input$Prediction_var1=="Circular model"||input$Prediction_var1=="Elliptic model"
                                     ||input$Prediction_var1=="Quadratic model"||input$Prediction_var1=="Generalized Additive Model"||input$Prediction_var1=="Generalized Linear Model") &&
              input$Dimension_var1== "Canonical Analysis"){
        map=map.with.ca(X,S,Y) # pour mfa et ca tu changes cette commande
        maps=cbind.data.frame(map$F1,map$F2)
        colnames(maps)=c("F1","F2")
        discretspace=discrete.function(map = maps)
        x.quad=predict.scores.lm(Y = Y,discretspace = discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", map = maps
                                 ,pred.na=TRUE)

        nb.quad=x.quad$nb.NA
        nb.quad=unlist(nb.quad)
        nb.quad=as.data.frame(nb.quad)



        #vect
        x.vect=predict.scores.lm(Y = Y,discretspace = discretspace,
                                 formula="~ F1 + F2", map = maps
                                 ,pred.na=TRUE)
        nb.vect=x.vect$nb.NA
        nb.vect=unlist(nb.vect)
        nb.vect=as.data.frame(nb.vect)

        #circ
        x.circ=predict.scores.lm(Y = Y,discretspace = discretspace,
                                 formula="~ F1 + F2 + (F1*F1 + F2*F2)", map = maps,
                                 pred.na=TRUE)
        nb.circ=x.circ$nb.NA
        nb.circ=unlist(nb.circ)
        nb.circ=as.data.frame(nb.circ)


        #ellip
        x.ellip=predict.scores.lm(Y = Y,discretspace = discretspace,
                                  formula="~I(F1*F1)+I(F2*F2)", map = maps,
                                  pred.na=TRUE)
        nb.ellip=x.ellip$nb.NA
        nb.ellip=unlist(nb.ellip)
        nb.ellip=as.data.frame(nb.ellip)

        # gam
        x.gam=predict.scores.gam(Y = Y,discretspace = discretspace,
                                 map = maps,
                                 pred.na=TRUE)


        nb.gam=x.gam$nb.NA
        nb.gam=unlist(nb.gam)
        nb.gam=as.data.frame(nb.gam)



        # glm
        x.glm=predict.scores.glmulti(Y = Y,discretspace = discretspace,
                                     map = maps,
                                     pred.na=TRUE)

        nb.glm=x.glm$nb.NA
        nb.glm=unlist(nb.glm)
        nb.glm=as.data.frame(nb.glm)



        x1=c(1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y),1:ncol(Y))
        x2=c(nb.quad,nb.vect,nb.circ,nb.ellip,nb.gam,nb.glm)
        x2=melt(x2)
        x3=c(rep("QR",ncol(Y)),rep("Vect",ncol(Y)),rep("Circ",ncol(Y)),rep("Ellip",ncol(Y)),rep("GAM",ncol(Y)),rep("GLM",ncol(Y)))
        dt=cbind.data.frame(x1,x2[,1],x3)
        colnames(dt)=c("consumer","occur.na","model")

        gr2<-ggplot(dt,aes
                    (x=model,y=occur.na ,fill=model))+geom_boxplot()+xlab("Consumers")+ylab("Occurence of NA")+
          ggtitle( "Comparison of nb-NA of prediction models from CA ")
        gr2+theme_bw()+ theme(plot.title = element_text(size=16, hjust = 0.5,face="bold"))
      }
      })

    output$downloadPlot_aic <- downloadHandler(
      filename = function() {
        paste("PLOT", downloadPlotType(), sep=".")
      },
      # The argument content below takes filename as a function
      # and returns what's printed to it.
      content = function(con) {
        # Gets the name of the function to use from the
        # downloadFileType reactive element. Example:
        # returns function pdf() if downloadFileType == "pdf".
        plotFunction <- match.fun(downloadPlotType())
        plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
        print(cart14())
        dev.off(which=dev.cur())}
    )

   ###### EPM smoothing

    output$Dimension_var3<-renderUI({
      selectInput("Dimension_var3","Choose dimension reduction method:",choices = c("Principal Component Analysis", "Multiple Factor Analysis", "Canonical Analysis"))
    })

    output$Prediction_var3<-renderUI({
      selectInput("Prediction_var3","Choose prediction model:",choices = c("Vector model", "Circular model","Elliptic model",
                     "Quadratic model", "Generalized Additive Model" , "Generalized Linear Model", "Bayesian model","Others"))
    })

    output$mod2<-renderUI({
      if(input$Prediction_var3 == "Others"){
        selectInput("mod2","Choose type of model:",choices = c("Polynomial model","GAM" , "GLM", "Bayesian model"))}
      else return(NULL)
    })

    output$formula_lm2=renderUI({
      if(input$Prediction_var3 == "Others"){
        if(input$mod2 == "Polynomial model"){
          textInput("formula_lm2","Input formula for polynomial model :")}
        else if(input$mod2 == "GAM"){
          textInput("formula_lm2","Input formula for GAM :")}
        else if(input$mod2 == "GLM"){
          textInput("formula_lm2","Input formula for GLM :")}
        else if(input$mod2 == "Bayesian model"){
          textInput("formula_lm2","Input formula for Bayesian model :")}}
    })

    output$par_var3<-renderUI({
      if(input$Prediction_var3 != "Bayesian model"){
        selectInput("par_var3","If rejection of predictions outside of [0:10]:",choices = c("NO","YES"))}
      else{selectInput("par_var3","If rejection of predictions outside of [0:10]:",choices = c("NO"))}
    })

    output$interval3=renderUI({
      Y=consInputData()
      sliderInput("interval3", "Interval of Consumers",
                  min = 1, max = ncol(Y), value = c(1,40))

    })

    output$smooth=renderPlot({cart15()})
    cart15=eventReactive(input$actioncarto15,{
      Y=consInputData()
      X=moy2()
      S=moy4()
      nbpoints=50
      if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Vector model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
      b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                               formula="~ F1 + F2", dimredumethod=1,
                               predmodel=1, graphpred=FALSE, drawmap=TRUE,
                               pred.na=TRUE,dmap.loess=FALSE)

        }
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Circular model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~ F1 + F2 + (F1*F1 + F2*F2)", dimredumethod=1,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Elliptic model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)", dimredumethod=1,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Quadratic model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=1,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Generalized Additive Model" && input$par_var3=="YES")
      {par(mfrow = c(1,2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=1, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~s(F1,k=3)+s(F2,k=3)", dimredumethod=1,
                                 predmodel=2, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Generalized Linear Model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=1,
                                 predmodel=3, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}

      ###############
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Vector model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~ F1 + F2", dimredumethod=1,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Circular model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~ F1 + F2 + (F1*F1 + F2*F2)", dimredumethod=1,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Elliptic model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)", dimredumethod=1,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Quadratic model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=1,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Generalized Additive Model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=1, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~s(F1,k=3)+s(F2,k=3)", dimredumethod=1,
                                 predmodel=2, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Generalized Linear Model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=1,
                                 predmodel=3, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Bayesian model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y,X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y,S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=1,
                                 predmodel=4, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}

      ################

      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Vector model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~ F1 + F2", dimredumethod=2,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Circular model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~ F1 + F2 + (F1*F1 + F2*F2)", dimredumethod=2,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Elliptic model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)", dimredumethod=2,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Quadratic model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=2,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Generalized Additive Model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=2, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~s(F1,k=3)+s(F2,k=3)", dimredumethod=2,
                                 predmodel=2, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Generalized Linear Model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=2,
                                 predmodel=3, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}

      #######################

      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Vector model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~ F1 + F2", dimredumethod=2,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Circular model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~ F1 + F2 + (F1*F1 + F2*F2)", dimredumethod=2,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Elliptic model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)", dimredumethod=2,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Quadratic model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=2,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Generalized Additive Model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=2, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~s(F1,k=3)+s(F2,k=3)", dimredumethod=2,
                                 predmodel=2, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Generalized Linear Model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=2,
                                 predmodel=3, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Bayesian model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=2, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=2,
                                 predmodel=4, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}

      #################################
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Vector model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~ F1 + F2", dimredumethod=3,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)

      }
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Circular model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~ F1 + F2 + (F1*F1 + F2*F2)", dimredumethod=3,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Elliptic model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)", dimredumethod=3,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Quadratic model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=3,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Generalized Additive Model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=3, predmodel=2, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~s(F1,k=3)+s(F2,k=3)", dimredumethod=3,
                                 predmodel=2, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Generalized Linear Model" && input$par_var3=="YES")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=3, nbpoints=50,
                pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=3,
                                 predmodel=3, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}

      #################################
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Vector model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~ F1 + F2",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~ F1 + F2", dimredumethod=3,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Circular model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~ F1 + F2 + (F1*F1 + F2*F2)",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~ F1 + F2 + (F1*F1 + F2*F2)", dimredumethod=3,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Elliptic model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)", dimredumethod=3,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)
        }
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Quadratic model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=1, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=3,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Generalized Additive Model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~s(F1,k=3)+s(F2,k=3)",
                dimredumethod=3, predmodel=2, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~s(F1,k=3)+s(F2,k=3)", dimredumethod=3,
                                 predmodel=2, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Generalized Linear Model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=3, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=3,
                                 predmodel=3, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Bayesian model" && input$par_var3=="NO")
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=3, predmodel=4, nbpoints=50,
                pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula="~I(F1*F1)+I(F2*F2)+F1*F2", dimredumethod=3,
                                 predmodel=4, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}

      #######################
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Others" && input$par_var3=="NO" &&
             input$mod2=="Polynomial regression" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=1, predmodel=1, nbpoints=50,
                   pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=1,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Others" && input$par_var3=="YES" &&
              input$mod2=="Polynomial regression" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=1, predmodel=1, nbpoints=50,
                   pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=1,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Others" && input$par_var3=="NO" &&
              input$mod2=="GAM" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=1, predmodel=2, nbpoints=50,
                   pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=1,
                                 predmodel=2, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Others" && input$par_var3=="YES" &&
              input$mod2=="Generalized Additive Model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=1, predmodel=2, nbpoints=50,
                   pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=1,
                                 predmodel=2, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}

      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Others" && input$par_var3=="NO" &&
              input$mod2=="Generalized Linear Model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=1, predmodel=3, nbpoints=50,
                   pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=1,
                                 predmodel=3, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Others" && input$par_var3=="YES" &&
              input$mod2=="Generalized Linear Model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=1, predmodel=3, nbpoints=50,
                   pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=1,
                                 predmodel=3, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}

      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Others" && input$par_var3=="NO" &&
              input$mod2=="Bayesian model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=1, predmodel=4, nbpoints=50,
                   pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=1,
                                 predmodel=4, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Principal Component Analysis" && input$Prediction_var3=="Others" && input$par_var3=="YES" &&
              input$mod2=="Bayesian model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=1, predmodel=4, nbpoints=50,
                   pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=1,
                                 predmodel=4, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}

      #########################

      #######################
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Others" && input$par_var3=="NO" &&
              input$mod2=="Polynomial regression" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=2, predmodel=1, nbpoints=50,
                   pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=2,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Others" && input$par_var3=="YES" &&
              input$mod2=="Polynomial regression" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=2, predmodel=1, nbpoints=50,
                   pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=2,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Others" && input$par_var3=="NO" &&
              input$mod2=="Generalized Additive Model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=2, predmodel=2, nbpoints=50,
                   pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=2,
                                 predmodel=2, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Others" && input$par_var3=="YES" &&
              input$mod2=="Generalized Additive Model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=2, predmodel=2, nbpoints=50,
                   pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=2,
                                 predmodel=2, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}

      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Others" && input$par_var3=="NO" &&
              input$mod2=="Generalized Linear Model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=2, predmodel=3, nbpoints=50,
                   pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=2,
                                 predmodel=3, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Others" && input$par_var3=="YES" &&
              input$mod2=="Generalized Linear Model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=2, predmodel=3, nbpoints=50,
                   pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=2,
                                 predmodel=3, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}

      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Others" && input$par_var3=="NO" &&
              input$mod2=="Bayesian model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=2, predmodel=4, nbpoints=50,
                   pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=2,
                                 predmodel=4, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Multiple Factor Analysis" && input$Prediction_var3=="Others" && input$par_var3=="YES" &&
              input$mod2=="Bayesian model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=2, predmodel=4, nbpoints=50,
                   pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=2,
                                 predmodel=4, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      #########################

      #######################
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Others" && input$par_var3=="NO" &&
              input$mod2=="Polynomial regression" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=3, predmodel=1, nbpoints=50,
                   pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=3,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Others" && input$par_var3=="YES" &&
              input$mod2=="Polynomial regression" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=3, predmodel=1, nbpoints=50,
                   pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=3,
                                 predmodel=1, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Others" && input$par_var3=="NO" &&
              input$mod2=="Generalized Additive Model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=3, predmodel=2, nbpoints=50,
                   pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=3,
                                 predmodel=2, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Others" && input$par_var3=="YES" &&
              input$mod2=="Generalized Additive Model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=3, predmodel=2, nbpoints=50,
                   pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=3,
                                 predmodel=2, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}

      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Others" && input$par_var3=="NO" &&
              input$mod2=="Generalized Linear Model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=3, predmodel=3, nbpoints=50,
                   pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=3,
                                 predmodel=3, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Others" && input$par_var3=="YES" &&
              input$mod2=="Generalized Linear Model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=3, predmodel=3, nbpoints=50,
                   pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=3,
                                 predmodel=3, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}

      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Others" && input$par_var3=="NO" &&
              input$mod2=="Bayesian model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=3, predmodel=4, nbpoints=50,
                   pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=3,
                                 predmodel=4, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=FALSE,dmap.loess=FALSE)}
      else if(input$Dimension_var3=="Canonical Analysis" && input$Prediction_var3=="Others" && input$par_var3=="YES" &&
              input$mod2=="Bayesian model" )
      {par(mfrow = c(1, 2))
        a=drawmap (Y[,input$interval3[1]:input$interval3[2]],X,S,axis=c(1,2),formula=input$formula_lm2,
                   dimredumethod=3, predmodel=4, nbpoints=50,
                   pred.na =TRUE, graph.pred =FALSE, graph.map =TRUE,
                   graph.map.3D =FALSE )
        b=denoising.loess.global(X,Y[,input$interval3[1]:input$interval3[2]],S, axis=c(1,2), discretspace=discretspace,
                                 formula=input$formula_lm2, dimredumethod=3,
                                 predmodel=4, graphpred=FALSE, drawmap=TRUE,
                                 pred.na=TRUE,dmap.loess=FALSE)}


    })



   ##### comparison of maps stability

    output$reduction<-renderUI({
      selectInput("reduction","Choose dimension reduction method:",choices = c("Principal Component Analysis",
                                                              "Multiple Factor Analysis", "Canonical Analysis"))
    })


    output$Prediction_com<-renderUI({
      selectInput("Prediction_com","Choose prediction model:",choices = c("Vector model", "Circular model","Elliptic model",
                                                                           "Quadratic model", "Generalized Additive Model" , "Generalized Linear Model"))
    })

    output$formula_lm=renderUI({
      textInput("formula_lm","Input formula for Polynomial and GLM models :")

    })


    output$formula_gam=renderUI({
      textInput("formula_gam","Input formula for GAM :")

    })

    output$numm<-renderUI({
      numericInput("numm","Introduce number of sampling n (at least 2):", 2,
                   min = 2, max = 100)
    })

    output$comparaison=renderPlot({cart16()})
    cart16=eventReactive(input$actioncarto16,{
      Y=consInputData()
      X=moy2()
      S=moy4()
      # nbpoints=50
      if(input$reduction=="Principal Component Analysis" && (input$Prediction_com=="Vector model" ||input$Prediction_com=="Circular model"||input$Prediction_com=="Elliptic model"
                                                             ||input$Prediction_com=="Quadratic model"||input$Prediction_com=="Generalized Additive Model"||input$Prediction_com=="Generalized Linear Model"||input$Prediction_com=="Bayesian model")){
        res= Dist_prob(Y,X,S,n=input$numm,axis=c(1,2),formula_lm=input$formula_lm,
                       formula_gam=input$formula_gam,dimredumethod=1,
                       nbpoints=50)

        res=melt(res)
        colnames(res)=c("prob", "var", "value")
        gr<-ggplot(res,aes(x=prob,y=value,fill=prob))+geom_boxplot()+xlab("Methods")+
          ylab("Difference Sum Squares of Preferences")
        gr<-gr+theme_bw()+theme(legend.position = "none",
                                axis.text.x = element_text(angle = 90))

        gr
      }

      else if(input$reduction=="Multiple Factor Analysis" && (input$Prediction_com=="Vector model" ||input$Prediction_com=="Circular model"||input$Prediction_com=="Elliptic model"
                                                             ||input$Prediction_com=="Quadratic model"||input$Prediction_com=="Generalized Additive Model"||input$Prediction_com=="Generalized Linear Model"||input$Prediction_com=="Bayesian model")){
        res= Dist_prob(Y,X,S,n=input$numm,axis=c(1,2),formula_lm=input$formula_lm,
                       formula_gam=input$formula_gam,dimredumethod=2,
                       nbpoints=50)

        res=melt(res)
        colnames(res)=c("prob", "var", "value")
        gr<-ggplot(res,aes(x=prob,y=value,fill=prob))+geom_boxplot()+xlab("Methods")+
          ylab("Difference Sum Squares of Preferences")
        gr<-gr+theme_bw()+theme(legend.position = "none",
                                axis.text.x = element_text(angle = 90))

        gr
      }

      else if(input$reduction=="Canonical Analysis" && (input$Prediction_com=="Vector model" ||input$Prediction_com=="Circular model"||input$Prediction_com=="Elliptic model"
                                                              ||input$Prediction_com=="Quadratic model"||input$Prediction_com=="Generalized Additive Model"||input$Prediction_com=="Generalized Linear Model"||input$Prediction_com=="Bayesian model")){
        res= Dist_prob(Y,X,S,n=input$numm,axis=c(1,2),formula_lm=input$formula_lm,
                       formula_gam=input$formula_gam,dimredumethod=3,
                       nbpoints=50)

        res=melt(res)
        colnames(res)=c("prob", "var", "value")
        gr<-ggplot(res,aes(x=prob,y=value,fill=prob))+geom_boxplot()+xlab("Methods")+
          ylab("Difference Sum Squares of Preferences")
        gr<-gr+theme_bw()+theme(legend.position = "none",
                                axis.text.x = element_text(angle = 90))

        gr
      }
    })

    output$downloadPlot_comp <- downloadHandler(
      filename = function() {
        paste("comp", downloadPlotType(), sep=".")
      },
      # The argument content below takes filename as a function
      # and returns what's printed to it.
      content = function(con) {
        # Gets the name of the function to use from the
        # downloadFileType reactive element. Example:
        # returns function pdf() if downloadFileType == "pdf".
        plotFunction <- match.fun(downloadPlotType())
        plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
        print(cart16())
        dev.off(which=dev.cur())}
    )


    output$dend<-renderPlot({
      df=consInputData()
      res.pca=PCA(t(df),graph = F)
      res.hcpc=HCPC(res.pca,graph = F)
      plot.HCPC(res.hcpc,choice="tree",new.plot=F)
    })

    clus1=reactive({
      df=consInputData()
      res.pca=PCA(t(df),graph = F)
      res.hcpc=HCPC(res.pca,graph = F)
      fviz_cluster(res.hcpc) + theme_minimal()+ ggtitle("Clustering Plot")
    })

     output$clus<-renderPlot({
     clus1()
    })


    output$downloadclus <- downloadHandler(
      filename = function() {
        paste("Plot", downloadPlotType(), sep=".")
      },
      # The argument content below takes filename as a function
      # and returns what's printed to it.
      content = function(con) {
        # Gets the name of the function to use from the
        # downloadFileType reactive element. Example:
        # returns function pdf() if downloadFileType == "pdf".
        plotFunction <- match.fun(downloadPlotType())
        plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
        print(clus1())
        dev.off(which=dev.cur())}
    )


    dend11<-reactive({
      df=consInputData()
      res.pca=PCA(t(df),graph = F)
      res.hcpc=HCPC(res.pca,graph = F)
      b=res.hcpc$call$t$tree
      c=as.dendrogram(b)
      nb=res.hcpc$call$t$nb.clust
        d=c %>%
      set("branches_k_color", k = nb) %>% set("branches_lwd", 0.7) %>%
        set("labels_cex", 0.6) %>% set("labels_colors", k = nb) %>%
        set("leaves_pch", 10) %>% set("leaves_cex", 0.5)%>%raise.dendrogram(0.015)%>%remove_nodes_nodePar
      ggd1 <- as.ggdend(d)
     ggplot(ggd1,labels=F)+ggtitle("Dendrogram")+scale_y_continuous(0,1.2)
    plot.HCPC(res.hcpc,choice="tree",new.plot=F)
    })

    output$dend1<-renderPlot({
      dend11()
    })
    output$downloaddend <- downloadHandler(
      filename = function() {
        paste("Plot", downloadPlotType(), sep=".")
      },

      content = function(con) {

        plotFunction <- match.fun(downloadPlotType())
        plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
        print(dend11())
        dev.off(which=dev.cur())}
    )

    output$dend12<-renderPrint({
      df=consInputData()
      res.pca=PCA(t(df),graph = F)
      res.hcpc=HCPC(res.pca,graph = F)
      res.hcpc$desc.var
          })


})
