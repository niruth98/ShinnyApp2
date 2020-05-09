shinyServer(function(input, output) {
  fetch_data = eventReactive(input$plot, {
    line_df<<-NULL
    box_df<<-NULL
    f1_df<<- NULL
    nf<-input$Features
    ns <- input$sim
    line_df <<- data.frame(n= 10:nf)
    box_df <<- data.frame(n=1:ns)
    f1_df <<- data.frame(n=1:ns)
    print(dim(f1_df))
    print(dim(box_df))
    val <-input$checkGroup
    if (length(val)==1){
      mod <- map_mod(val)
      mk <- mod[[1]]
      mn <- mod[[2]]
      get_df(nf,mk,mn,ns)
    }
    else if(length(val)==2){
      mod <- map_mod(val)
      mk1 <- mod[[1]]
      mn1 <- mod[[2]]
      mk2 <- mod[[3]]
      mn2 <- mod[[4]]
      get_df(nf,mk1,mn1,ns)
      get_df(nf,mk2,mn2,ns)
    }

  })

  output$lineplot = renderPlot({
    fetch_data()
    d <- melt(line_df, id.vars="n")
    g1<-ggplot(d, aes(n,value, col=variable)) +
      geom_point()+
      geom_line()+
      labs(color='Model')+
      xlab('Number of Genes Selected')+
      ylab('Accuracy')+
      # scale_color_brewer(palette = 'Set1')+
      scale_color_manual(values=c('#2E84D5','#C84B4B'))+
      ggtitle('Accuracy based on number of Features')+
      theme_classic()+
      theme(plot.title = element_text(hjust=0.5))
    g1

  })


  output$combined = renderPlot({
    fetch_data()
    g5<-ggplot(melt(f1_df[-1]),aes(y='',x=value,fill=variable))+
      geom_boxplot()+
      coord_flip()+
      guides(fill=FALSE)+
      xlab('F1 score')+
      ylab('')+
      scale_fill_manual(values=c('#2E84D5','#C84B4B'))+
      scale_x_continuous(labels = scales::percent)+
      # ggtitle('Distribution of F1 Score ')+
      theme_classic()
    # theme(plot.title = element_text(hjust=0.5))
    g4<-ggplot(melt(box_df[-1]),aes(y='',x=value,fill=variable))+
      geom_boxplot()+
      coord_flip()+
      labs(fill='Model')+
      xlab('Accuracy')+
      ylab('')+
      scale_fill_manual(values=c('#2E84D5','#C84B4B'))+
      scale_x_continuous(labels = scales::percent)+
      # ggtitle('Distribution of Accuracy')+
      theme_classic()
    # theme(plot.title = element_text(hjust=0.5))
    gc<-ggarrange(g4,g5, ncol=2,common.legend = TRUE,legend = 'bottom')
    annotate_figure(gc,top = text_grob("Accuracy and F1 Score for slected number of features",  face = "bold", size = 10))
  })



  output$pca = renderPlot({
    x <- fetch_data()
    gse_pca2 <- prcomp(t(x))
    df_toplot2 <- data.frame(rejection_status,
                             pc1 = gse_pca2$x[,1], pc2 = gse_pca2$x[,2],pc3=gse_pca2$x[,3]  )


    g1 <- ggplot(df_toplot2, aes(x = pc1, y = pc2, color = rejection_status)) +
      geom_point() +
      scale_color_manual(values=c('#2E84D5','#C84B4B'))+
      theme_classic()+
      xlab('PC1')+
      ylab('PC2')+
      labs(color='Rejection Status')+
      ggtitle("PCA Distribution of Stable vs Non-Stable patients based on Feature selection")
    # ggtitle('Distribution of Stable vs Non-Stable patients based on selected number of Features')+
    theme(plot.title = element_text(hjust=0.5))
    g1
  })

  output$ex = DT::renderDataTable({
    x <- fetch_data()
    DT::datatable(x[1:5],options = list(searching=FALSE))
  })
})