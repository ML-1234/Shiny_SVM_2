library(shiny)
library(randomForest)
library(unbalanced)
library(e1071)
library(caret)
library(class)
library(ggplot2)
library(corrplot)
library(ROCR)
library(xgboost)


shinyServer(function(input, output) {

  
  
  output$intro <- renderText({
    paste( " <br> <br>
           Les <b>Support Vector Machines</b> (SVM) représentent une méthode statistique développée dans les années 1990.<br> 
           Cette méthode est destinée à résoudre des problèmes de classification puisqu’elle va permettre de déterminer si un élément appartient ou non à une classe.
           <br> <br>")
  })
  
  output$intro2 <- renderText({
    paste( "<br> <br> Pour mieux comprendre son fonctionnement, il est utile de s’intéresser à sa représentation graphique.
           Pour cela, on dispose d’un ensemble de données. <br>
           Notre but va être de chercher à les séparer en deux groupes distincts. <br>
           Un groupe représente ainsi la survenance de l’évènement (prévision 1) et l’autre la non-survenance (prévision 0). <br>
           Cette séparation linéaire va se faire à l’aide d’une frontière appelée <b>hyperplan</b>. <br> <br>
           ")
  })
  #Bdd linéairement séparable
  n <- 500
  delta <- 0.13
  df_linear <- data.frame(x1 = runif(n), x2 = runif(n))
  df_linear$y <- factor(ifelse(df_linear$x2 - 1.4*df_linear$x1 < 0, -1, 1), levels = c(-1, 1))
  df_linear<- df_linear[abs(1.4*df_linear$x1 - df_linear$x2) > delta, ]
  
  
  output$plot_linear <- renderPlot({
    
    plot_margins <- ggplot(data = df_linear, aes(x = x1, y = x2, color = y)) + geom_point() + 
      scale_color_manual(values = c("red", "blue")) + theme_bw() +
      geom_abline(slope = 1.4, intercept = 0)+
      geom_abline(slope = 1.4, intercept = delta, linetype = "dashed") +
      geom_abline(slope = 1.4, intercept = -delta, linetype = "dashed")
    
    
    plot_margins
  })
  
  output$vs <- renderText({
    paste( " <br> <br>Il existe de nombreux hyperplans séparateurs.
           L’algorithme SVM va nous aider à trouver l'optimal, celui qui maximise la séparation en classant correctement toutes les observations. <br> 
           Pour le trouver, il suffit de chercher l’hyperplan pour lequel la distance entre la frontière des deux groupes et l’observation la plus proche est maximale. <br>
           
           Le double de cette distance est appelée <b>marge</b>. On parlera donc de maximisation de la marge. <br>
           Il en résulte que les observations les plus proches de la frontière, appelées <b> vecteurs de supports </b>, sont les points situés sur la marge. <br> <br>")
  })
  
  output$plot_linear_SVM <- renderPlot({
    
    svm_model<- svm(y ~ .,data = df_linear,type = "C-classification", kernel = "linear", scale = FALSE)
    
    SVM_plot <- ggplot(data = df_linear, aes(x = x1, y = x2, color = y)) + 
      geom_point() +
      scale_color_manual(values = c("red", "blue")) + geom_point(data = df_linear[svm_model$index, ], aes(x = x1, y = x2), color = "purple", size = 4, alpha = 0.2) + theme_bw()
    
    SVM_plot
    
  })
  
  
  #Bdd presque linéairement séparable
  
  output$cout <- renderText({
    paste( " <br> <br> Cependant, il arrive souvent que l’on soit face à des échantillons non linéairement séparables.
           Dans cette situation, deux cas de figure apparaissent. <br> <br>")
  })
  
  output$cout2 <- renderText({
    paste( " <br> Le premier est que la séparation optimale reste linéaire malgré le fait que quelques observations ne puissent pas être correctement classées. <br> <br>")
  })
  
  output$plot_almostlinear_SVM <- renderPlot({
    delta <- 0.03
    df_linear <- data.frame(x1 = runif(n), x2 = runif(n))
    df_linear$y <- factor(ifelse(df_linear$x2 - 1.4*df_linear$x1 < 0, -1, 1), levels = c(-1, 1))
    df_linear<- df_linear[abs(1.4*df_linear$x1 - df_linear$x2) > delta, ]
    
    plot_margins_almostlinear <- ggplot(data = df_linear, aes(x = x1, y = x2, color = y)) + geom_point() + 
      scale_color_manual(values = c("red", "blue")) + theme_bw() +
      geom_abline(slope = 1.4, intercept = 0)+
      geom_abline(slope = 1.4, intercept = delta, linetype = "dashed", colour="orange") +
      geom_abline(slope = 1.4, intercept = -delta, linetype = "dashed", colour="orange") +
      geom_abline(slope = 1.4, intercept = 0, colour="orange") +
      geom_abline(slope = 1.4, intercept = delta*3, linetype = "dashed") +
      geom_abline(slope = 1.4, intercept = -delta*3, linetype = "dashed")
    
    plot_margins_almostlinear
  })
  
  output$vr <- renderText({
    paste( "<br> <br> Pour définir le nombre d'observations mal classées autorisé <b>(variable ressort)</b>, on fait appel à un <b>paramètre de pénalisation</b> qui est le <b>coût</b>.<br>
           On l'utilise car les performances des SVMs y sont très sensibles. <br>
           Ce paramètre permet l’acceptation d'un certain nombre de variables ressorts dans le but de maximiser la marge. <br>
           
           Cependant, il faut être prudent car lorsqu'on choisit un coût élevé, cela signifie que peu d’erreurs de classification sont acceptées et donc que la marge sera plus petite.
           Dans ce cas, on fait face à un risque de <b>sur-apprentissage</b>. <br>
           Dans la situation inverse, lorsque le coût est faible, la priorité est donnée à la maximisation de la marge, au préjudice de la minimisation du nombre d’erreurs de classification. 
           On est alors face à un risque de <b>sous-apprentissage</b>.  <br>
           L'objectif est alors de trouver un arbitrage entre l’optimisation de la marge et le nombre d'erreurs de classification. <br> <br> ")
  })
  
  output$vr2 <- renderText({
    paste( "<br>  Le deuxième cas de figure apparaît lorsque l’échantillon n’est pas linéairement séparable. <br> <br>")
  })
  #Bdd radialement séparable
  
  output$plot_radial_SVM <- renderPlot({
    df <- data.frame(x1 = runif(n, min = -1, max = 1), 
                     x2 = runif(n, min = -1, max = 1))
    
    radius <- 0.8
    radius_squared <- radius^2
    
    df$y <- factor(ifelse(df$x1^2 + df$x2^2 < radius_squared, -1, 1), levels = c(-1, 1))
    
    scatter_plot <- ggplot(data = df, aes(x = x1, y = x2, color = y)) + 
      geom_point() + theme_bw() +
      scale_color_manual(values = c("red", "blue"))
    
    scatter_plot
    
  })
  output$fin <- renderText({
    paste( "<br>  Ici on constate que la séparation linéaire n’est pas possible.<br> 
           Afin de trouver la séparation optimale on va alors chercher à <b>transformer l’espace de représentation des données d’entrée</b> en un espace de plus grandes dimensions en rajoutant des variables explicatives créées à partir de la transformation des variables initiales.<br>
           Cette transformation se fait à l’aide des <b>fonctions kernels</b>. Elles sont très utile puisque l’on n’a pas besoin de connaître la transformation à appliquer.<br>
           Dans ce nouvel espace de plus grande dimension, il sera alors plus probable de trouver une séparation linéaire. <br> <br> <br>
           ")
    
  })
  
})
  
