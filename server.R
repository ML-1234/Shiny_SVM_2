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
  ####Base de données####
  bdd <- readRDS(file = 'base.rds')
  
  bdd$Class <- as.factor(bdd$Class)
  form <- Class~.
  
  idx <- sample(1:nrow(bdd), as.integer(0.75*nrow(bdd)))
  train <- bdd[idx, ]
  test <- bdd[-idx, ]
  
  X <- train[,-ncol(train)]
  Y <- train$Class
  newData <- ubBalance(X, Y, type="ubUnder", positive=1, perc=8, method="percPos")
  train_ub <- as.data.frame(cbind(newData$X, newData$Y))
  colnames(train_ub)[colnames(train_ub)=="newData$Y"] <- "Class"
  
  
  ####Fonctions#####
  ### Fonction des P-values des corrélations ###
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  
  
  #Fonction taux d'erreur
  erreur<-function(matrice){
    paste(round((matrice$table[[2]]+matrice$table[[3]])/sum(matrice$table)*100, 3),"%")
    
  }
  ### Couleur des modèles ###
  cols <- c('Support Vector Machine'= '#6A4A3C', 'Régression Logistique'= '#00A0B0', 'KNN' = '#CC333F', 'Random Forest'= '#EB6841', 'Gradient Boosting' = '#EDC951')
  
  ###Création Matrice de confusion en Plot###
  draw_confusion_matrix <- function(cm, color) {
    
    layout(matrix(c(1,1,2)))
    par(mar=c(2,2,2,2))
    plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
    title('MATRICE DE CONFUSION', cex.main=2)
    
    # Création de la matrice
    rect(150, 430, 240, 370, col=color)
    text(195, 435, 'Pas de défaut', cex=1.2)
    rect(250, 430, 340, 370, col='white')
    text(295, 435, 'Défaut', cex=1.2)
    text(125, 370, 'Prédiction', cex=1.3, srt=90, font=2)
    text(245, 450, 'Observation', cex=1.3, font=2)
    rect(150, 305, 240, 365, col='white')
    rect(250, 305, 340, 365, col=color)
    text(140, 400, 'Pas de défaut', cex=1.2, srt=90)
    text(140, 335, 'Défaut', cex=1.2, srt=90)
    
    # Ajout des informations de la matrice 
    res <- as.numeric(cm$table)
    text(195, 400, res[1], cex=1.6, font=2, col='white')
    text(195, 335, res[2], cex=1.6, font=2, col='black')
    text(295, 400, res[3], cex=1.6, font=2, col='black')
    text(295, 335, res[4], cex=1.6, font=2, col='white')
    
    # Ajout des statistiques : sensitivité, spécificité, précision
    plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "STATISTIQUES", cex.main=1.8, xaxt='n', yaxt='n')
    text(10, 85, "Sensitivité", cex=1.2, font=2)
    text(10, 70, paste(round(as.numeric(cm$byClass[1])*100, 3), '%'), cex=1.2)
    text(50, 85, "Spécificité", cex=1.2, font=2)
    text(50, 70, paste(round(as.numeric(cm$byClass[2])*100, 3), '%'), cex=1.2)
    text(90, 85, "Précision", cex=1.2, font=2)
    text(90, 70, paste(round(as.numeric(cm$byClass[5])*100, 3), '%'), cex=1.2)
    
    
    # Ajout des taux d'exactitude et d'erreur
    text(30, 35, "Taux d'exactitude", cex=1.5, font=2)
    text(30, 20, paste(round(as.numeric(cm$overall[1])*100, 3), '%'), cex=1.4)
    text(70, 35, "Taux d'erreur", cex=1.5, font=2)
    text(70, 20, paste(round((1 - sum(diag(cm$table))/sum(cm$table))*100, 3),"%"), cex=1.4)
  }
  
  
  output$pre <- renderText({
    paste( "<br> <br> Dans le cadre de notre cursus universitaire, nous avons mis en place un démonstrateur sous R Shiny afin de montrer l'implémentation 
           et les performances des machines à vecteurs de support dans la détection des transactions frauduleuses commises sur les cartes de crédit.<br> <br>
           Avant de commencer, il est important pour nous de remercier M. HURLIN, créateur de ce projet et professeur de SVM, M. DELSOL, professeur de R Shiny ainsi que  M. DUDEK pour son intervention sur le déploiement d'applications Shiny sous Github.<br> <br>
           Dès à présent, afin de comprendre le fonctionnement de ce démonstrateur, nous vous invitons à télécharger la notice située dans l'onglet suivant.")
  })
  
  output$report <- downloadHandler(
    filename = "Notice.html",
    content = function(file) {
      # Document temporaire crée par copie, modifié avec les nouvelles valeurs des paramètres et replacé
      tempReport <- file.path(tempdir(), "Notice.Rmd")
      file.copy("Notice.Rmd", tempReport, overwrite = TRUE)
      
      # Ici on met les paramètres à rendre réactifs dans le document
      # params <- list(n = input$slider)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
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
  
  # SVM
  ##Modèle
  
  svm.fit <- reactive({svm(form,data=train_ub,type="C-classification",kernel=input$noyau,cost=input$cout,probability=TRUE)})
  
  svm.pred <- reactive({predict(svm.fit(), test, probability=TRUE)})
  
  cmsvm <- reactive({pred <- svm.pred()
  confusionMatrix(test$Class, pred)})
  
  ##Matrice de confusion
  output$m_svm <- renderPlot({
    draw_confusion_matrix(cmsvm(), cols[1])
  })
  
  
  # Régression logistique
  ##Modèle
  
  glm.fit <- reactive({glm(form,data=train_ub,family="binomial")})
  glm.prob <- reactive({predict(glm.fit(), test, type="response")})
  
  cmrl <- reactive({glm.pred <- factor(ifelse(glm.prob()>0.5, 1,0))
  confusionMatrix(glm.pred, test$Class)})
  
  ##Matrice de confusion
  output$confusion_RL <- renderPlot({
    draw_confusion_matrix(cmrl(), cols[2])
  })

  
  # RandomForest
  ## Modèle et matrice simple
  rf.fit <- reactive({randomForest(form,train_ub, mtry=input$mtry,ntree=input$ntree)})
  rf.pred <- reactive({predict(rf.fit(),test,type="response")})
  cmrf <- reactive({confusionMatrix(rf.pred(), test$Class)})
  
  output$selected_param <- renderText({ 
    paste( "Vous avez choisi le nombre de feuilles égales à", input$mtry, "et un nombre d'arbres égal à", input$ntree,". <br> <br>")
  })
  
  # output$optimal <- renderText({ 
  #   paste( "<br> Les paramètres optimaux qui permettent de minimiser le taux d'erreur sont de", mtry_opt, "pour le nombre de feuilles et",ntree_opt, "pour le nombre d'arbres dans la forêt.")
  # })
  
  ##Matrice de confusion
  output$confusion_rf <- renderPlot({draw_confusion_matrix(cmrf(), cols[4])})
  
  output$erreur_rf <- renderText({
    taux_erreur <- paste(round((1 - sum(diag(cmrf()$table))/sum(cmrf()$table))*100, 3),"%")
    paste( "L'erreur est de", taux_erreur,".")
  })
  
  
  # Gradient Boosting
  ##Modèle
  boost.fit <- reactive({

    
    TrainData <- as.matrix(train_ub[,-31] )
    TrainClasses <- as.matrix(train_ub[,31])
    
    xgboost(data=TrainData,label=TrainClasses, eta=input$skrinkage, nrounds=50, max_depth=input$max_prof,verbose=0)})
  
  boost.pred <- reactive({predict(boost.fit(), newdata=as.matrix(test[,-31]))})
  
  cmgb <- reactive({
    boost.pred.class <- factor(ifelse(boost.pred()>0.5, 1,0))
    test$Class <- as.factor(test$Class)
    train_ub$Class <- as.factor(train_ub$Class)
    confusionMatrix(test$Class, boost.pred.class)})
  
  # #Texte optimal
  # output$optimal_gb <- renderText(
  #   paste( "Les paramètres optimaux qui permettent de minimiser le taux d'erreur sont de", max_prof_opt, "pour la profondeur maximale de l'arbre et de",shrinkage_opt, "pour le paramètre de lissage. <br> <br>")
  # )
  ##Matrice de confusion
  output$m_gb <- renderPlot({draw_confusion_matrix(cmgb(), cols[5])})
  
  #### AJOUT COMPARAISON : ROC ####
  #Courbe ROC
  output$roc <-renderPlot({
    
    ##SVM
    svm.fit.prob <-attr(svm.pred(),"probabilities")
    ROCRpred_svm <- prediction(svm.fit.prob[,2], test$Class)
    perf_svm <- ROCR::performance(ROCRpred_svm, 'tpr','fpr')
    roc_svm.data <- data.frame(fpr=unlist(perf_svm@x.values),
                               tpr=unlist(perf_svm@y.values), model="Support Vector Machine")
    
    ##Régression logistique
    ROCRpred_glm <- prediction(glm.prob(), test$Class)
    perf_glm <- ROCR::performance(ROCRpred_glm, 'tpr','fpr')
    roc_glm.data <- data.frame(fpr=unlist(perf_glm@x.values),
                               tpr=unlist(perf_glm@y.values), model="Régression logistique")
    
    ##Gradient Boosting
    ROCRpred_gb <- prediction(boost.pred(), test$Class)
    perf_gb <- ROCR::performance(ROCRpred_gb, 'tpr','fpr') 
    roc_gb.data <- data.frame(fpr=unlist(perf_gb@x.values),
                              tpr=unlist(perf_gb@y.values), model="Gradient Boosting")
    
    ##Random Forest 
    train_ub$Class <- ifelse(train_ub$Class==1, 1,0)
    test$Class <- ifelse(test$Class==1, 1,0)
    rf.prob <- predict(rf.fit(),test,type="prob")
    ROCRpred_rf <- prediction(rf.prob[,2], test$Class)
    perf_rf <- ROCR::performance(ROCRpred_rf, 'tpr','fpr') 
    roc_rf.data <- data.frame(fpr=unlist(perf_rf@x.values),
                              tpr=unlist(perf_rf@y.values), model="Random Forest")
    
    ##Ensemble
    ggplot() + 
      geom_line(data = roc_glm.data, aes(x=fpr, y=tpr, colour = "Régression Logistique")) + 
      geom_line(data = roc_rf.data, aes(x = fpr, y=tpr, colour = "Random Forest")) +
      geom_line(data = roc_gb.data, aes(x = fpr, y=tpr, colour = "Gradient Boosting")) +
      geom_line(data = roc_svm.data, aes(x = fpr, y=tpr, colour = "Support Vector Machine")) +
      
      geom_abline(color = "darkgrey", linetype=2) + theme_bw() + 
      scale_colour_manual(name = "Modèles", values = cols) + 
      xlab("Taux de Faux positifs") +
      ylab("Taux de Vrais positifs") +
      theme(legend.position = c(0.8, 0.2), 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15))
    
  })
  #### AJOUT COMPARAISON : ROC ####
  output$ma_table <- renderTable({
    #Régression logistique
    ROCRpred_glm <- prediction(glm.prob(), test$Class)
    AUC_glm <- ROCR::performance(ROCRpred_glm, 'auc')
    AUC_glm <- paste(round(unlist(AUC_glm@y.values)*100, 3),"%")
    
    #Random Forest
    train_ub$Class <- ifelse(train_ub$Class==1, 1,0)
    test$Class <- ifelse(test$Class==1, 1,0)
    rf.prob <- predict(rf.fit(),test,type="prob")
    ROCRpred_rf <- prediction(rf.prob[,2], test$Class)
    AUC_rf <- ROCR::performance(ROCRpred_rf, measure='auc')
    AUC_rf <- paste(round(unlist(AUC_rf@y.values)*100, 3),"%")
    
    #SVM
    svm.fit.prob <-attr(svm.pred(),"probabilities")
    ROCRpred_svm <- prediction(svm.fit.prob[,2], test$Class)
    AUC_svm <- ROCR::performance(ROCRpred_svm,'auc')
    AUC_svm <- paste(round(unlist(AUC_svm@y.values)*100, 3),"%")
    
    #Gradient Boosting
    ROCRpred_gb <- prediction(boost.pred(), test$Class)
    AUC_gb <- ROCR::performance(ROCRpred_gb, 'auc')
    AUC_gb <- paste(round(unlist(AUC_gb@y.values)*100, 3),"%")
    
    
    
    df <- data.frame(SVM=c(erreur(cmsvm()),AUC_svm), RL=c(erreur(cmrl()),AUC_glm), GB=c(erreur(cmgb()),AUC_gb), RF=c(erreur(cmrf()),AUC_rf))
    rownames(df)<-c("Taux d'erreur", "AUC")
    colnames(df) <- c("Support Vector Machine","Régression Logistique","Gradient Boosting","Random Forest")
    df
    
    
  },digits=4, striped = TRUE, bordered = TRUE, rownames=TRUE,width=600) 
  
  output$p1 <- renderText({paste("\n", "&nbsp; &nbsp; Dans la base de données <em> creditcard </em>, les cas de défaut ne représentent que <strong> 0.1727486% </strong> des observations.
                                       Nos données sont donc largement asymétriques, comme nous le montre le graphique suivant,
                                       dans lequel la colonne des cas de défaut est presque invisible.", "\n","\n",
                                 sep="<br/>")})
  
  
  output$g1 <- renderPlot({ggplot(bdd, aes(Class)) + geom_bar(fill = c("#0073C2FF","#ffa500")) +
      labs(x = " ", y = " ") + 
      scale_x_discrete(labels=c("Non-défaut", "Défaut")) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "italic"))})
  
  
  output$p2a <- renderText({paste("\n", "&nbsp; &nbsp; Lors de la modélisation, l’asymétrie des données peut fausser le résultat. 
                                                     En effet, la classification repose sur un mécanisme de minimisation du taux d’erreur et sur une hypothèse de bonne 
                                                     représentation de la population, mais le manque d’observations de la classe minoritaire ne donne pas assez d’informations au modèle
                                                     pour bien apprendre de ces données.",
                                  "\n","&nbsp; &nbsp; Ainsi, il pourra avoir un très bon taux d'exactitude en classant tous les individus dans la classe largement majoritaire.",
                                  "\n","&nbsp; &nbsp; On aura donc à la fois un problème de représentativité de la population par l’échantillon et avec l’aspect minimiseur 
                                               du taux d’erreur de l’algorithme du modèle en lui-même.",
                                  "\n","&nbsp; &nbsp; Il se dessine alors deux méthodes de traitement des données asymétriques : changer l’algorithme ou rééquilibrer les données 
                                               en utilisant des <strong> stratégies d’échantillonnage </strong>. C’est cette dernière technique que nous avons choisi de développer ici.",
                                  "\n","Dans les stratégies d’échantillonnage, il existe deux manières de procéder :",
                                  sep="<br/>")})
  
  output$p2b <- renderText({paste("&nbsp;- Le <strong> sur-échantillonnage </strong> : consiste à augmenter le nombre d’observations de la classe minoritaire en créant 
                                                         des observations artificielles.",
                                  "&nbsp;- Le <strong> sous-échantillonnage </strong> : enlève des observations de la classe majoritaire. 
                                                         Le choix des observations à supprimer peut se faire aléatoirement ou selon des critères spécifiques.", sep="<br/>")})
  
  output$p2c <- renderText({paste("\n","&nbsp; &nbsp; En général, le sur-échantillonnage est préféré car il ne suppose pas la perte d’une partie des données, 
                                               mais le sous-apprentissage peut aussi aider lorsque l’échantillon est considéré trop large.",
                                  "\n","&nbsp; &nbsp; Afin de ne pas surmener l’application Shiny, nous avons préféré appliquer la méthode de sous-échantillonnage, 
                                               et garder ainsi un échantillon d’apprentissage avec moins d’observations.", 
                                  sep="<br/>")})
  
  
  output$p3 <- renderText({paste("\n", "&nbsp; &nbsp; Avant d’appliquer un quelconque traitement sur nos données, nous avons extrait de la base de données un <strong> échantillon 
                                                     de validation </strong>, afin de pouvoir vérifier la classification sur un échantillon qui a gardé l’asymétrie d’origine. ",
                                 "\n", "&nbsp; &nbsp; Toujours dans l’objectif de garder une application la plus fluide possible, nous avons retenu la méthode de 
                                                     sous-échantillonnage la plus simple : le <strong> sous-échantillonnage aléatoire </strong>, qui retire aléatoirement des observations de la classe majoritaire.",
                                 "\n", "&nbsp; &nbsp; Afin de garder un nombre significatif d’observations ainsi que le caractère asymétrique de la base de données initiale 
                                                     dans l’échantillon, nous avons arbitrairement choisi de d’augmenter à <strong> 8% </strong> la part de de cas de défaut dans l’échantillon d’apprentissage.", 
                                 sep="<br/>")})
  
  
  output$p4 <- renderText({paste("\n", "&nbsp; &nbsp; La nouvelle base a", nrow(train_ub), "observations dont", sum(train_ub$Class==1), "cas de défaut et",sum(train_ub$Class==0), "cas de non-défaut.",  "\n","\n")})
  
  
  output$g4 <- renderPlot({ggplot(train_ub, aes(Class)) + geom_bar(fill = c("#0073C2FF","#ffa500")) +
      labs(x = " ", y = " ") + 
      scale_x_discrete(labels=c("Non-défaut", "Défaut")) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "italic"))})
  
  
  output$sum2 <- renderPrint({summary(train_ub[,as.numeric(input$var2)])})
  
  output$box2 <- renderPlot({
    x<-summary(train_ub[,as.numeric(input$var2)])
    boxplot(x,horizontal=TRUE,col="brown",main=names(train_ub[,as.numeric(input$var2)]))
    #Dboxplot(x,col="sky blue",border="purple",main=names(train_ub[,as.numeric(input$var1)]))
  })
  
  output$cor2 <- renderPlot({
    train_ub_corr <- as.data.frame(lapply(train_ub, as.numeric))
    train_ub_M <-cor(train_ub_corr)
    p.mat <- cor.mtest(train_ub_corr)
    
    corrplot(train_ub_M, type="upper", p.mat = p.mat, sig.level = 0.05)
  })
  
})


  
