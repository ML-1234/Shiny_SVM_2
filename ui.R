library(shiny)
library(shinythemes)


shinyUI(fluidPage(theme = shinytheme("flatly"),
                  
                  # Titre de l'application
                  titlePanel(img(src="En-tête.png", width="100%")),
                  navbarPage("Sommaire",
                             
                             tabPanel("Préambule",
                                      sidebarLayout(
                                        sidebarPanel(
                                          h3("Notice d'utilisation"),
                                          downloadButton("notice",
                                                         "Télécharger la notice")),
                                        mainPanel(
                                          HTML("<br/>"),
                                          h3("Présentation de l'application"),
                                          div(htmlOutput("pre",align="justify",width=500,height = 400))
                                        ))),
                             
                             
                             
                             tabPanel("Base de données",
                                      tabsetPanel(
                                        tabPanel("Présentation",
                                                 absolutePanel(
                                                   h2("Origine et objectif"),
                                                   div(htmlOutput("pa1"), align="justify"),
                                                   div(htmlOutput("pa2"), align="justify"),
                                                   div(htmlOutput("pa3"), align="justify"),
                                                   
                                                   
                                                   h2("Exploration de la table", align="justify"),
                                                   HTML("\n<br/>"),
                                                   div(DT::dataTableOutput("table"), align="center"),
                                                   
                                                   h2("Prédicteurs"),
                                                   div(htmlOutput("pa4"), align="justify"),
                                                   
                                                   
                                                   sidebarLayout(
                                                     sidebarPanel(
                                                       selectInput("var",label="Choisissez une variable :",choice=c( "V1"=1,     "V2"=2,     "V3"=3,     "V4"=4,     "V5"=5,     "V6"=6,     "V7"=7,    "V8"=8,    "V9"=9,     "V10"=10,    "V11"=11,    "V12"=12,    "V13"=13,    "V14"=14,   "V15"=15,    "V16"=16,   "V17"=17,   "V18"=18,   
                                                                                                                     "V19"=19,   "V20"=20,    "V21"=21,    "V22"=22,    "V23"=23,   "V24"=24,   "V25"=25,    "V26"=26,    "V27"=27,    "V28"=28,    "Amount"=29, "Time"=30), selectize=FALSE),
                                                       submitButton("Mise à jour")),
                                                     
                                                     mainPanel(
                                                       h3("Statistiques descriptives", align="center"),
                                                       div(verbatimTextOutput("sum"), align="center"),
                                                       div(plotOutput("dist"), align="center")
                                                     )
                                                   ),
                                                   
                                                   
                                                   left="20%", right="20%" )
                                        ),
                                        
                                        
                                        tabPanel("Traitement",
                                                 absolutePanel(
                                                   h2("Données asymétriques"),
                                                   div(htmlOutput("p1"), align="justify"),
                                                   
                                                   h3("Distribution de la variable cible avant traitement", align="center"),
                                                   div(plotOutput("g1", height=500, width=400), align="center"),
                                                   
                                                   div(htmlOutput("p2a"), align="justify"),
                                                   div(htmlOutput("p2b"), align="left"),
                                                   div(htmlOutput("p2c"), align="justify"),
                                                   
                                                   h2("Traitements appliqués"),
                                                   div(htmlOutput("p3"), align="justify"),
                                                   
                                                   
                                                   h2("Présentation de la nouvelle base d’apprentissage"),
                                                   div(htmlOutput("p4"), align="justify"),
                                                   
                                                   h3("Distribution de la variable cible après traitement", align="center"),
                                                   div(plotOutput("g4", height=500, width=400), align="center"),
                                                   
                                                   
                                                   left="20%", right="20%" )
                                        ))),
                             
                             
                             tabPanel("Support Vector Machine",
                                      tabsetPanel(
                                        tabPanel("Principe",
                                                 absolutePanel(
                                                   div(htmlOutput("intro"), align="justify"),
                                                   
                                                   
                                                   h3("Cas linéairement séparable"),
                                                   
                                                   div(htmlOutput("intro2"), align="justify"),
                                                   div(plotOutput("plot_linear", height = 300, width = 400), align="center"),
                                                   
                                                   div(htmlOutput("vs"), align="justify"),
                                                   div(plotOutput("plot_linear_SVM", height = 300, width = 400),align="center"),
                                                   
                                                   div(htmlOutput("cout"), align="justify"),
                                                   
                                                   
                                                   h3("Cas presque linéairement séparable"),
                                                   
                                                   div(htmlOutput("cout2"), align="justify"),
                                                   div(plotOutput("plot_almostlinear_SVM", height = 300, width = 400), align="center"),
                                                   
                                                   div(htmlOutput("vr"), align="justify"),
                                                   
                                                   h3("Cas non linéairement séparable"),
                                                   
                                                   div(htmlOutput("vr2"), align="justify"),
                                                   div(plotOutput("plot_radial_SVM", height = 300, width = 400), align="center"),
                                                   
                                                   div(htmlOutput("fin"), align="justify"),
                                                   
                                                   left="20%", right = "20%")
                                        ),
                                        tabPanel("Méthodes",
                                                 h1("Application de la méthode des SVM"),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     helpText("Choisissez vos paramètres :"),
                                                     radioButtons("noyau","Fonction noyau", choices=c("Linéaire"="linear","Polynomial"="polynomial","Base radiale" ="radial","Sigmoïde"="sigmoid"),selected="linear"),
                                                     selectInput("cout", "Coût de pénalisation", choices = c(0.125,0.25,0.5,1,2,4,8), selected = 1),
                                                     submitButton("Mise à jour")
                                                   ),
                                                   
                                                   mainPanel(
                                                     div(HTML("&nbsp;&nbsp;&nbsp;Comme nous l'avons vu précédemment, les techniques SVM (non linéaires) font appel à une fonction implicite transformant l’espace d’entrée.
                                                     La méthode requiert donc de sélectionner un noyau. <br> <br>
                                                     &nbsp;&nbsp;&nbsp;En réalisant les transformations de variables adéquates, on peut ainsi rendre linéairement séparable un problème qui ne l’est pas dans l’espace initial. <br> <br>
                                                     <ul>
                                                     <li> <strong> Noyau :</strong> aide à projeter des données dans un espace dimensionnel supérieur où les points peuvent être séparés linéairement.</li>
                                                     <li> <strong> Coût de pénalisation :</strong> permet de pénaliser les erreurs plus ou moins fortement selon le niveau de coïncidence souhaité des données d’apprentissage. </li>
                                                     </ul>
                                                     "), align="justify"),
                                                  
                                                     div(plotOutput("m_svm", height = 400, width = 500), align="center")
                                                   )),
                                                 
                                                 
                                                 h1("Méthodes concurrentes"),
                                                 h3("Régression logistique"),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     helpText("Choisissez votre paramètre :"),
                                                     numericInput("cutoff", label = "Cut-off", min=0.1,max=0.9,value=0.5,step=0.1),
                                                     submitButton("Mise à jour")
                                                   ),
                                                   
                                                   mainPanel(
                                                     div(HTML("&nbsp;&nbsp;&nbsp;La régression logitique permet de mesurer l’association entre la variable expliquée qualitative et les variables explicatives.
                                                     Chacune des variables explicatives va être pondérée par son coefficient trouvé afin de donner au final la meilleure prédiction. <br> <br>
                                                    <ul>
                                                    <li> <strong> Cut-off :</strong> seuil qui va permettre de déterminer un équilibre entre le taux de faux positifs et le taux de faux négatifs. </li>
                                                    </ul>
                                                          "), align="justify"),
                                                     
                                                     div(plotOutput("confusion_RL", height = 400, width = 500), align="center")
                                                     
                                                   )),
                                                 
                                                 h3("Random Forest"),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     helpText("Choisissez vos paramètres :"),
                                                     sliderInput("mtry","Nombre de variables sélectionnées",min=0, max=30, value=2), 
                                                     sliderInput("ntree","Nombre d'arbres dans la forêt",min=0, max=500, value=90),
                                                     submitButton("Mise à jour")
                                                   ),
                                                   mainPanel(
                                                     div(HTML("&nbsp;&nbsp;&nbsp;La forêt aléatoire (ou Random Forest) est constituée d'un grand nombre d'arbres de décision individuels qui sont construits sur des échantillons différents.
                                                     Chaque arbre individuel de la forêt prévoit le non-défaut ou le défaut (0 ou 1) et la classe majoritaire devient la prédiction de notre modèle. <br> <br>
                                                     <ul>
                                                     <li> <strong> Nombre de variables sélectionnées :</strong> les variables sont sélectionnées aléatoirement parmi les variables explicatives pour construire chaque arbre. </li>
                                                     <li> <strong> Nombre d’arbres dans la forêt :</strong> plus le nombre d’arbres sera grand, et plus votre prédiction sera robuste.</li>
                                                     </ul> "), align="justify"),
                                                     
                                                     
                                                     div(plotOutput("confusion_rf", height = 400, width = 500), align="center")
                                                     
                                                       )),
                                                 
                                                 h3("Gradient Boosting"),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     helpText("Choisissez vos paramètres :"),
                                                     sliderInput("max_prof","Profondeur maximale de l'arbre", min=1, max=20,value=3),
                                                     sliderInput("skrinkage", "Paramètre de lissage",min=0,max=1,value=0.3),
                                                     submitButton("Mise à jour")
                                                   ),
                                                   mainPanel(
                                                     div(HTML("&nbsp;&nbsp;&nbsp;La méthode du Gradient Boosting consiste à utiliser plusieurs modèles que nous agrégeons ensuite pour obtenir un seul résultat. <br> <br>
                                                     &nbsp;&nbsp;&nbsp;Dans la construction des modèles, le Boosting commence par construire un premier modèle qu’il va évaluer. 
                                                     A partir de cette mesure, chaque individu va être pondéré en fonction de la performance de la prédiction. <br> 
                                                     \n<br/>&nbsp;&nbsp;&nbsp; L’objectif est de donner un poids plus important aux individus pour lesquels la valeur a été mal prédite pour la construction du modèle suivant. 
                                                     Le fait de corriger les poids au fur et à mesure permet de mieux prédire les valeurs difficiles. <br> <br>
                                                     <ul>
                                                     <li> <strong> Profondeur maximale de l’arbre :</strong> si l’arbre est petit il sera résistant au sur-apprentissage, mais on aura un danger de sous-apprentissage.  Inversement si l’arbre est grand. </li>
                                                     <li> <strong> Paramètre de lissage (shrinkage) : </strong> s'il est trop faible, on aura une lenteur de convergence car les corrections seront timides. S’il est trop élevé,  on a des oscillations et donc sur-apprentissage. Bonne valeur usuelle autour de 0.1.</li>
                                                     </ul> "), align="justify"),
                                                     
                                                     div(plotOutput("m_gb", height = 400, width = 500), align="center")
                                                     
                                                   )
                                                 )),
                                                 
                                                 
                                        tabPanel("Comparaison et Conclusion", 
                                                 absolutePanel(
                                                 div(htmlOutput("comp1"), align="justify"),
                                                 splitLayout(cellWidths = c("38%", "62%"), div(plotOutput("roc", height=320, width=345), align="center"), div(plotOutput("prcurve", height=320, width=565), align="center")),
                                                 
                                                 div(htmlOutput("comp2"), align="justify"),
                                                 h3("Tableau de comparaison", align="center"),
                                                 div(tableOutput("ma_table"),align="center"),
                                                 
                                                 div(htmlOutput("comp3"), align="justify"),
                                                 
                                                 left="20%", right = "20%")
                                        ))
                             ))))