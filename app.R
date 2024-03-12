#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggfortify)#
library(dplyr)# j'ai utilisé la fonction group_by 
library(ggplot2)#pour tracer les diagramme circulaire
library(shinydashboard)
library(DT)#cree des tableau interactif
library(readr)#lire et ecrire dans les fichier csv
library(gmodels)#Matrice de confusion
library(shiny)
library(rpart)#arbre de decision
library(rpart.plot)
library(nnet)#reseau de neurone
library(arules)
library(e1071)#SVM
library(DMwR2)#plus voisin
library(neuralnet)



datas <- read.table("ObesityDataSet.csv", header=T, dec='.',na.strings = c('?'), sep=',')
datasup <- datas
datasup[ , c('Gender','family_history_with_overweight','FAVC','CAEC','SMOKE','SCC','CALC','MTRANS')] <- list(NULL)
datac<-datas[ , c('Gender','family_history_with_overweight','FAVC','CAEC','SMOKE','SCC','CALC','MTRANS')]

#Preparation des données pour la classification ascendante hierarchique
print(datasup)
i=c(-1,-5, -6, -9, -10,  -12, -15,-16, -17, -18)
sup <-datas[,i]
sup <- sup[,-14]
str(sup)
#centrer et reduire les données 
df.cr = scale(as.matrix(sup), center = TRUE, scale = TRUE)
row.names(df.cr)<-datas[,2]#ici, attribution de la classe Nr..Hotel.review comme etiquette aux données centrés et reduites 
print(df.cr)
#matrice de Distance entre les individus
df.dist <- dist(df.cr, method = "euclidean")

#modelsvm <- svm(NObeyesdad~.,datasup)



shinyApp(
  ui = dashboardPage(
    dashboardHeader(
      title = h2("TP DE DATAMINING"),
      titleWidth = 400
      
    ),
    dashboardSidebar(
      sidebarMenu(
        
        menuItem("HOME", tabName = "manipulation de donnees", icon = icon("home")),
        
        menuItem("manipulation de donnees", tabName = "Manipulation de donnees", icon = icon("gears"),
                 menuSubItem("Donnee initial",tabName = "err", icon = icon("dashboard")),
                 menuSubItem("Suppression des donnees",tabName = "dfd", icon = icon("trash")),
                 menuSubItem("Valeurs manquantes",tabName = "dhdh", icon = icon("filter"))
                 ),
        
        menuItem("visualisation de donnees", tabName = "Visualisation de donnees", icon = icon("eye"),
                 menuSubItem("Statistiques",tabName = "ery", icon = icon("chart-line")),
                 menuSubItem("Histogrammes",tabName = "eri", icon = icon("chart-simple")),
                 menuSubItem("Diagramme en batons",tabName = "ert", icon = icon("square-poll-vertical")),
                 menuSubItem("Boite a moutache",tabName = "erp", icon = icon("square")),
                 menuSubItem("Nuage de points",tabName = "er", icon = icon("cloud")),
                 menuSubItem("Diagramme circulaire",tabName = "ert1", icon = icon("chart-pie"))
                 ),
        
        menuItem("extraction de donnees",icon = icon("database"),
                 tabName = "edd"
                 ),
        
        menuItem("classification supervisee",tabName = "classification supervisee",icon = icon("cubes"),
                 menuSubItem("Reseau de neuronnes",tabName = "eer", icon = icon("code-merge")),
                 menuSubItem("Plus proche voisins",tabName = "dfv", icon = icon("person")),
                 menuSubItem("SVM",tabName = "df", icon = icon("dashboard")),
                 menuSubItem("Arbre de decision",tabName = "erk", icon = icon("tree"))
                 )
        
        
        
        )
    ),
    dashboardBody(
      tabItems(
        
        #presentation des donnees initailes
        tabItem(
          "Donnee initial",tabName = "err",
          DTOutput("donne_initial"),
          downloadButton('save_data', 'save to csv')),
        #suppression de donnees
        tabItem(
          "Suppression des donnees",tabName = "dfd",
          h2('Suppression des attributs peu pertinente'),
          h4('Les attributs que nous jugeons peu pertinente pour notre analyse sont les attributs suivant: Gender,family_history_with_overweight,FAVC,CAEC,SMOKE,SCC,CALC,MTRANS,NObeyesdad' ),
          DTOutput("donne_new"),
          downloadButton('save_data1', 'save to csv')
              ),
        
        #remplacement des valeurs manquantes
        tabItem(
          "Remplacement des valeurs manquantes",tabName = "dhdh",
          h2("Notre jeu de donnnes ne possedent pas de valeurs manquantes")
        ),
        
        #statistiques
        tabItem(
          "Statistique",tabName = "ery",
          h2("Statistiques", align = 'center'),
          verbatimTextOutput('statistique')
        ),
        
        
        #histogrammes
        tabItem(
          "Histogrammes",tabName = "eri",
          h2("Histogramme", align = 'center'),
          selectInput('varh', 'Choisis une variable numerique :', choices = names(datasup)),
          plotOutput("hist")
        ),
        
        #nuage de points
        tabItem(
          "Nuage de points",tabName = "er",
          h2("Nuage des points", align = 'center'),
          selectInput('var1', 'Choisis une variable numerique :', choices = names(datasup)),
          selectInput('var2', 'Choisis une 2e variable numerique :', choices = names(datasup)),
          plotOutput("nuage")
        ),
        
        #boite a moustache
        tabItem(
          "Boite a moutache",tabName = "erp",
          h2("Boite a moustache", align = 'center'),
          selectInput('varb', 'Choisis une variable numerique :', choices = names(datasup)),
          plotOutput('Boite')
          
        ),
        
        #diagramme en batons
        tabItem(
          "Diagramme en batons",tabName = "ert",
          h2("Diagramme en baton", align = 'center'),
          selectInput('vard', 'Choisis une variable numerique :', choices = names(datasup)),
          plotOutput('baton')
        ),
        #diagramme circulaire
        tabItem(
          "Diagramme en cercle",tabName = "ert1",
          h2("Diagramme en cercle", align = 'center'),
          selectInput('vard1', 'Choisis une variable numerique :', choices = names(datac)),
          plotOutput('cercle')
        ),
        
        #extraction de donnees
        tabItem(
          "extraction de donnees",tabName = "edd",
          h2("Affichage des regles d'association", align = 'center'),
          verbatimTextOutput('regles'),
          h2("Commentaires des regles d'association", align = 'center')
        ),
        
        #arbre de decision
        tabItem(
          "Arbre de decision et Matrice de confusion",tabName = "erk",
          h2("Arbre de decision", align = 'center'),
          plotOutput('arbre'),
          h2("Matrice de confusion", align = 'center'),
          verbatimTextOutput('MatriceA'),
          verbatimTextOutput('evalA')
          
           ),
        
        #Reseau de neurones
        tabItem(
          "Reseau de neurones",tabName = "eer",
          h2("Reseau de neurones", align = 'center'),
          numericInput('varR', "Entrer le nombre K de couche cachee", value = 1, max = 100, step  = 1),
          verbatimTextOutput('neurone'),
          h2("Matrice de confusion", align = 'center'),
          verbatimTextOutput('MatriceR'),
          verbatimTextOutput('evalB')
        ),
        
        #Plus proche voisin
        tabItem(
          "Plus proche voisin",tabName = "dfv",
          h2("Plus proche voisin", align = 'center'),
          numericInput('varP', "Entrer le nombre de poid de K", value = 1, max = 100, step  = 1),
          plotOutput('voisin'),
          h2("Matrice de confusion", align = 'center'),
          verbatimTextOutput('MatriceP'),
          verbatimTextOutput('evalC')
        ),
        
        #SVM
        tabItem(
          "SVM",tabName = "df",
          h2("SVM", align = 'center'),
          verbatimTextOutput('SVM'),
          h2("Matrice de confusion", align = 'center'),
          verbatimTextOutput('MatriceS'),
           verbatimTextOutput('evalD')
        ),
        
        
        #Methode Kmeans
        tabItem(
          "Methode Kmeans",tabName = "Kmeans",
          h2("Methode Kmeans", align = 'center'),
          numericInput('varK', "Entrer le nombre de clusters", value = 1, max = 100, step  = 1),
          plotOutput('Kmeans')
        ),
        
        #Classification Hierachique
        tabItem(
          "Classification Hierachique ascendante",tabName = "hier",
          h2("Classification Hierachique ascendante", align = 'center'),
          numericInput('varH', "Entrer le nombre de groupe", value = 2, max = 500, step  = 1),
          plotOutput('Hierachique')
        )
        
       ),
      #h2("bienvenue dans l'application de la classification des données fait par l'etudiant LONTSI LAMBOU RONALDINO en option data science niveau 3")
      
      
      ),
      
      
     
      
    
    title = "Analyse de l'obésité",
    skin = "yellow"
  ),
  
  
  
  server = function(input, output) {
    #charge les donnee
    df <- reactive({
      datas
    })
    #affiche les donnee
    output$donne_initial <- renderDT({
      df()
    })
    df1 <- reactive({
      datasup
    })
    output$donne_new <- renderDT({
      df1()
    })
    #sauvegarde de la df au format csv
    output$save_data <- downloadHandler(
      filename <- function(){
        paste("data_", Sys.Date(), ".csv", sep = ',')
      },
      content <- function(file){
        write.csv(df(), file)
      }
    )
    output$save_data1 <- downloadHandler(
      filename <- function(){
        paste("New_data_", Sys.Date(), ".csv", sep = ',')
      },
      content <- function(file){
        write.csv(df1(), file)
      }
    )
    #Resumer statistique
    output$statistique <- renderPrint({
      summary(datasup)
    })
    #Boite a moustache
    output$Boite <- renderPlot({
      
      boxplot(datasup[,input$varb], main = 'Boite a moustache', xlab = input$varb)
      abline(datasup)
    })
    #Diagramme en baton
    output$baton <- renderPlot({
      
      barplot(datasup[,input$vard], main = 'Diagramme en baton', xlab = input$vard)
    })
    #Histogramme
    output$hist <- renderPlot({
      hist(datasup[,input$varh], main = 'Histogramme', xlab = input$varh)
    })
    #Nuage de point
    output$nuage <- renderPlot({
      plot(datasup[,input$var1], datasup[,input$var2],
           xlab = input$var1, ylab = input$var2)
    })
    #recupere la colonne pour afffiche le diagramme en cercle
    observeEvent(input$vard1 ,{
      choices<-input$vard1
    })
    #affichage du diagramme en cercle
output$cercle <- renderPlot({
  req(input$vard1)
  
  # Calculer les pourcentages
  data_summary <- datac %>%
    group_by(!!as.name(input$vard1)) %>%
    summarize(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)

  # Créer le diagramme en secteur
  ggplot(data_summary, aes(x = "", y = percentage, fill = !!as.name(input$vard1))) + 
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    labs(fill = input$vard1) +
    theme_minimal() +
    geom_text(aes(label = paste0(round(percentage, 1), "%")),
              position = position_stack(vjust = 0.5)) +
    ylim(c(0, 100))  # Ajuster les limites y pour s'assurer que les étiquettes sont affichées correctement
})

 output$regles <- renderPrint({
  dataset = read.transactions('ObesityDataSet.csv', rm.duplicates = TRUE)
  #cr=read.table("ObesityDataSet.csv",header =TRUE,dec="." );
  #dataset=as(cr,"transactions")
  set.seed = 220 # Setting seed
  associa_rules = apriori(data = dataset, parameter = list(support = 0.00005, confidence = 0.0009))
  #print(str(associa_rules))
  # Vérifier le nombre de règles

  #print(length(associa_rules))
   inspect(associa_rules)
  
  #inspect(sort(associa_rules, by = 'lift')[1:10]))
})

    
    #datasupProche = datas[ , c('User.country','Score','Period.of.stay','NObeyesdad','Traveler.type','Pool','Gym','Tennis.court','Spa','Casino','Free.internet','Nr..rooms','User.continent','Review.month','Review.weekday')] <- list(NULL)
    datasup$NObeyesdad = as.factor(datasup$NObeyesdad)#Transformer la classe en variable categorielle
    nt = sample(1:nrow(datasup), 0.7*nrow(datasup))#choisir aleatoirement les 70% des lignes de iris
    train = datasup[nt,]#nouveau jeu de donnee
    test = datasup[-nt,]#pour le jeu de test
    
    #arbre de decision
    output$arbre<-renderPlot({
      train$NObeyesdad = as.factor(train$NObeyesdad)
      AD = rpart(NObeyesdad~.,data=train)
      #Plot de l'arbre de decision
      rpart.plot(AD)
    })
    #evaluation du modeles
    output$MatriceA<- renderPrint({
      AD = rpart(NObeyesdad~.,data=train)
      pred = predict(AD, test, type = c("class"))#valeur predite du jeu de donnee test
      M = CrossTable(pred, test[,4])
    })
    output$evalA <- renderPrint(
      {
        AD = rpart(NObeyesdad~.,data=train)
        pred = predict(AD, test, type = c("class"))#valeur predite du jeu de donnee test
        M = table(pred, test[,4])
        eval <- function(M){
         gener = round(sum(diag(M)) / sum(M), digits = 6)
          n = dim(M)
          i=0
          rappel =  0
          for (i in 1:n) {
            rappel = M[i,i]/sum(M[i,]) + rappel
          }
          rappel = round(rappel / n, digits = 6)
          pres = 0
          m = dim(M)
          for (i in 1:m) {
            pres = M[i,i]/sum(M[,i]) + pres
          }
          pres = round(pres / m, digits = 6)
          measures = paste("Accuracy:", gener,
                 "Rappel (Recall):", rappel,
                 "Précision (Precision):", pres)
          return(measures)
        }
        eval(M)
      }
    )
    
    
    #SVM
    output$SVM<-renderPrint({
      #SVM et test
      
      #dt = subset(train, select = c(4))
      modelsvm <- svm(NObeyesdad~., data = train, scale = FALSE)
      modelsvm
    })
    output$MatriceS<- renderPrint({
      dt = subset(train, select = c(4))
      modelsvm <- svm(NObeyesdad~., data = train, scale = FALSE)
      predn = predict(modelsvm, test ,type = c("class"))
      M = CrossTable(predn, test[,4])
    })
    output$evalD<- renderPrint(
      {
        dt = subset(train, select = c(4))
        modelsvm <- svm(NObeyesdad~., data = train, scale = FALSE)
        predn = predict(modelsvm, test ,type = c("class"))
        M = table(predn, test[,4])
        eval <- function(M){
         gener = round(sum(diag(M)) / sum(M), digits = 6)
          n = dim(M)
          i=0
          rappel =  0
          for (i in 1:n) {
            rappel = M[i,i]/sum(M[i,]) + rappel
          }
          rappel = round(rappel / n, digits = 6)
          pres = 0
          m = dim(M)
          for (i in 1:m) {
            pres = M[i,i]/sum(M[,i]) + pres
          }
          pres = round(pres / m, digits = 6)
          measures = paste("Accuracy:", gener,
                 "Rappel (Recall):", rappel,
                 "Précision (Precision):", pres)
          return(measures)
        }
        eval(M)
      }
    )
    #Plus proche voisin
   output$voisin <- renderPlot({
  knn = kNN(NObeyesdad~., train, test, stand = FALSE, k = input$varP)
  plot(knn)
})

output$MatriceP <- renderPrint({
  knn = kNN(NObeyesdad~., train, test, stand = FALSE, k = input$varP)
  M = CrossTable(knn, test[,2])
})

output$evalC <- renderPrint({
  knn <- kNN(NObeyesdad ~ ., train, test, stand = FALSE, k = input$varP)
  M <- table(knn, test[, 2])
  print(str(M))
  eval <- function(M) {
    gener <- round(sum(diag(M)) / sum(M), digits = 6)
    n <- dim(M)[1]
    rappel <- 0
    for (i in 1:n) {
      rappel <- M[i, i] / sum(M[i, ]) + rappel
    }
    rappel <- round(rappel / n, digits = 6)
    pres <- 0
    m <- dim(M)[2]
    for (i in 1:m) {
      pres <- M[i, i] / sum(M[, i]) + pres
    }
    pres <- round(pres / m, digits = 6)
    measures <- paste("Accuracy:", gener,
                      "Rappel (Recall):", rappel,
                      "Précision (Precision):", pres)
    return(measures)
  }

  eval(M)
})
    
    #Reseau de neurone
    output$neurone<-renderPrint({
      #Reseau de neurone et test
      nn <- nnet(NObeyesdad ~., train, size = input$varR)#reseau de neurone
      nn
    })
    output$MatriceR<- renderPrint({
      nn <- nnet(NObeyesdad ~., train, size = input$varR)#reseau de neurone
      predn = predict(nn, test,type = c("class"))
      M =CrossTable(predn, test[,2])
    })
     output$evalB<- renderPrint(
      {
        nn <- nnet(NObeyesdad ~., train, size = input$varR)#reseau de neurone
        predn = predict(nn, test,type = c("class"))
        M = table(predn, test[,2])
        eval <- function(M){
         gener = round(sum(diag(M)) / sum(M), digits = 6)
          n = dim(M)
          i=0
          rappel =  0
          for (i in 1:n) {
            rappel = M[i,i]/sum(M[i,]) + rappel
          }
          rappel = round(rappel / n, digits = 6)
          pres = 0
          m = dim(M)
          for (i in 1:m) {
            pres = M[i,i]/sum(M[,i]) + pres
          }
          pres = round(pres / m, digits = 6)
          measures = paste("Accuracy:", gener,
                 "Rappel (Recall):", rappel,
                 "Précision (Precision):", pres)
          return(measures)
        }
        eval(M)
      }
    )
     
    #Kmeans
    output$Kmeans <- renderPlot({
      #NOMBRE DE CLUSTERS DYNAMIQUE 
      df.kmeans <- kmeans(df.dist,input$varK)
      attributes(df.kmeans)
      df.kmeans$cluster
      table(df.kmeans$cluster)
      #plot(df.kmeans$cluster)
      #AFFICHAGE DU GRAPHE
      autoplot(df.kmeans, df.dist,frame= TRUE)
      
    })
    
    #Classification Hierachique ascendant
    output$Hierachique <- renderPlot({
      #Classification hierarchique ascendante
      cah.df <- hclust(df.dist,method = "ward.D")
      #Affichage du dectogramme 
      plot(cah.df)
      rect.hclust(cah.df,k=input$varH,border = 2)
      #decoupage des données en groupe
      groupes.cah <- cutree(cah.df, k = 3)
      table(groupes.cah)
      datas_tot <- cbind(datasup,groupes.cah)
      head(datas_tot)
    })
      output$pieChart <- renderPlot({
    # Créer un diagramme en cercle avec ggplot2
    ggplot(data, aes(x = "", fill = smoke)) +
      geom_bar(width = 1, stat = "count") +
      coord_polar(theta = "y") +
      labs(fill = "Statut de fumeur") +
      theme_minimal()
  })
    
    
  }
)

