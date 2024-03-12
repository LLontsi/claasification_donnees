Analyse de l'obésité - Application Shiny

Ce projet est une application Shiny conçue pour aider à l'analyse de données sur l'obésité. L'objectif principal de cette application est de fournir un outil convivial pour explorer, visualiser et analyser les données sur l'obésité à partir d'un ensemble de données préexistant.
Table des matières

    Introduction
    Fonctionnalités
    Installation
    Utilisation
    Structure du Projet
    Contributions
    Licence
    Contact

Introduction

L'obésité est un problème de santé majeur dans de nombreuses régions du monde, avec des implications importantes pour la santé publique. Cette application vise à aider les chercheurs, les praticiens et les décideurs à explorer et à comprendre les données sur l'obésité afin de mieux informer les politiques et les interventions de santé publique.
Fonctionnalités
Manipulation de données

    Chargement de données : Les utilisateurs peuvent charger un ensemble de données sur l'obésité au format CSV.

    Nettoyage des données : L'application permet aux utilisateurs de nettoyer les données en supprimant les attributs inutiles, en gérant les valeurs manquantes et en convertissant les types de données si nécessaire.

Visualisation de données

    Statistiques Descriptives : Les utilisateurs peuvent obtenir des statistiques descriptives telles que la moyenne, l'écart-type et la médiane pour les différentes variables du jeu de données.

    Histogrammes : Visualisez la distribution des variables numériques à l'aide d'histogrammes interactifs.

    Nuages de Points : Explorez les relations entre les variables en traçant des nuages de points interactifs.

    Diagrammes en Bâtons : Visualisez la distribution des variables catégorielles à l'aide de diagrammes en bâtons interactifs.

    Diagrammes Circulaires : Obtenez une vue d'ensemble des proportions des catégories à l'aide de diagrammes circulaires interactifs.

Extraction de Données

    Règles d'Association : Les utilisateurs peuvent extraire des règles d'association entre les différentes variables du jeu de données pour découvrir des modèles cachés.

Classification Supervisée

    Arbres de Décision : Exécutez des modèles d'arbres de décision pour prédire la catégorie d'obésité d'un individu à partir de ses caractéristiques.

    Réseaux de Neurones : Entraînez des réseaux de neurones pour la classification de l'obésité en fonction des caractéristiques de l'individu.

    Plus Proches Voisins : Utilisez l'algorithme des k plus proches voisins pour prédire la catégorie d'obésité d'un individu en se basant sur ses voisins les plus proches dans l'espace des caractéristiques.

    SVM (Support Vector Machines) : Exécutez des modèles SVM pour la classification de l'obésité en fonction des caractéristiques de l'individu.

Installation

Pour installer cette application sur votre machine locale, suivez ces étapes :

    Clonez ce dépôt GitHub sur votre machine en utilisant la commande suivante :

    bash

    git clone https://github.com/votre-utilisateur/analyse-obesite-shiny.git

    Assurez-vous d'avoir R et RStudio installés sur votre machine.

    Ouvrez le fichier app.R dans RStudio et exécutez-le pour lancer l'application.

Utilisation

Une fois l'application lancée, vous pouvez naviguer entre les différents onglets pour effectuer différentes tâches :

    Manipulation de Données : Chargez votre jeu de données, nettoyez-le et préparez-le pour l'analyse.

    Visualisation de Données : Explorez visuellement les données à l'aide de diverses techniques de visualisation.

    Extraction de Données : Découvrez des modèles cachés dans les données en extrayant des règles d'association entre les variables.

    Classification Supervisée : Exécutez plusieurs modèles de classification pour prédire la catégorie d'obésité d'un individu.

Structure du Projet

    app.R : le fichier principal contenant le code Shiny de l'application.
    ObesityDataSet.csv : le jeu de données utilisé par l'application.
    README.md : ce fichier README.

Contributions

Les contributions à ce projet sont les bienvenues ! Si vous souhaitez contribuer, veuillez soumettre une Pull Request.
Licence

Ce projet est distribué sous la licence MIT.
Contact

Pour toute question ou commentaire, veuillez contacter l'auteur :

Your Name
