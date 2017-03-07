# Mise à jour et standardisation des séries chronologiques d'abondance du saumon atlantique sur les cours d'eau de l'ORE DiaPFC

___

## DATA

Lange F., Guéraud F., Huchet E., Rives J. and Prévost E. 2017. Abundances and biological traits of the juveniles salmon sampled in the survey of Salmon abundance Indices in the Nivelle river (France). [doi:10.15468/alsjvy](doi:10.15468/alsjvy)

Jeannot N., Azam D., Guilloux Y. and Prévost E. 2017. Abundances and biological traits of the juveniles salmon sampled in the survey of Salmon abundance Indices in the Scorff river (France). [doi:10.15468/mz4lyw](doi:10.15468/mz4lyw)



## STRUCTURE

Le dossier se compose de sous-dossiers correspondant aux différents sites de l'ORE analysés (Bresle, Scorff, Oir et Nivelle). Ces dossiers sont sous-divisés en fonction des stades observés (adultes, smolts ou tacons) :  

ORE/  # dossier racine  
  |  
  |_Abundance/ # dossier contenant les analyses des indices d'abondances  
	- README.md # ce fichier  
	- run.sh # script bash permettant de créer les scripts d'analyse par site et par stade (ex: analyse_tacon.R) puis de lancer les analyses; faire ./run.sh dans un terminal (linux)  
	- analyse.R # script d'analyse principal; ce script est automatiquement modifié via run.sh pour changer les paramètres (ex: nombre d'itérations,...)  
  |_doc/ # contient les rapports,...  
  |  
 	|_Bresle/  
   			|_adult/  
			  |_smolt/  
          			- data/ # données d'échantillonnage  
          			- inits/ # contient un script R (inits_smolt.R) qui génère automatiquement les valeurs initiales pour les chaînes MCMC  
          			- model/ # modele pour les inférences  
          			- results/ # contient les diagnostiques et les résultats des analyses  
          			- Sab/ # dossier des analyses faites par Sabrina Servanty (cf. rapport dans le dossier doc/)  
          			- parameters_smolt.R # paramètres à monitorer  
  		|
 		|_Oir/  
      			|_adult/  
     			|_smolt/  
      			|_tacon/  
  		|
  		|_Scorff/  
      			|_adult/  
      			|_smolt/  
      			|_tacon/  
  		|  
  		|_Nivelle/  
      			|_adult/  
      			|_tacon/  
				- analyse_coda_tacon.R # Permet d'obtenir des fichiers textes avec les estimations finales d'abondance à partir des CODA (S. Servanty)  


## ANALYSES

1. Ouvrir le fichier "run.sh" et faire les modifications nécessaires (nb d'itérations,...)
2. Dans un terminal (linux): ./run.sh 

___

## TO DO

- défifnir des requêtes pour extraire les données de la base directement
- repenser les tableaux de données: automatisation via la BDD? extraction via tableurs excel? Cela forcera à reprendre les scripts R (notamment au niveau du nettpyage/formattage des données)
- Vérifier les dates
- Nettoyage scripts data et inits (enlever les parties commentées)
- mettre sous GitHub
- ajouter nom du site aux résultats
- Passer sous JAGS? OpenBUGS n'est plus (trop) maintenu, problème de format de données et d'allocation de mémoire + meilleur intégration de jags dans R
- Nécessite de regarder si il y a des "cut" dans les modèles

# BRESLE
* séparation 1HM/PHM basée sur la taille / seuil fixé à 70cm -> à actualiser?

# SCORFF
* modele SMOLT change chaqque année en fonction de l'arret des pièges!
* automatisation des inits pour les adultes (cf inits_stade.R)

# OIR
* automatisation des inits (cf inits_stade.R)
* Les données smolts vont jusqu'à 2016!

# NIVELLE
* automatisation des inits (cf inits_stade.R)
