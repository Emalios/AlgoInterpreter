# AlgoInterpreter

'AlgoInterpreter' est un interpréteur pour un langage de programmation recopiant strictement sa syntaxe des conventions de l'IUT Nancy-Charlemagne afin de pouvoir permettre aux étudiants d'exécuter ce type de code vu abstraitement en cours.

## Fonctionalitées actuelles et prévues:

+ [ ] **Types**
  + [X] Entier
  + [X] Booléen
  + [X] Chaîne
  + [ ] Flottants
  + [ ] Tableaux
  

+ [X] **Instructions**
  + [X] si ... alors ... sinon
  + [X] affectation
  + [X] pour x de ... a ... faire ...
  + [X] tant que x faire ...


+ [ ] **Fonctions intégrées**
    + [X] lire()
    + [X] ecrire()
    + [ ] Fonctions sur les chaînes
    + [ ] Fonctions sur les tableaux


+ [ ] Meilleurs messages d'erreurs

## Comment installer

Afin de pouvoir utiliser le langage, vous allez avoir besoins de sbt (Scala Build Tools). 

Je vous conseille d'utiliser un environnement linux, wsl suffit si vous êtes sous windows.

* ### Linux 
  [Installer sbt](https://www.scala-sbt.org/1.x/docs/Installing-sbt-on-Linux.html)

  Afin de cloner le repo vous pouvez exécuter cette commande:
  ```
  git clone https://github.com/Emalios/AlgoInterpreter.git
  ```

* ### Windows

  [Installer sbt](https://www.scala-sbt.org/download.html)

## Exécution

* ### Linux
  
  Rendez-vous dans le dossier AlgoInterpreter avec votre terminal linux et exécutez la commande
  ```
  sbt run
  ```
  
  À la première exécution, sbt va télécharger Scala et les librairies nécessaires au bon fonctionnement du langage, ce qui mettre un peu de temps.

  Ensuite vous devrez choisir le Main à exécuter, celui qui vous intéresse est au numéro '1' (vous taperez simplement '1' quand sbt vous demandera de choisir).

  ## Important

  Pour l'instant l'interpréteur ne s'occupe que de lire le code contenue dans un fichier nommé '**input.txt**' à la racine du projet, donc dans le dossier AlgoInterpreter, ainsi vous devrez créer ce fichier et écrire votre code à l'intérieur. Il est néanmoins prévu de changer ce système pour quelque chose de plus abstrait. 
