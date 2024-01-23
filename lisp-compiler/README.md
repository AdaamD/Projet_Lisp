# Compilateur & Machine Virtuelle  

## Introduction 
Ce projet se concentre sur la création d'un compilateur et d'une machine virtuelle (VM) écrits en Lisp. L'objectif du compilateur est de convertir une expression Lisp en une sortie équivalente sous la forme d'un fichier asm. Ensuite, la machine virtuelle prend en charge le fichier asm généré par le compilateur, interprétant son contenu pour générer un résultat final.

## Utilisation Générale

### Compilateur
Pour utiliser le compilateur, suivez les étapes ci-dessous :

1. Chargez le fichier du compilateur :
   > (load "compilateur.lisp")

2. Exécutez la fonction de compilation avec le nom du fichier d'entrée et le nom du fichier de sortie :
   > (compilation "NomFichierEntree.lisp" "NomFichierSortie.asm") 
   Exemple : (compilation "fibo.lisp" "fibo.asm").

### Machine Virtuelle
Voici la procédure pour utiliser la Machine Virtuelle :

1. Chargez le fichier de la machine virtuelle :
   > (load "machine.lisp") 

2. Créez une nouvelle instance de la machine virtuelle :
   > (vm-creation 'mv)

3. Chargez le fichier asm dans la machine virtuelle :
   > (vm-load 'mv "CheminFichier.asm")
   Exemple : (vm-load 'mv "Output/fibo.asm")

4. Exécutez le code à l'intérieur de la machine virtuelle :
   > (vm-run-code 'mv)

## Fichiers de programme automatiques
Dans le répertoire 'Auto', il existe deux fichiers, `testFACT` et `testFIBO`. Vous pouvez charger l'un de ces deux fichiers. Ils chargeront les fichiers du compilateur et de la VM, créeront une nouvelle instance de la VM et chargeront et exécuteront le fichier de test.

## Auteur

Ce projet a été réalisé par :

- DAIA Adam
- DAFAOUI Mohammed
- RABIA Aness

Étudiants en M1 Génie Logiciel à l'Université de Montpellier