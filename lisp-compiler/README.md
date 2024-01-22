# Machine Virtuelle & Compilateur en Lisp

## Vue d'ensemble
Ce projet est destiné à la création d'un compilateur et d'une machine virtuelle (VM) écrits en Lisp. Le compilateur transforme une expression Lisp en une sortie équivalente au fichier asm. Ensuite, la machine virtuelle prend ce fichier asm généré par le compilateur et l'interprète pour produire un résultat final.

## Comment utiliser le Compilateur?
Pour utiliser le compilateur, suivez les étapes ci-dessous:

1. Chargez le fichier du compilateur:
   > (load "compilateur.lisp")

2. Exécutez la fonction de compilation avec le nom du fichier d'entrée et le nom du fichier de sortie:
   > (compilation "NomFichierEntree.lisp" "NomFichierSortie.asm") 
   Exemple : (compilation "fibo.lisp" "fibo.asm").

## Comment utiliser la Machine Virtuelle?
Voici la procédure pour utiliser la Machine Virtuelle:

1. Chargez le fichier de la machine virtuelle:
   > (load "machine.lisp") 

2. Créez une nouvelle instance de la machine virtuelle:
   > (vm-creation 'mv)

3. Chargez le fichier asm dans la machine virtuelle:
   > (vm-load 'mv "CheminFichier.asm")
   Exemple : (vm-load 'mv "Output/fibo.asm")

4. Exécutez le code à l'intérieur de la machine virtuelle:
   > (vm-run-code 'mv)

## Fichiers de programme automatiques
Dans le répertoire 'Auto', il existe deux fichiers, `testFACT` et `testFIBO`. Vous pouvez charger l'un de ces deux fichiers. Ils chargeront les fichiers du compilateur et de la VM, créeront une nouvelle instance de la VM et chargeront et exécuteront le fichier de test.