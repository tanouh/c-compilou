# Compilou

Un mini compilateur C en Ocaml réalisé dans le cadre d'un cours à Telecom Paris

## Developpeurs

- Mahandry Tania
- Garnier Kevin
- Dobrzynski Alexis
- Srichanwit Frédéric

## Extensions de base

- Deux types : ``int``
- Les fonctions (prenant des arguments et retournant des valeurs)
- if et if-else avec ou sans accolades
- arithmetique basique (``+``,``-``,``*``,``/``,``%``), comparaisons (``==``, ``<``, ``<=``, ``>``, ``>=``)
and logique (``&&``, ``||``)
- declaration de variables locale et assignements
- Une fonction ``print_int`` qui print ses arguments

## Extensions supplementaires

- type ``void``
- Messages d'erreurs precis
- Commentaires
- Fonctions sans retour
- Fonction sans limites du nombre de variables (dans la limite de la mémoire)
- Simplification des instructions comportant seulement des entiers
- `print_int` a un nombre variable d'argument et ecrit le resultat en sortie standard comme la fonction `print` de python
- assignement et declaration sur une seule ligne
- script de test avec différents tests

## Compilation et execution

Pour compiler, il suffit de utiliser la commande
``make``

Pour compiler un programme C, il suffit d'utiliser la commande
```./compilou.exe [filename.c]```

Pour executer les tests, vous devez vous placer à la racine du projet et vous pouvez utiliser :

- Si vous etes sous linux :

  - ``make test``
  - ``./test.sh``

- Si vous etes sous macos :

  - ``make test_macos``
  - ``./test_macos.sh``

