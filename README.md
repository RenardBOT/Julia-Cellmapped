# Julia-Cellmapped
Visualisation d'ensembles de Julia en utilisant des techniques de cell-mapping.
Pour le moment le programme ne dispose pas d'executable.

Installer le module graphique Cairo :
```shell
opam install cairo2
``` 

A chaque utilisation, positionner les variables d'environnement :
```shell
eval $(opam env)
``` 

Puis pour afficher des ensembles de Julia, il faut importer les 3 modules cell.ml, julia.ml et graphics.ml dans un terminal interactif ocaml avec la librairie unix: 

```shell
$ocaml unix.cma
#use "topfind";;
#require "cairo2";;
#use "src/julia.ml";;
#use "src/graphics.ml";;
#use "src/main.ml";;
```

Ensuite il est possible d'utiliser la fonction julia_draw pour dessiner un ensemble de Julia.
Le prototype de la fonction est **draw_julia c_re c_im iter hsv_hue hsv_light nom_fichier** où :
- **c_re et c_im** sont les valeurs réelles et imaginaires de la constante de Julia pour l'ensemble choisi (par exemple -0.8+ i*0.9)
- **iter** est le nombre d'itérations de l'algorithme (divise en 4 chaque cellules à chaque itérations)
- **hsv_hue et hsv_light** sont les valeurs de teinte et de luminsoité de la couleur avec laquelle dessiner au format HSV *(TSL en français, où la teinte T est comprise entre 0 et 360, et la lumiosité L est comprise entre 0 et 1)*
- **filename** est le nom du fichier .png

Par exemple : 
```
draw_julia (-.0.835) (-.0.2321) 5 80. 1. "julia";;
```
