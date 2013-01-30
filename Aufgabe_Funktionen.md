Afgabe
========================================================


Aufgabe 1
-------------------------

Funktionen sind in R Objekte. Es gibt eine eigene Klasse "function". Was ist eine Klasse? Was machen die Funktionen class(), attributes(), mode()? Tipp: "R Language Definition", Google und R-Hilfe.

Aufgabe 2
-------------------------
Eine Funktion kann neben dem Namen durch drei Bestandteile charakterisiert werden. Den Argumenten, dem Body und dem return-value. Beschreibe die Bestandteile anhand folgendem Schemas.


```r
function(args) {
    body
    return(value)
}
```


Aufgabe 3
-------------------------
Beschreibe folgende Funktion. Was ist der Name, die Argumente, body und return-value?


```r
some.function <- function(x, y) {
    # Hier ist typischerweise eine kurze Beschreibung der Funktion
    return.value <- x + y
    return(return.value)
}
```


Was ist der Output folgender Aufrufe. Überlege erst und überprüfe deine Überlegung in dem du die Funktion aufrufst.


```r
some.function(1, 2)
some.function(c(1, 1), c(2, 2))
some.function(c(1, 1), c(2, 2), c(3, 3))
some.function(c(1, 2, 3), c(1, 2))
some.function(some.function(c(1, 2), c(3, 4)), c(5, 6))
```




Aufgabe 4
-------------------------
Schreibe eine eigene Funktion die als Argument einen numerischen Vektor hat und ihn quadriert zurückgibt.


```r
sq.x <- function(x) {
    # Body
}
```


Teste die Funktion mit beliebigen Vektoren. Überprüfe ob der return-value deinen Erwartungen entspricht.






