# Opis

Interpreter języka Latte rozszerzonego o:
* inferencję typów
* funkcje zagnieżdżone ze statycznym wiązaniem
* funkcje wyższego rzędu, anonimowe i domknięcia

W rozwiązaniu inspirowałem się opisem monady interpretera znajdującym się w dokumentacji pakietu `transformers`:
[link](https://hackage.haskell.org/package/transformers-0.6.0.4/docs/Control-Monad-Trans-Class.html#g:7)

Typechecker jest niekompletny i z tego powodu do czasu jego uzupełnienia wyłączyłem rzucanie wyjątków gdy typy się nie zgadzają.

## Tabelka funkcjonalności

Na 15 punktów

* 01 (trzy typy)
* 02 (literały, arytmetyka, porównania)
* 03 (zmienne, przypisanie)
* 04 (print)
* 05 (while, if)
* 06 (funkcje lub procedury, rekurencja)
* 07 (przez zmienną / przez wartość / in/out)

Na 20 punktów
* 09 (przesłanianie i statyczne wiązanie)
* 10 (obsługa błędów wykonania)
* 11 (funkcje zwracające wartość)

Na 30 punktów
* 12 (4) (statyczne typowanie)
* 13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
* 17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)

Razem 30 pkt
