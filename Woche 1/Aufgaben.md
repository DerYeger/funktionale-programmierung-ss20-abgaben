# Woche 1

Juri Lozowoj, 35244015 \
Jan Müller, 35011918

## Aufgabe a

### 4 * 8 -33

Das Ergebnis ist `-1`.

### (*) 4 8 - 33

Das Ergebnis ist `-1`.

### (-)(*) 4 8 33

Der Fehler lässt sich durch Klammerung beheben: `(-) ((*) 4 8) 33`.

### 2+-3

Der Fehler lässt sich durch Klammerung beheben: `2 + (-3)`.

### succ 7

Das Ergebnis ist `8`.

### sin (pi / 2)

Das Ergebnis ist `1.0`.

### round 3.5

Das Ergebnis ist `4`.

### read 3

Der Fehler lässt sich durch Hinzufügen von Anführungszeichen und Typangabe beheben: `read "3" :: Int`.

### 2^63

Das Ergebnis ist `9223372036854775808`.

### 2^63 :: Int

Das Ergebnis ist `-9223372036854775808`, auf Grund eines Overflows.

## Aufgabe b

Funktionsnamen müssen kleingeschrieben werden und das `where` muss eingerückt sein.

```
n = a `div` length xs
    where 
        a = 10
        xs = [1, 2, 3, 4, 5]
```

## Aufgabe c

Die vorletzte Zeile ist kein gültiger Code, da die unäre Funktion `myAdd` ein Argument des Typs `(Int, Int)` erwartet und nur `Int` bekommt.
Die letzte Zeile ist hingegen gültig, da es sich um eine partielle Applikation handelt. Diese ist möglich, da `add` den Typ `Int -> Int -> Int` hat. Das Resultat der partiellen Applikation hat also den Typ `Int -> Int` und berechnet die Funktion `add 1 y`, der Parameter x wird also eingefroren.

## Aufgabe d

```
mult x y = x * y
double = mult 2
```

## Aufgabe e

```
combi :: (String, String) -> String
combi (first, second) = first ++ second
```

## Aufgabe f

```
infixr ?
(?) minuend subtrahend
    | minuend < 0 = 0
    | subtrahend < 0 = 0
    | subtrahend > minuend = 0
    | otherwise = minuend - subtrahend
```

## Aufgabe g

`AtomicInteger::incrementAndGet` erfüllt diese Eigenschaft nicht, da es einen Nebeneffekt.
Eine Methode mit dieser Eigentschaft ist `Math::pow`, da jeder Aufruf dieser Methode durch ihr Ergebnis ersetzt werden könnte.

## Aufgabe h

First-Class Citizens können als Parameter und Rückgabewerte verwendet werden.
Bei Haskell sind Funktionen First-Class Citizens und können dementsprechend an andere Funktionen übergeben und von diesen auch zurückgegeben werden.

## Aufgabe k

```
main = interact parseFile

parseFile input =
    let
        fileLines = lines input
        charCount = show $ length input
        wordCount = show . length . words $ input
        secondLine = if (length fileLines >= 2) then fileLines !! 1 else ""
        secondLineLength = show . length $ secondLine
    in charCount ++ " characters\n" ++ wordCount ++ " words\nSecond line: " ++ secondLine ++ "\n" ++ secondLineLength ++ " characters on the second line\n"
```
