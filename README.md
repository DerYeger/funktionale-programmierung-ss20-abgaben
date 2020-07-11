# Notizen

## Grundlagen

### Referentielle Transparenz

Reguläre Funktionen sind in Haskell referentiell transparent.
D.h. Funktionsaufrufe mit identische Argumenten führen auch zu identischen Ergebnissen.
Demzufolge sind Seiteneffekte bei puren Funktionen nicht möglich.

### Bürgerrechte erster Klasse

Funktionen haben in Haskell bürgerrechte erster Klasse.
Sie können also selbst wieder als Parameter übergeben und das Ergebnis anderer Funktionen sein.

### Lazy evaluation

Haskell verwendet Call-by-name sowie Sharing durch Pointer.
Dadurch ist es möglich auf potentiell unendlich großen Datenstrukturen zu arbeiten.
Des Weiteren werden so nur tatsächlich benötigte Ausdrücke evaluiert und dies auch nur einmal.

### Typinferenz

Der Haskell-Compiler kann Typen automatisch inferieren, wodurch eine Verwendung von einer Art generischen Typvariablen möglich ist.

### Curryfizierung

Bei der Curryfizierung handelt es sich um eine partielle Applikation von Funktionsargumenten.

### Thunk

Ein Thunk ist ein nicht-evaluierter Ausdruck.

## Typen

### Typklassen

Typklassen spezifizieren Schnittstellen zu Operationen und sind mit Interfaces vergleichbar.
Die Zugehörigkeit eines Typs zu einer Typklassen ist von dessen Definition getrennt.

## Eq: Funktionen können (offensichtlich) keine Mitglied dieser Typklasse sein.

## Ord (Ordering): ein diskreter Typ, d. h. Typ der Klasse num mit den Werten GT, LT, EQ

## Enum: enthält Fload und Double.

### newtyp

Bei newtype handelt es sich um einen Mechanismus, der einargumentige Typen zur Kompilierzeit durch den Typ ihres Arguments ersetzt.
Sie dienen damit der Typsicherheit ohne die Performanz zu beeinflussen.

## Folds

### foldl'

`foldl'` dreht die Reihenfolge einer Liste um.
Dementsprechend ist eine Verwendung von `foldl'` nur bei endlichen, großen Listen sinnvoll.

### foldr

`foldr` kann auf unendlichen Listen arbeiten, falls die übergebene Funktion im zweiten Argument, also dem Akkumulator, lazy ist.

## Monaden

Monaden sind Hüllen für Elemente.
Über die Methode >>=, auch Bind genannt, lassen sich Funktionen in den Kontext einer Monade einbinden, die deren Wert verweden.

## Parallelisierung

### Thread-Programmierung

In Haskell werden alle laufenden Threads beim Ende des main-Threads terminiert.
Um dies zu vermeiden gibt es Wartemechanismen wie `waitAll`.
Threads werden mit Hilfe von Funktionen wie `forkIO` erzeugt und können auf gemeinsame Bezeichner zugreifen.

### MVar

`MVars` sind Haskells Grundbaustein der Synchronisation und Kommunikation.
Threads können Werte ablegen oder herausnehmen, wobei sie blockiert werden falls die `MVar` voll bzw. leer ist.
Über die Methode `modifyMVar` ist zudem die Implementierung eines kritischen Abschnitts möglich.

### Software Transactional Memory

Bei STM werden `TVars` in STM-Monaden modifiziert.
Dies passiert atomar, weshalb eine Verwendung von IO oder `MVar` verboten ist.

### Eval Monade

Die Eval Monade ermöglicht Parallelisierung durch die Funktionen `rpar` und `rseq`.
Erstere gibt an, dass ihr (unevaluiertes) Argument parallel evaluiert werden kann (zu WHNF), während bei `rseq` auf das Ergebnis gewartet wird.

### Par Monade

Die Par Monade verwendet `IVars` um Erbenisse herumzureichen.
Anders als die verwandten `MVars` können sie jedoch nur einmal beschrieben werden.
In Kombination mit der Methode `fork` entstehen so Datenflussgraphen