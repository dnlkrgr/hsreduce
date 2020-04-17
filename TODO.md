# ToDo's / Protocol

## MUST
  - [ ] **uniplate statt syb benutzen**
  - [ ] **paths lib benutzen**
  - [ ] **ExceptT benutzen**
  - [ ] **Testen mit QuickCheck, hedgehog, tasty, usw.**
  - [ ] **performante Datentypen benutzen (Text, strikte ByteStrings, usw.)**
  - [ ] Haskell Source Plugins anschauen
  - [ ] Structureshrink Zum Laufen Bringen
  - [ ] Automatisiert Test-Cases Hinzufügen + Laufen Lassen Können
    - wahrsch. am besten repo dazu einrichten
  - erfassen, welche Herausforderungen es bei den Pässen gibt
    - gleich immer aufschreiben
    - was hat man wissenschaftlich rausgefunden?
    - was ist alles mit Herausforderungen gemeint?
      - z.B., wie effektiv die Pässe sind?
  - grobe Transformationen zuerst
    - z.B. imports
      - erst versuchen, alle zu löschen
      - dann 1/2, 1/4, 1/8, ...
      - dann nur unused
      - dann nur Anzahl der Funktionen minimieren
  - 24 days of GHC extensions durchlesen
    - GADTs, Type Families anschauen
  - [ ] #14270 mit git repo reproduzieren
  - ocharles/weeder benutzen
  - [x] Ticket mit Container Bug reproduzieren

## SHOULD

  - [ ] Ausgabe schöner machen
  - [ ] Am Anfang überprüfen, ob Test-Case überhaupt interessant
    - eigentlich ist User selbst dafür verantwortlich
  - Code weiter runterbrechen in one-line-Funktionen
  - reduce-loop: BFS

## Ausarbeitung

- Passes
  - was gab es anfangs für Probleme? wie wurden die behoben?
    - welche Änderungen musste man durchführen, um die Pässe zu verbessern?
  - welche Feinheiten sind zu beachten?
  - welche syntaktischen Elemente sind "wieviel wert"?
  - wie effektiv sind die Pässe?
  - was waren Herausforderungen für Implementierung der Pässe?

## Merging Modules
  - [x] **HsAllInOne auf GHC API umstellen**

## Reduction Passes 

- Typsignaturen vereinfachen
  - Unit / ()
    - [ ] Typparameter durch Unit / () ersetzen + Benutzungen löschen
    - [x] Typparameter durch Unit / () ersetzen
  - forall
    - [ ] Variablen aus forall entfernen
    - [x] forall weg
  - Kontexte / Constraints
    - [ ] Variablen aus Kontext entfernen
    - [x] Kontexte / Constraints entfernen
  - unnötige Parameter weg
    - [ ] dann auch in allen Verwendungen löschen
  - Bangtypes: Bang weg
- Typen vereinfachen / Typaliase
  - [ ] unnötige Typaliase weg
  - [ ] Typ mit 1 Konstruktor: kann gleich Typalias sein, oder?
  - [ ] Typaliase reduzieren / minimieren
  - [ ] Typaliase inlinen
  - [ ] deriving-Clause minimieren
  - [ ] Produkttypen minimieren
- Parameter
  - [ ] unbenutzte Parameter zu `_` abändern oder gleich löschen
    - [ ] -Wunused-params benutzen
  - `_` kommt anstatt Parameter vor
    - [ ] Parameter entfernen
    - [ ] aus Signatur löschen
    - [ ] aus Matches löschen
    - [ ] aus Verwendungen löschen
- Remove Unused
  - Decls
    - GADTs
      - [ ] Typvariablen durch () ersetzen
      - [x] forall weg
      - [x] Typkontext / Constraints weg
    - [ ] iwie das Filtern von Decls schöner machen, einmal filter und für alle Decls kurz den Check implementieren oder iwie "Inversion of Control" hinkriegen
    - [x] Datendeklarationen
      - [x] unbenutzte Konstruktoren löschen
      - [x] Data Decls ohne Konstruktoren löschen
    - [x] unbenutzte Function Bindings
      - Schwierigkeiten: wenn die Signatur nicht mitgelöscht wird, macht es keinen Sinn, nur das FunBind zu löschen
      - wegen der nicht-funktionierenden Löschung sind dann andere decls noch lebendig
    - [x] sonstige Decls versuchen zu entfernen
  - Imports
    - [ ] Anzahl importierter Funktionen minimieren
      - sollte mit "-Wunused-imports" nicht so schwer sein, das kann es so fein erkennen
    - explizites import all
      - [ ] Konvertieren zu explizitem import all
    - [ ] alle imports qualified machen
    - [x] unused imports entfernen
  - Exports
    - Handling von impliziten export all
      - [x] Konvertieren zu explizitem export all
    - [x] unused exports entfernen
    - Beobachtung: alle Exports entfernen ist nicht beste Strategie, siehe #14270
  - Pragmas
    - die befinden sich bei SigD!
      - [ ] UNPACK Pragmas
      - [ ] INLINE Pragmas
      - [ ] other Pragmas
    - [x] LANGUAGE Pragmas
- On the Project Level
  - [ ] Module mergen
  - [ ] Dependencies vendorn: nicht mehr als separate Dependency
  - [ ] cabal file: dependencies entfernen
- Template haskell
  - [ ] TH: splices dumpen, gedumpte einfügen in HS-Datei und dann weiter reduzieren
- Misc
  - [ ] Datei formattieren
  - [ ] case-Ausdrücke minimieren
    - [ ] Matches entfernen und ein `_ -> ...` Match einführen
  - [ ] arithmetische, boolesche Ausdrücke vereinfachen
- Normalization
  - [ ] renaming functions
  - [ ] renaming parameters
  - [ ] renaming constants
  - [ ] renaming data declarations / types
- Stubbing
  - [x] Matches mit GRHS == "undefined" entfernen
  - [ ] Matches mit RHS == "undefined" weg
  - [x] LGRHS: guards entfernen
  - [ ] LGRHS: guards einzeln entfernen
  - [x] HsExpr: place undefined into every possible HsExpr
  - [x] HsType durch () ersetzen
  - [x] where-Klauseln löschen
  - [x] GADTs: forall und Kontext löschen
  - [x] Case-Expressions mit nur einem Fall
  - [x] if-then-else mit einem Fall == "undefined"


## Reporting to implement

- general info
  - [ ] Performance
    - [ ] bytes / minute
    - [ ] total % reduced
- Passes
  - [ ] how often a pass succeeded
  - [ ] total number of run tests during a pass
  - [ ] how much reduction an individual pass brought
  - [ ] how much time was spent during the pass
  - [ ] if a pass looks at different elements: what kind of expression / element brought the most reduction
  - [ ] what are the best / worst passes, and why? How could they improve?

## Interesting tickets

- 18018
- 17722
- 17684
- 16127
- 17516

## creduce's core

```c++
current = original_test_case
do
  size_at_start = size(current)
  foreach (p, option) in pass_list
    state = p::new(current, option)
    do
      variant = current         // this is a file copy operation
      result = p::transform(variant, option, state)
      if result == ERROR
  report_problem_in_pass(p, option)
      if result == OK
        if is_interesting(variant)
          current = variant     // also a file copy
  else
          state = p::advance(current, option, state)
    while result == OK
while size(current) < size_at_start
```

## `creduce --not-c` Performance

- \<name of test-case>: <% amount reduced>, \<so far minimal file size>

- test-cases/ticket14270: **88.6 %, 389 bytes**
  - pass statistics:
    - method pass_balanced :: square-inside worked 1 times and failed 1 times
    - method pass_balanced :: square-only worked 1 times and failed 1 times
    - method pass_clex :: rm-toks-9 worked 1 times and failed 522 times
    - method pass_clex :: rm-toks-15 worked 1 times and failed 439 times
    - method pass_blank :: 0 worked 1 times and failed 0 times
    - method pass_clex :: rm-toks-13 worked 1 times and failed 454 times
    - method pass_balanced :: parens-to-zero worked 1 times and failed 109 times
    - method pass_lines :: 2 worked 1 times and failed 377 times
    - method pass_clex :: rm-toks-16 worked 2 times and failed 407 times
    - method pass_peep :: a worked 2 times and failed 173 times
    - method pass_clex :: rm-toks-12 worked 2 times and failed 467 times
    - method pass_balanced :: parens-inside worked 2 times and failed 66 times
    - method pass_balanced :: parens-only worked 2 times and failed 21 times
    - method pass_clex :: rm-toks-10 worked 3 times and failed 492 times
    - method pass_balanced :: parens worked 3 times and failed 69 times
    - method pass_balanced :: curly worked 3 times and failed 24 times
    - method pass_lines :: 1 worked 4 times and failed 390 times
    - method pass_clex :: rm-toks-6 worked 4 times and failed 571 times
    - method pass_clex :: rm-toks-8 worked 5 times and failed 531 times
    - method pass_clex :: rm-toks-2 worked 5 times and failed 681 times
    - method pass_lines :: 6 worked 6 times and failed 402 times
    - method pass_clex :: rm-toks-5 worked 7 times and failed 595 times
    - method pass_clex :: rm-toks-4 worked 7 times and failed 626 times
    - method pass_clex :: rm-toks-3 worked 9 times and failed 654 times
    - method pass_clex :: rename-toks worked 13 times and failed 46 times
    - method pass_clex :: rm-toks-1 worked 14 times and failed 691 times
    - method pass_clex :: rm-tok-pattern-4 worked 36 times and failed 2719 times
    - method pass_lines :: 0 worked 39 times and failed 542 times
- test-cases/ticket14779: **77.6%, 521 bytes**
  - pass statistics:
    - method pass_clex :: rm-toks-2 worked 1 times and failed 343 times
    - method pass_clex :: delete-string worked 1 times and failed 2 times
    - method pass_clex :: rm-toks-5 worked 1 times and failed 330 times
    - method pass_blank :: 0 worked 1 times and failed 0 times
    - method pass_balanced :: square-inside worked 1 times and failed 7 times
    - method pass_clex :: rm-toks-10 worked 1 times and failed 320 times
    - method pass_clex :: rm-toks-4 worked 2 times and failed 335 times
    - method pass_clex :: rm-toks-1 worked 2 times and failed 345 times
    - method pass_lines :: 1 worked 3 times and failed 181 times
    - method pass_clex :: rm-tok-pattern-4 worked 6 times and failed 2448 times
    - method pass_clex :: rename-toks worked 13 times and failed 36 times  
    - method pass_lines :: 0 worked 20 times and failed 199 times
- test-cases/ticket15696_1: **68.6 %, 431 bytes**
  - pass statistics:
    - method pass_clex :: rm-toks-7 worked 1 times and failed 214 times
    - method pass_clex :: rm-toks-11 worked 1 times and failed 203 times
    - method pass_balanced :: square-inside worked 1 times and failed 1 times
    - method pass_clex :: rm-toks-15 worked 1 times and failed 188 times
    - method pass_balanced :: parens worked 1 times and failed 28 times
    - method pass_blank :: 0 worked 1 times and failed 0 times
    - method pass_balanced :: parens-only worked 1 times and failed 23 times
    - method pass_clex :: rm-toks-3 worked 2 times and failed 229 times
    - method pass_clex :: rm-toks-4 worked 2 times and failed 221 times
    - method pass_clex :: rm-tok-pattern-4 worked 2 times and failed 1480 times
    - method pass_clex :: rm-toks-2 worked 4 times and failed 235 times
    - method pass_clex :: rm-toks-1 worked 5 times and failed 243 times
    - method pass_clex :: rename-toks worked 8 times and failed 57 times
    - method pass_lines :: 0 worked 16 times and failed 201 times
- test-cases/ticket15753: **66.2 %, 1312 bytes**
  - pass statistics:
    - method pass_lines :: 2 worked 1 times and failed 391 times
    - method pass_balanced :: parens-to-zero worked 1 times and failed 138 times
    - method pass_balanced :: parens worked 1 times and failed 100 times
    - method pass_blank :: 0 worked 1 times and failed 0 times
    - method pass_clex :: rm-toks-8 worked 1 times and failed 1137 times
    - method pass_indent :: regular worked 1 times and failed 0 times
    - method pass_balanced :: curly worked 1 times and failed 34 times
    - method pass_clex :: rm-toks-11 worked 1 times and failed 1125 times
    - method pass_lines :: 1 worked 1 times and failed 413 times
    - method pass_clex :: rm-toks-5 worked 2 times and failed 1184 times
    - method pass_clex :: rm-toks-7 worked 3 times and failed 1145 times
    - method pass_clex :: rm-toks-6 worked 3 times and failed 1166 times
    - method pass_clex :: rm-toks-4 worked 3 times and failed 1194 times
    - method pass_clex :: rm-toks-3 worked 6 times and failed 1206 times
    - method pass_peep :: a worked 6 times and failed 1020 times
    - method pass_clex :: rm-toks-2 worked 6 times and failed 1224 times
    - method pass_lines :: 6 worked 7 times and failed 409 times
    - method pass_lines :: 0 worked 13 times and failed 505 times
    - method pass_clex :: rm-tok-pattern-4 worked 14 times and failed 8816 times
    - method pass_clex :: rm-toks-1 worked 17 times and failed 1236 times
    - method pass_clex :: rename-toks worked 19 times and failed 142 times

## structureshrink Performance
