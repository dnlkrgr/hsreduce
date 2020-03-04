# ToDo / Protocol

## ToDo's
* MUST
  * erfassen, welche Herausforderungen es bei den Pässen gibt
    * gleich immer aufschreiben
    * was hat man wissenschaftlich rausgefunden?
    * was ist alles mit Herausforderungen gemeint?
      * z.B., wie effektiv die Pässe sind?
  * grobe Transformationen zuerst
    * z.B. imports
      * erst versuchen, alle zu löschen
      * dann 1/2, 1/4, 1/8, ...
      * dann nur unused
      * dann nur Anzahl der Funktionen minimieren
  * ocharles/weeder benutzen
  * Haskell Source Plugins anschauen
  * 24 days of GHC extensions durchlesen
    * GADTs, Type Families anschauen
  * am Anfang überprüfen, ob Test-Case überhaupt interessant
  * structureshrink zum Laufen bringen
  * automatisiert Test-Cases hinzufügen + laufen lassen können
* SHOULD
  * Code weiter runterbrechen in one-line-Funktionen
  * Ausgabe schöner machen
  * reduce-loop: BFS

## Passes to implement
* Remove Unused
  - [ ] unnötige Methoden / ganze Instanzen entfernen
  - [ ] Kontexte in Funktionen
  - [ ] Imports
    - [ ] Anzahl importierter Funktionen minimieren
    - [x] unused imports entfernen
  - [ ] Exports
    - [ ] Handling von impliziten export all
    - [x] unused exports entfernen
  - [x] Data Decls ohne Konstruktoren
  - [] Pragmas
    - [ ] UNPACK Pragmas
    - [ ] INLINE Pragmas
    - [ ] other Pragmas
    - [x] LANGUAGE Pragmas
  - [x] unused Bindings entfernen
  - [x] Data Decl: Konstruktoren entfernen
* Typsignaturen vereinfachen
  - [ ] unnötige Parameter weg
  - [ ] Typparameter durch Unit / () ersetzen
  - [ ] forall weg
* On the Project Level
  - [ ] Module mergen
  - [ ] Dependencies vendorn: nicht mehr als separate Dependency
  - [ ] cabal file: dependencies entfernen
* Typen vereinfachen / Typaliase
  - [ ] unnötige Typaliase weg
  - [ ] Typ mit 1 Konstruktor: kann gleich Typalias sein, oder?
  - [ ] Typaliase reduzieren / minimieren
  - [ ] Typaliase inlinen
  - [ ] deriving-Clause minimieren
  - [ ] Produkttypen minimieren
* Template haskell
  - [ ] TH: splices dumpen, gedumpte einfügen in HS-Datei und dann weiter reduzieren 
* Misc
  - [ ] Datei formattieren
  - [ ] case-Ausdrücke minimieren
  - [ ] ganze Deklarationen löschen
  - [ ] arithmetische, boolesche Ausdrücke vereinfachen
  - [ ] ganze Decls löschen
* Stubbing
  - [x] HsExpr: place undefined into every possible HsExpr

## Interesting tickets
* 17722
* 17684
* 16127

## creduce's core

```
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

* \<name of test-case>: <% amount reduced>, \<so far minimal file size>

* test-cases/ticket14779: **77.6%, 521 bytes**
  * pass statistics:                                                   
    *  method pass_clex :: rm-toks-2 worked 1 times and failed 343 times
    *  method pass_clex :: delete-string worked 1 times and failed 2 times
    *  method pass_clex :: rm-toks-5 worked 1 times and failed 330 times
    *  method pass_blank :: 0 worked 1 times and failed 0 times         
    *  method pass_balanced :: square-inside worked 1 times and failed 7 times
    *  method pass_clex :: rm-toks-10 worked 1 times and failed 320 times
    *  method pass_clex :: rm-toks-4 worked 2 times and failed 335 times 
    *  method pass_clex :: rm-toks-1 worked 2 times and failed 345 times 
    *  method pass_lines :: 1 worked 3 times and failed 181 times       
    *  method pass_clex :: rm-tok-pattern-4 worked 6 times and failed 2448 times
    *  method pass_clex :: rename-toks worked 13 times and failed 36 times  
    *  method pass_lines :: 0 worked 20 times and failed 199 times

## structureshrink Performance