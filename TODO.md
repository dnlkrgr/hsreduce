# ToDo / Protocol

## ToDo's
* Code weiter runterbrechen in one-line-Funktionen
* Ausgabe schöner machen
* am Anfang überprüfen, ob Test-Case überhaupt interessant
* ocharles/weeder benutzen
* Haskell Source Plugins anschauen
* grobe Transformationen zuerst
* 24 days of GHC extensions
  * GADTs, Type Families anschauen
* erfassen, welche Herausforderungen es bei den Pässen gibt
  * gleich immer aufschreiben
  * was hat man wissenschaftlich rausgefunden?
  * was ist alles mit Herausforderungen gemeint?
    * z.B., wie effektiv die Pässe sind?
* structureshrink zum Laufen bringen
* reduce-loop: BFS
* automatisiert Test-Cases hinzufügen + laufen lassen können

## Passes to implement
* Stubbing
  - [ ] let und where
  - [ ] unnötige Methoden / ganze Instanzen entfernen
  - [x] Function Bindings: alle Matches mit undefined belegen
  - [x] Typinstanzen: bei Methoden undefined einsetzen
* Remove Unused
  - [ ] Imports
  - [ ] Kontexte in Funktionen
  - [ ] Data Decls ohne Konstruktoren
  - [x] Pragmas
  - [x] unused Bindings entfernen
  - [x] Data Decl: Konstruktoren entfernen
* Typsignaturen vereinfachen
  - [ ] unnötige Parameter weg
  - [ ] Typparameter durch Unit / () ersetzen
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
* Imports / Exports
  - [ ] Exporte minimieren
  - [ ] Imports: Anzahl importierter Funktionen minimieren
* Misc
  - [ ] Datei formattieren
  - [ ] case-Ausdrücke minimieren
  - [ ] ganze Deklarationen löschen
  - [ ] arithmetische, boolesche Ausdrücke vereinfachen
  - [ ] ganze Decls löschen
  - [ ] Typsignaturen löschen

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