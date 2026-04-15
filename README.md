# Projekt pro funkcionální část FLP 2025/26

## Výsledný stav
Ve výsledném projektu jsem se téměř neodchýlil od šablony a změnami byly pouze přidání pár importů standardních knihoven. Mezi nimi:

- `Data.Char` – pro využití isSpace
- `Data.List` – pro využití dropWhileEnd
- `Control.Monad` – pro využití forM
- `Control.Exception` – pro využití try
- `Data.Maybe` – pro využití fromMaybe

A další standardní knihovny bez kterých vz nebylo možné využít doporučené funkce. Přidání bylo konzultováno s AI viz AI.md.

Žadné rozšíření nebylo implementováno a dle pokynů v zadání funkcí byly příslušné proměnné a přepínače jako `useRegex` ignorovány.

V projektu jsem používal dokumentační styl Haddock pro komentování funkcí a datových typů přímo ve zdrojovém kódu dle zadání. 
Primárně vždy šlo o:
- Zakomentování letmého popisu funkce
- Sekvenční popsání logiky pomocí Behavior
- Popis návratových hodnot a formát návratového objektu
- Interní poznámky
- Příklad v jiném porgramovacím jazyce
- Disclaimer k využití AI

## Ne/Problémy při řešení
Téměř na žádné větší problémy jsem při implementaci nenarazil. Postupoval jsem logicky po souborech a bylo docela jednoduché a přirozené je vyplnit. 

Velmi atypicky jsem byl schopný projekt zpravoznit, vypracovat a zkompletovat v jendom průchodu implementace a následné refektorizaci (primárně komentářů).

Jediný problém byla vniřtní funkce passedTest, kterou jsem nebyl schopný vymyslet viz AI.md.


## Zdroje

- THE GLASGOW HASKELL COMPILER TEAM. *GHC User's Guide*. [online]. Dostupné z: https://downloads.haskell.org/~ghc/latest/docs/users_guide.pdf

- HASKELL COMMUNITY. *Hackage: Haskell Package Documentation*. [online]. Dostupné z: https://hackage.haskell.org/

- HASKELL HADDOCK TEAM. *Haddock Documentation: Markup*. [online]. Dostupné z: https://haskell-haddock.readthedocs.io/latest/markup.html

- Materiály k předmětu FLP, Fakulta informačních technologií.

- OpenAI. *ChatGPT – konzultace při řešení projektu*. [online]. Dostupné z: viz soubor `AI.md`

## Feedback k projektu

Zadání i kostra projektu mi přijdou hezky zpracované, ale přišlo mi, že by bylo vhodné studenta poslat si README před tím než dostane nálož specifikace chování tesů a spouštění parseru s interpretem. Z zadání mi bylo prvně nejasné zda bude součástí i nějaké implementace právě toho spouštění nebo vyhodnocení.