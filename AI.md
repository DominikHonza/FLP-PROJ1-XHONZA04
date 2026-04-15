# Využití AI nástrojů v projektu

## Využité nástroje + odkaz na konverzace
- ChatGPT - https://chatgpt.com/share/69df84d4-c050-8386-98e9-be7fae9bbf62

## Prvotní nastavení AI na počátku konverzace

Konverzaci jsem uvedl promptem, kterým sem se pokusil uvést AI do role
vedoucího/poradce při vypracovávání projektu a donutit ho neposkytovat mi přímo výsledek/řešení nebo případně hotový kód, ale nabízet nápovědy.

První uvozovací prompt:
```
Čau mám zadaný projekt v Haskellu potřebuji abys mi dělal vedoucího a radil jak mám postupovat a napovídal kdybych se zaseknul, nedávej mi přímo řešení ani kódy, lae pokud ti neco poslu neboj se to orpavit když si reknu. Potrenuju abys mi napovidal a vedle jak pokracovat s s projektem a pripadne vysvetlil kdyz se nekde seknu proc a jak. Pridavam do zdroju zadani at orientacne vis o co go, projekt sem si rozjel a pridavam i readme z repa, pojdme tedy na ten CLI.hs coz je prvni cast ukolu, co bych mel ocekavat jak se bude chovat po supusteni kdyz udelam CLI.hs ? Musim rovnou dodefinovat ty undefined funkce aby se to rozběhlo ?
```

## Workflow využívání AI jako poradce

AI jsem využíval primárně pro nápovědy a orientaci v zadání.

Hlavní využití spočívalo v nasměrování, které části kodu doplňovat jako další a v jakém pořadí, případně dohledávání typů v Types.hs.

V druhé půlce projektu se mi osvědčilo při nepochopení zadání si nechat vysvětlit, co se po mně ve funkci chce příkladem v c sharpu (na dvou místech je v projektu pro mou lepší orientaci tento hint ponechaný). 

V závěrečné části jsem ho po vyplění funkcí používal na ověření, že implmentace funkce davá nějak smysl, než budu schopný zbuildit a spustit projekt jako takový.

Úplně nakonec jsem ho po spuštění využil na opravu a dohledání importů potřebných pro jednotlivé funkce, které sem využil dle hintů v zádání funkcí. Dále také na opravu názvů proměnných, které neseděly.

Okrajově jsem AI využil na refaktorizaci, ale ta většinou zavedla syntax, které sem nerozumněl a proto je ponechána jen v jendé pomocné funkci trim a pár okrajových případech, předvším šlo o přepis lamda funkcí a skládání funkcí.

Jedinou části funkce generovanou AI je vnitřní funkce passedTests ve funkci computeStats ve Report.hs. Zde je také umístěný disclaimer o využití AI.

Kontrola archivu pro odevzdání.

**Konkrétní příklady využití:**
- vysvětlení chybových hlášek z překladače GHC
- návrh typových signatur funkcí
- pochopení práce s IO a pattern matchingem
- orientaci v cizím kódu (Types.hs)
