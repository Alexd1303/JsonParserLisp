# JsonParserLisp

Dubini Alessandro 885957<br>
Foggetti Mattia 885958<br>
Martin Elia Leonardo 886366<br>

Introduzione

Lo sviluppo di applicazioni web su Internet, ma non solo, richiede di scambiare dati fra applicazioni
eterogenee, ad esempio tra un client web scritto in Javascript e un server, e viceversa. Uno standard
per lo scambio di dati molto diffuso è lo standard JavaScript Object Notation, o JSON. Lo scopo di
questo progetto è di realizzare due librerie, una in Prolog e l’altra in Common Lisp, che costruiscano
delle strutture dati che rappresentino degli oggetti JSON a partire dalla loro rappresentazione come
stringhe.

La descrizione dei metodi è contenuta nei commenti del codice.

La sintassi delle stringhe JSON

La sintassi JSON è definita nel sito https://www.json.org.
Dalla grammatica data, un oggetto JSON può essere scomposto ricorsivamente nelle seguenti parti:
1. Object
2. Array
3. Value
4. String
5. Number


Esempi Lisp

CL-prompt> (defparameter x (jsonparse "{\"nome\" : \"Arthur\",
 \"cognome\" : \"Dent\"}"))
X

CL-prompt> x
(JSONOBJ ("nome" "Arthur") ("cognome" "Dent"))

CL-prompt> (jsonaccess x "cognome")
"Dent"

CL-prompt> (jsonaccess (jsonparse
 "{\"name\" : \"Zaphod\",
 \"heads\" : [[\"Head1\"], [\"Head2\"]]}")
 "heads" 1 0)
"Head2"

CL-prompt> (jsonparse "[1, 2, 3]")
(JSONARRAY 1 2 3)

CL-prompt> (jsonparse "{}")
(JSONOBJ)

CL-prompt> (jsonparse "[]")
(JSONARRAY)

CL-prompt> (jsonparse "{]")
ERROR: syntax error

CL-prompt> (jsonaccess (jsonparse " [1, 2, 3] ") 3)
ERROR: …
