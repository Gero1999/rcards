# The help catalog is separate from console prompts because it contains whole
# game explanations. Canonical game types and action protocols stay unchanged.
table_help_catalog <- list(
  en=list(
    help="Help",close="Back to the table",about_title="About cardgame",
    about=c("A local card table for you and bots. Choose a game, player count, and language, then start a table.",
      "Your cards, public information, and legal moves are shown on the table. Results stay visible until you choose to continue. New table starts fresh; refreshing the browser also resets this session."),
    game_title="How to play",rules_link="Full supported rules",
    contribute_title="Bugs and new games",bug_link="Report a bug",game_link="Suggest a game or improvement",
    contribute="These links open the project's GitHub repository. For a bug, include the game, language, steps to reproduce, and what you expected. For a new game, describe its rules and player count.",
    games=list(
      holdem=c("Use your two hole cards and five community cards to make the best five-card poker hand. Betting proceeds through preflop, flop, turn, and river; a hand ends at showdown or when only one player remains.",
        "Fold, check, call, or raise when offered. A raise is the TOTAL you will have contributed in the current betting round, including chips already committed. The displayed bounds show legal totals.",
        "Next hand retains stacks and rotates the dealer and blinds. Players without chips sit out; no chips are added automatically."),
      mus=c("Four players form opposite-seat partnerships. Each receives four cards. All four must request mus to exchange cards; a cut starts the lances. When discarding, select any cards to replace, or none to stand pat.",
        "Compete at grande (high cards), chica (low cards), pares (pairs), and juego (a qualifying total), or punto when nobody has juego. Use the legal pass, bet, raise, accept, reject, and ordago choices shown. Bets and raises specify the total stake.",
        "Next hand retains partnership scores and rotates mano. The configured target is 30 or 40 points. New match starts again at zero after a match ends, with the same players and rules."),
      blackjack=c("Try to beat the dealer without exceeding 21. Aces count as 1 or 11. The dealer follows the configured rules; an original two-card 21 is a natural.",
        "Hit draws a card; stand keeps the total. Double adds a matching wager, draws one card, and stands. Split separates equal ranks into two hands with an extra wager. Only currently legal actions are offered, and the active split hand is highlighted.",
        "Next round retains your bankroll and uses the next wager you choose. A fresh complete shoe is shuffled each round; this interface does not keep a continuous shoe. You need enough chips for your chosen wager."),
      scopa=c("Play one hand card. A table card of the same value must be captured if available; otherwise take a combination adding up to the played value. If no capture exists, leave the card on the table.",
        "Select your card, then choose a numbered capture option. The matching table positions are highlighted. Clearing the table earns a scopa except on the final play of a deal.",
        "Score for most cards, most coins, the seven of coins, primiera, and scopas. Two or three players compete individually; four play in opposite-seat partnerships. Next hand keeps scores; New match resets them after the configured target is reached."),
      liar=c("Select one or more cards to discard face down and announce a rank. Claims keep the same rank until a challenge. You may bluff; the legal card limit is shown.",
        "During a response window, challenge the latest claim or pass to decline a challenge. Passing here does not skip a discard turn. A caught bluffer picks up the pile; otherwise the challenger does. Only the latest claimed batch is revealed.",
        "Your final cards must survive the response window before you win. Next deal reshuffles the same configured pack and rotates the dealer, keeping the same players.")
    )
  ),
  es=list(
    help="Ayuda",close="Volver a la mesa",about_title="Acerca de cardgame",
    about=c("Una mesa local para jugar contra bots. Elige un juego, el n\u00famero de jugadores y el idioma; despu\u00e9s inicia la mesa.",
      "La mesa muestra tus cartas, la informaci\u00f3n p\u00fablica y las acciones legales. Los resultados permanecen hasta que decidas continuar. Nueva mesa empieza desde cero; actualizar el navegador tambi\u00e9n reinicia esta sesi\u00f3n."),
    game_title="C\u00f3mo jugar",rules_link="Reglas completas admitidas",
    contribute_title="Errores y nuevos juegos",bug_link="Informar de un error",game_link="Proponer un juego o una mejora",
    contribute="Estos enlaces abren el repositorio del proyecto en GitHub. Para un error, indica el juego, idioma, pasos para reproducirlo y resultado esperado. Para un juego nuevo, describe las reglas y el n\u00famero de jugadores.",
    games=list(
      holdem=c("Combina tus dos cartas privadas y las cinco comunitarias para formar la mejor mano de cinco cartas. Se apuesta en preflop, flop, turn y river; la mano termina al mostrar las cartas o cuando queda un solo jugador.",
        "Ret\u00edrate, pasa, iguala o sube cuando est\u00e9 permitido. Una subida indica el TOTAL aportado en esta ronda de apuestas, incluidas las fichas ya puestas. Los l\u00edmites mostrados indican los totales legales.",
        "Siguiente mano conserva las fichas y rota el repartidor y las ciegas. Quien no tenga fichas queda fuera de la mano; no se a\u00f1aden fichas autom\u00e1ticamente."),
      mus=c("Cuatro jugadores forman parejas en asientos opuestos y reciben cuatro cartas. Todos deben pedir mus para cambiar cartas; cortar inicia los lances. Al descartar, selecciona las cartas que quieras cambiar, o ninguna para quedarte servido.",
        "Se compite en grande, chica, pares y juego, o punto si nadie tiene juego. Usa las opciones legales de pasar, apostar, subir, aceptar, rechazar y \u00f3rdago que aparezcan. Las apuestas y subidas indican el total del envite.",
        "Siguiente mano conserva los puntos y rota la mano. El objetivo configurado es de 30 o 40 puntos. Nueva partida empieza desde cero al terminar la anterior, con los mismos jugadores y reglas."),
      blackjack=c("Intenta superar a la banca sin pasar de 21. Los ases valen 1 u 11. La banca sigue las reglas configuradas; un 21 con las dos cartas originales es un blackjack natural.",
        "Pedir a\u00f1ade una carta; plantarse mantiene el total. Doblar a\u00f1ade otra apuesta igual, pide una carta y planta. Dividir separa cartas del mismo valor en dos manos con otra apuesta. Solo se ofrecen acciones legales y se resalta la mano activa.",
        "Siguiente ronda conserva tu saldo y usa la siguiente apuesta que elijas. Cada ronda baraja un zapato completo nuevo; no se conserva un zapato continuo. Necesitas fichas suficientes para la apuesta elegida."),
      scopa=c("Juega una carta de tu mano. Si hay una carta del mismo valor en la mesa, debes capturarla; si no, captura una combinaci\u00f3n que sume el valor jugado. Si no existe captura, deja la carta en la mesa.",
        "Selecciona tu carta y una opci\u00f3n de captura numerada. Se resaltan las posiciones correspondientes de la mesa. Vaciar la mesa suma una scopa, salvo en la \u00faltima jugada del reparto.",
        "Punt\u00faan la mayor\u00eda de cartas, oros, el siete de oros, primiera y las scopas. Dos o tres juegan individualmente; cuatro forman parejas opuestas. Siguiente mano conserva los puntos; Nueva partida los reinicia tras alcanzar el objetivo configurado."),
      liar=c("Selecciona cartas para descartarlas boca abajo y declara un valor. El valor se mantiene hasta un desaf\u00edo. Puedes mentir; se muestra el m\u00e1ximo de cartas permitido.",
        "Durante la respuesta, desaf\u00eda la \u00faltima declaraci\u00f3n o pasa para no desafiar. Pasar aqu\u00ed no permite saltarse el turno de descartar. El mentiroso descubierto recoge la pila; si dec\u00eda la verdad, la recoge quien desaf\u00eda. Solo se revela el \u00faltimo grupo declarado.",
        "Tus \u00faltimas cartas deben superar la ventana de respuesta para ganar. Siguiente reparto baraja el mismo tipo de mazo y rota el repartidor, con los mismos jugadores.")
    )
  ),
  de=list(
    help="Hilfe",close="Zur\u00fcck zum Tisch",about_title="\u00dcber cardgame",
    about=c("Ein lokaler Kartentisch f\u00fcr dich und Bots. W\u00e4hle Spiel, Spielerzahl und Sprache und starte dann den Tisch.",
      "Der Tisch zeigt deine Karten, \u00f6ffentliche Informationen und erlaubte Z\u00fcge. Ergebnisse bleiben sichtbar, bis du fortsetzt. Neuer Tisch beginnt von vorn; auch das Neuladen des Browsers setzt diese Sitzung zur\u00fcck."),
    game_title="Spielanleitung",rules_link="Vollst\u00e4ndiges unterst\u00fctztes Regelprofil",
    contribute_title="Fehler und neue Spiele",bug_link="Fehler melden",game_link="Spiel oder Verbesserung vorschlagen",
    contribute="Diese Links \u00f6ffnen das GitHub-Repository des Projekts. Gib bei Fehlern Spiel, Sprache, Schritte zum Nachstellen und das erwartete Ergebnis an. Beschreibe bei neuen Spielen Regeln und Spielerzahl.",
    games=list(
      holdem=c("Bilde aus deinen zwei verdeckten Karten und f\u00fcnf Gemeinschaftskarten die beste Pokerhand mit f\u00fcnf Karten. Gesetzt wird in Preflop, Flop, Turn und River. Die Hand endet beim Showdown oder wenn nur ein Spieler bleibt.",
        "W\u00e4hle Aussteigen, Schieben, Mitgehen oder Erh\u00f6hen, soweit erlaubt. Eine Erh\u00f6hung ist dein GESAMTEINSATZ dieser Setzrunde einschlie\u00dflich bereits gesetzter Chips. Die angezeigten Grenzen gelten f\u00fcr diesen Gesamtbetrag.",
        "N\u00e4chste Hand beh\u00e4lt Chipst\u00e4nde bei und verschiebt Button und Blinds. Spieler ohne Chips setzen aus; es werden keine Chips automatisch hinzugef\u00fcgt."),
      mus=c("Vier Spieler bilden gegen\u00fcbersitzende Partnerschaften und erhalten je vier Karten. Alle m\u00fcssen Mus verlangen, um Karten zu tauschen; ein Abbruch startet die Lances. W\u00e4hle beim Tausch beliebige Karten oder keine, um die Hand zu behalten.",
        "Gespielt werden Grande (hohe Karten), Chica (niedrige Karten), Pares (Paare) und Juego (qualifizierte Summe), sonst Punto. Nutze die angezeigten erlaubten Optionen zum Passen, Setzen, Erh\u00f6hen, Annehmen, Ablehnen und \u00d3rdago. Eins\u00e4tze und Erh\u00f6hungen nennen den Gesamtbetrag.",
        "N\u00e4chste Hand beh\u00e4lt Teampunkte bei und wechselt Mano. Das eingestellte Ziel betr\u00e4gt 30 oder 40 Punkte. Nach Spielende beginnt Neues Match mit denselben Spielern und Regeln wieder bei null."),
      blackjack=c("Schlage die Bank, ohne 21 zu \u00fcberschreiten. Asse z\u00e4hlen 1 oder 11. Die Bank folgt den eingestellten Regeln; 21 aus den urspr\u00fcnglichen zwei Karten ist ein nat\u00fcrlicher Blackjack.",
        "Ziehen gibt eine Karte; Halten beh\u00e4lt die Summe. Verdoppeln setzt denselben Betrag nach, zieht eine Karte und h\u00e4lt. Teilen trennt gleiche R\u00e4nge mit einem weiteren Einsatz in zwei H\u00e4nde. Nur erlaubte Aktionen werden angeboten; die aktive Hand ist markiert.",
        "N\u00e4chste Runde beh\u00e4lt dein Guthaben und verwendet den von dir gew\u00e4hlten n\u00e4chsten Einsatz. Jede Runde mischt einen vollst\u00e4ndigen neuen Kartenschlitten; Karten werden nicht runden\u00fcbergreifend weiterverwendet. Du brauchst genug Chips f\u00fcr den gew\u00e4hlten Einsatz."),
      scopa=c("Spiele eine Handkarte. Eine gleichwertige Tischkarte muss genommen werden, falls vorhanden; sonst nimm eine Kombination mit derselben Summe. Gibt es keinen Fang, bleibt die Karte auf dem Tisch.",
        "W\u00e4hle deine Karte und eine nummerierte Fangoption. Die passenden Tischpositionen werden markiert. Das Leeren des Tisches bringt eine Scopa, au\u00dfer beim letzten Zug eines Deals.",
        "Punkte gibt es f\u00fcr die meisten Karten, M\u00fcnzen, die M\u00fcnzen-Sieben, Primiera und Scopas. Zwei oder drei spielen einzeln; vier bilden gegen\u00fcbersitzende Teams. N\u00e4chste Hand beh\u00e4lt Punkte, Neues Match setzt sie nach Erreichen des Ziels zur\u00fcck."),
      liar=c("W\u00e4hle Karten zum verdeckten Ablegen und sage einen Wert an. Der Wert bleibt bis zu einem Zweifel gleich. Bluffen ist erlaubt; die maximale Kartenzahl wird angezeigt.",
        "In der Antwortphase kannst du die letzte Ansage anzweifeln oder passen. Passen \u00fcberspringt hier keinen Ablagezug. Bei einem Bluff nimmt der Ansager den Stapel, sonst der Zweifler. Nur die zuletzt angesagten Karten werden gezeigt.",
        "Deine letzten Karten m\u00fcssen die Antwortphase \u00fcberstehen, bevor du gewinnst. N\u00e4chster Deal mischt denselben Kartensatz und wechselt den Geber mit denselben Spielern.")
    )
  ),
  it=list(
    help="Aiuto",close="Torna al tavolo",about_title="Informazioni su cardgame",
    about=c("Un tavolo locale per giocare contro i bot. Scegli gioco, numero di giocatori e lingua, poi avvia il tavolo.",
      "Il tavolo mostra le tue carte, le informazioni pubbliche e le mosse consentite. I risultati restano visibili finch\u00e9 decidi di continuare. Nuovo tavolo ricomincia da zero; anche aggiornare il browser azzera questa sessione."),
    game_title="Come giocare",rules_link="Regole complete supportate",
    contribute_title="Errori e nuovi giochi",bug_link="Segnala un errore",game_link="Proponi un gioco o un miglioramento",
    contribute="Questi link aprono il repository del progetto su GitHub. Per un errore indica gioco, lingua, passaggi per riprodurlo e risultato atteso. Per un gioco nuovo descrivi regole e numero di giocatori.",
    games=list(
      holdem=c("Forma la migliore mano di cinque carte usando le tue due carte private e le cinque comuni. Si punta in preflop, flop, turn e river; la mano finisce allo showdown o quando resta un solo giocatore.",
        "Lascia, passa, vedi o rilancia quando consentito. Un rilancio indica il TOTALE versato in questo giro di puntate, comprese le fiches gi\u00e0 messe. I limiti mostrati valgono per questo totale.",
        "Mano successiva conserva le fiches e ruota bottone e bui. Chi non ha fiches salta la mano; non vengono aggiunte fiches automaticamente."),
      mus=c("Quattro giocatori formano coppie nei posti opposti e ricevono quattro carte. Tutti devono chiedere mus per cambiare carte; tagliare avvia i lanci. Quando scarti seleziona le carte da cambiare, oppure nessuna per tenerle tutte.",
        "Si compete in grande (carte alte), chica (carte basse), pares (coppie) e juego (totale valido), oppure punto se nessuno ha juego. Usa le opzioni consentite per passare, puntare, rilanciare, accettare, rifiutare e \u00f3rdago. Puntate e rilanci indicano la posta totale.",
        "Mano successiva conserva i punti delle coppie e ruota mano. L'obiettivo configurato \u00e8 di 30 o 40 punti. Nuova partita riparte da zero dopo la fine, con gli stessi giocatori e regole."),
      blackjack=c("Prova a battere il banco senza superare 21. Gli assi valgono 1 o 11. Il banco segue le regole configurate; 21 con le due carte originali \u00e8 un blackjack naturale.",
        "Carta aggiunge una carta; stare mantiene il totale. Raddoppiare aggiunge una puntata uguale, pesca una carta e si ferma. Dividere separa valori uguali in due mani con un'altra puntata. Sono offerte solo azioni consentite e la mano attiva \u00e8 evidenziata.",
        "Round successivo conserva il saldo e usa la prossima puntata scelta. Ogni round mescola un mazzo completo nuovo; il sabot non continua fra i round. Servono fiches sufficienti per la puntata scelta."),
      scopa=c("Gioca una carta dalla mano. Se sul tavolo c'\u00e8 una carta dello stesso valore devi prenderla; altrimenti prendi una combinazione con somma uguale. Se non ci sono prese lascia la carta sul tavolo.",
        "Seleziona la tua carta e una presa numerata. Le posizioni corrispondenti sul tavolo vengono evidenziate. Svuotare il tavolo vale una scopa, tranne nell'ultima giocata della smazzata.",
        "Si segnano punti per carte, denari, settebello, primiera e scope. Due o tre giocano individualmente; quattro formano coppie nei posti opposti. Mano successiva conserva i punti; Nuova partita li azzera dopo il raggiungimento dell'obiettivo."),
      liar=c("Seleziona carte da scartare coperte e dichiara un valore. Il valore resta lo stesso fino a una contestazione. Puoi bluffare; il limite di carte \u00e8 mostrato.",
        "Nella fase di risposta contesta l'ultima dichiarazione oppure passa. Passare qui non permette di saltare un turno di scarto. Chi bluffa prende il mucchio se scoperto; altrimenti lo prende chi contesta. Si mostra solo l'ultimo gruppo dichiarato.",
        "Le tue ultime carte devono superare la fase di risposta prima della vittoria. Smazzata successiva rimescola lo stesso mazzo configurato e ruota il mazziere, con gli stessi giocatori.")
    )
  )
)

table_help_text <- function(language="en") table_help_catalog[[console_language(language)]]

# Only the bundled original SVG is embedded. A data URI works in installed
# packages and development checkouts without public routes or remote requests.
table_brand_tag <- function() {
  path <- system.file("app","logo.svg",package="cardgame")
  if(!nzchar(path)) stop("The bundled cardgame logo is unavailable")
  svg <- paste(readLines(path,warn=FALSE),collapse="\n")
  shiny::tags$div(class="rc-brand",shiny::tags$img(class="rc-logo",alt="cardgame",
    width=96,height=111,src=paste0("data:image/svg+xml;charset=UTF-8,",utils::URLencode(svg,reserved=TRUE))))
}

table_help_dialog <- function(view,language="en") {
  text <- table_help_text(language)
  type <- view$game_type
  if(!is.character(type) || length(type)!=1L || is.na(type) || !type %in% names(text$games))
    stop("Choose a supported game for help")
  names <- c(holdem="Texas Hold'em",mus="Mus",blackjack="Blackjack",scopa="Scopa",liar="Mentiroso")
  link <- function(label,url) shiny::tags$a(label,href=url,target="_blank",rel="noopener noreferrer")
  dialog <- shiny::modalDialog(
    title=shiny::tags$span(id="rcards-help-title",text$help),
    shiny::tags$div(class="rc-help",
      shiny::tags$section(shiny::tags$h3(text$about_title),lapply(text$about,shiny::tags$p)),
      shiny::tags$section(`data-rc-help-game`=type,
        shiny::tags$h3(paste0(text$game_title,": ",names[[type]])),
        shiny::tags$ol(lapply(text$games[[type]],shiny::tags$li)),
        link(text$rules_link,paste0("https://github.com/Gero1999/rcards/blob/main/docs/rules/",type,".md"))),
      shiny::tags$section(shiny::tags$h3(text$contribute_title),shiny::tags$p(text$contribute),
        shiny::tags$p(link(text$bug_link,"https://github.com/Gero1999/rcards/issues/new?title=Bug%3A%20")),
        shiny::tags$p(link(text$game_link,"https://github.com/Gero1999/rcards/issues/new?title=Game%20suggestion%3A%20")))),
    footer=shiny::modalButton(text$close),easyClose=TRUE,size="m")
  dialog$attribs$role <- "dialog"
  dialog$attribs$class <- paste(dialog$attribs$class,"rc-help-modal")
  dialog$attribs[["aria-modal"]] <- "true"
  dialog$attribs[["aria-labelledby"]] <- "rcards-help-title"
  dialog
}
