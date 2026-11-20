# Display text for the Hold'em odds panel. Reuse the Help dialog's close label.
locale_catalog <- c(locale_catalog, list(
  "Hand odds" = c(
    es = "Probabilidades de la mano", de = "Handchancen", it = "Probabilit\u00e0 della mano",
    pt = "Probabilidades da m\u00e3o", zh = "\u624b\u724c\u6982\u7387", `zh-Hant` = "\u624b\u724c\u6a5f\u7387"
  ),
  "What if?" = c(
    es = "\u00bfY si...?", de = "Was w\u00e4re, wenn?", it = "E se...?",
    pt = "E se...?", zh = "\u5047\u8bbe\u60c5\u666f", `zh-Hant` = "\u5047\u8a2d\u60c5\u5883"
  ),
  "Current hand" = c(
    es = "Mano actual", de = "Aktuelle Hand", it = "Mano attuale",
    pt = "M\u00e3o atual", zh = "\u5f53\u524d\u624b\u724c", `zh-Hant` = "\u76ee\u524d\u624b\u724c"
  ),
  "Hole card %s" = c(
    es = "Carta privada %s", de = "Eigene Karte %s", it = "Carta privata %s",
    pt = "Carta da m\u00e3o %s", zh = "\u5e95\u724c %s", `zh-Hant` = "\u5e95\u724c %s"
  ),
  "Flop card %s" = c(
    es = "Carta %s del flop", de = "Flop-Karte %s", it = "Carta %s del flop",
    pt = "Carta %s do flop", zh = "\u7ffb\u724c %s", `zh-Hant` = "\u7ffb\u724c %s"
  ),
  "Turn card" = c(
    es = "Carta del turn", de = "Turn-Karte", it = "Carta del turn",
    pt = "Carta do turn", zh = "\u8f6c\u724c", `zh-Hant` = "\u8f49\u724c"
  ),
  "River card" = c(
    es = "Carta del river", de = "River-Karte", it = "Carta del river",
    pt = "Carta do river", zh = "\u6cb3\u724c", `zh-Hant` = "\u6cb3\u724c"
  ),
  "Unknown" = c(
    es = "Desconocida", de = "Unbekannt", it = "Sconosciuta",
    pt = "Desconhecida", zh = "\u672a\u77e5", `zh-Hant` = "\u672a\u77e5"
  ),
  "Opponents in this hand: %s" = c(
    es = "Rivales en esta mano: %s",
    de = "Gegner in dieser Hand: %s",
    it = "Avversari in questa mano: %s",
    pt = "Advers\u00e1rios nesta m\u00e3o: %s",
    zh = "\u672c\u624b\u724c\u7684\u5bf9\u624b\u4eba\u6570\uff1a%s",
    `zh-Hant` = "\u672c\u624b\u724c\u7684\u5c0d\u624b\u4eba\u6578\uff1a%s"
  ),
  "Calculate odds" = c(
    es = "Calcular probabilidades", de = "Chancen berechnen", it = "Calcola le probabilit\u00e0",
    pt = "Calcular probabilidades", zh = "\u8ba1\u7b97\u6982\u7387", `zh-Hant` = "\u8a08\u7b97\u6a5f\u7387"
  ),
  "Reset to current hand" = c(
    es = "Restaurar la mano actual",
    de = "Aktuelle Hand wiederherstellen",
    it = "Ripristina la mano attuale",
    pt = "Restaurar a m\u00e3o atual",
    zh = "\u6062\u590d\u5f53\u524d\u624b\u724c",
    `zh-Hant` = "\u9084\u539f\u76ee\u524d\u624b\u724c"
  ),
  "Back to the table" = vapply(table_help_catalog[setdiff(names(table_help_catalog), "en")],
    function(text) text$close, character(1)),
  "Select Calculate odds to update the result." = c(
    es = "Pulsa Calcular probabilidades para actualizar el resultado.",
    de = "W\u00e4hle Chancen berechnen, um das Ergebnis zu aktualisieren.",
    it = "Seleziona Calcola le probabilit\u00e0 per aggiornare il risultato.",
    pt = "Selecione Calcular probabilidades para atualizar o resultado.",
    zh = "\u9009\u62e9\u201c\u8ba1\u7b97\u6982\u7387\u201d\u4ee5\u66f4\u65b0\u7ed3\u679c\u3002",
    `zh-Hant` = "\u9078\u64c7\u300c\u8a08\u7b97\u6a5f\u7387\u300d\u4ee5\u66f4\u65b0\u7d50\u679c\u3002"
  ),
  "Choose two hole cards." = c(
    es = "Elige dos cartas privadas.",
    de = "W\u00e4hle zwei eigene Karten.",
    it = "Scegli due carte private.",
    pt = "Escolha duas cartas para a m\u00e3o.",
    zh = "\u8bf7\u9009\u62e9\u4e24\u5f20\u5e95\u724c\u3002",
    `zh-Hant` = "\u8acb\u9078\u64c7\u5169\u5f35\u5e95\u724c\u3002"
  ),
  "Choose all three flop cards." = c(
    es = "Elige las tres cartas del flop.",
    de = "W\u00e4hle alle drei Flop-Karten.",
    it = "Scegli tutte e tre le carte del flop.",
    pt = "Escolha as tr\u00eas cartas do flop.",
    zh = "\u8bf7\u9009\u62e9\u5168\u90e8\u4e09\u5f20\u7ffb\u724c\u3002",
    `zh-Hant` = "\u8acb\u9078\u64c7\u5168\u90e8\u4e09\u5f35\u7ffb\u724c\u3002"
  ),
  "Choose a turn card before the river." = c(
    es = "Elige una carta para el turn antes del river.",
    de = "W\u00e4hle vor der River-Karte zuerst die Turn-Karte.",
    it = "Scegli la carta del turn prima di quella del river.",
    pt = "Escolha a carta do turn antes da carta do river.",
    zh = "\u8bf7\u5148\u9009\u62e9\u8f6c\u724c\uff0c\u518d\u9009\u62e9\u6cb3\u724c\u3002",
    `zh-Hant` = "\u8acb\u5148\u9078\u64c7\u8f49\u724c\uff0c\u518d\u9078\u64c7\u6cb3\u724c\u3002"
  ),
  "Each card can appear only once." = c(
    es = "Cada carta puede aparecer una sola vez.",
    de = "Jede Karte darf nur einmal vorkommen.",
    it = "Ogni carta pu\u00f2 comparire una sola volta.",
    pt = "Cada carta pode aparecer apenas uma vez.",
    zh = "\u6bcf\u5f20\u724c\u53ea\u80fd\u51fa\u73b0\u4e00\u6b21\u3002",
    `zh-Hant` = "\u6bcf\u5f35\u724c\u53ea\u80fd\u51fa\u73fe\u4e00\u6b21\u3002"
  ),
  "Choose cards from the standard French deck." = c(
    es = "Elige cartas de la baraja francesa est\u00e1ndar.",
    de = "W\u00e4hle Karten aus einem franz\u00f6sischen Standardblatt.",
    it = "Scegli le carte dal mazzo francese standard.",
    pt = "Escolha cartas do baralho franc\u00eas padr\u00e3o.",
    zh = "\u8bf7\u9009\u62e9\u6807\u51c6\u6cd5\u5f0f\u724c\u7ec4\u4e2d\u7684\u724c\u3002",
    `zh-Hant` = "\u8acb\u9078\u64c7\u6a19\u6e96\u6cd5\u5f0f\u724c\u7d44\u4e2d\u7684\u724c\u3002"
  ),
  "Win at showdown" = c(
    es = "Victoria al mostrar las cartas", de = "Sieg beim Showdown", it = "Vittoria allo showdown",
    pt = "Vit\u00f3ria no showdown", zh = "\u644a\u724c\u83b7\u80dc", `zh-Hant` = "\u6524\u724c\u7372\u52dd"
  ),
  "Tie at showdown" = c(
    es = "Empate al mostrar las cartas", de = "Gleichstand beim Showdown", it = "Pareggio allo showdown",
    pt = "Empate no showdown", zh = "\u644a\u724c\u6253\u5e73", `zh-Hant` = "\u6524\u724c\u5e73\u624b"
  ),
  "A tie means sharing the best hand at showdown." = c(
    es = "Hay empate si tu mano es tan fuerte como la mejor mano rival al mostrar las cartas.",
    de = "Beim Gleichstand ist deine Hand im Showdown genauso stark wie die beste gegnerische Hand.",
    it = "C'\u00e8 un pareggio se la tua mano e la migliore mano avversaria hanno lo stesso valore allo showdown.",
    pt = "H\u00e1 empate quando a sua m\u00e3o e a melhor m\u00e3o advers\u00e1ria t\u00eam o mesmo valor no showdown.",
    zh = "\u5e73\u624b\u8868\u793a\u644a\u724c\u65f6\uff0c\u4f60\u548c\u81f3\u5c11\u4e00\u4f4d\u5bf9\u624b\u62e5\u6709\u540c\u6837\u5f3a\u7684\u6700\u4f73\u724c\u578b\u3002",
    `zh-Hant` = "\u5e73\u624b\u8868\u793a\u6524\u724c\u6642\uff0c\u4f60\u548c\u81f3\u5c11\u4e00\u4f4d\u5c0d\u624b\u64c1\u6709\u540c\u6a23\u5f37\u7684\u6700\u4f73\u724c\u578b\u3002"
  ),
  "Lose at showdown" = c(
    es = "Derrota al mostrar las cartas", de = "Niederlage beim Showdown", it = "Sconfitta allo showdown",
    pt = "Derrota no showdown", zh = "\u644a\u724c\u843d\u8d25", `zh-Hant` = "\u6524\u724c\u843d\u6557"
  ),
  "Flush by the river" = c(
    es = "Color al llegar al river", de = "Flush bis zum River", it = "Colore entro il river",
    pt = "Flush at\u00e9 o river", zh = "\u6cb3\u724c\u65f6\u7ec4\u6210\u540c\u82b1", `zh-Hant` = "\u6cb3\u724c\u6642\u7d44\u6210\u540c\u82b1"
  ),
  "Five cards of one suit among your seven cards, including straight flushes." = c(
    es = "Cinco cartas del mismo palo entre tus siete cartas, incluidas las escaleras de color.",
    de = "F\u00fcnf Karten derselben Farbe unter deinen sieben Karten, einschlie\u00dflich Straight Flushes.",
    it = "Cinque carte dello stesso seme tra le tue sette carte, comprese le scale colore.",
    pt = "Cinco cartas do mesmo naipe entre as suas sete cartas, incluindo straight flushes.",
    zh = "\u4e03\u5f20\u724c\u4e2d\u6709\u4e94\u5f20\u540c\u4e00\u82b1\u8272\u7684\u724c\uff0c\u5305\u62ec\u540c\u82b1\u987a\u3002",
    `zh-Hant` = "\u4e03\u5f35\u724c\u4e2d\u6709\u4e94\u5f35\u540c\u4e00\u82b1\u8272\u7684\u724c\uff0c\u5305\u62ec\u540c\u82b1\u9806\u3002"
  ),
  "Exact showdown calculation (%s outcomes)." = c(
    es = "C\u00e1lculo exacto al mostrar las cartas (%s resultados posibles).",
    de = "Exakte Showdown-Berechnung (%s m\u00f6gliche Ergebnisse).",
    it = "Calcolo esatto per lo showdown (%s esiti possibili).",
    pt = "C\u00e1lculo exato do showdown (%s resultados poss\u00edveis).",
    zh = "\u7cbe\u786e\u8ba1\u7b97\u644a\u724c\u6982\u7387\uff08%s \u79cd\u53ef\u80fd\u7ed3\u679c\uff09\u3002",
    `zh-Hant` = "\u7cbe\u78ba\u8a08\u7b97\u6524\u724c\u6a5f\u7387\uff08%s \u7a2e\u53ef\u80fd\u7d50\u679c\uff09\u3002"
  ),
  "Simulated showdown odds (%s samples)." = c(
    es = "Probabilidades simuladas al mostrar las cartas (%s muestras).",
    de = "Simulierte Showdown-Chancen (%s Stichproben).",
    it = "Probabilit\u00e0 stimate allo showdown (%s simulazioni).",
    pt = "Probabilidades simuladas no showdown (%s amostras).",
    zh = "\u6a21\u62df\u644a\u724c\u6982\u7387\uff08%s \u6b21\u62bd\u6837\uff09\u3002",
    `zh-Hant` = "\u6a21\u64ec\u6524\u724c\u6a5f\u7387\uff08%s \u6b21\u62bd\u6a23\uff09\u3002"
  ),
  "Unknown opposing cards and board cards are drawn at random. Betting and side pots are excluded." = c(
    es = "Las cartas desconocidas de los rivales y de la mesa se sacan al azar. El c\u00e1lculo excluye las apuestas y los botes laterales.",
    de = "Unbekannte Gegner- und Gemeinschaftskarten werden zuf\u00e4llig gezogen. Die Berechnung l\u00e4sst Eins\u00e4tze und Nebenpots au\u00dfer Acht.",
    it = "Le carte sconosciute degli avversari e del tavolo sono estratte a caso. Il calcolo esclude le puntate e i piatti laterali.",
    pt = "As cartas desconhecidas dos advers\u00e1rios e da mesa s\u00e3o sorteadas aleatoriamente. O c\u00e1lculo exclui apostas e potes laterais.",
    zh = "\u5bf9\u624b\u7684\u672a\u77e5\u5e95\u724c\u548c\u672a\u77e5\u516c\u5171\u724c\u5747\u968f\u673a\u62bd\u53d6\u3002\u8ba1\u7b97\u4e0d\u8003\u8651\u4e0b\u6ce8\u548c\u8fb9\u6c60\u3002",
    `zh-Hant` = "\u5c0d\u624b\u7684\u672a\u77e5\u5e95\u724c\u548c\u672a\u77e5\u516c\u5171\u724c\u5747\u96a8\u6a5f\u62bd\u53d6\u3002\u8a08\u7b97\u4e0d\u8003\u616e\u4e0b\u6ce8\u548c\u908a\u6c60\u3002"
  ),
  "95%% simulation interval: %s to %s." = c(
    es = "Intervalo del 95 %% de la simulaci\u00f3n: de %s a %s.",
    de = "95-%%-Simulationsintervall: %s bis %s.",
    it = "Intervallo di simulazione al 95%%: da %s a %s.",
    pt = "Intervalo de simula\u00e7\u00e3o de 95%%: %s a %s.",
    zh = "95%% \u6a21\u62df\u533a\u95f4\uff1a%s \u81f3 %s\u3002",
    `zh-Hant` = "95%% \u6a21\u64ec\u5340\u9593\uff1a%s \u81f3 %s\u3002"
  )
))
