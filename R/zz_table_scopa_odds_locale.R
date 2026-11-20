# Display text for the Scopa move comparison.
locale_catalog <- c(locale_catalog, list(
  "Compare moves" = c(
    es = "Comparar jugadas", de = "Spielz\u00fcge vergleichen", it = "Confronta le mosse",
    pt = "Comparar jogadas", zh = "\u6bd4\u8f83\u51fa\u724c", `zh-Hant` = "\u6bd4\u8f03\u51fa\u724c"
  ),
  "Card to play" = c(
    es = "Carta a jugar", de = "Karte zum Ausspielen", it = "Carta da giocare",
    pt = "Carta a jogar", zh = "\u6253\u51fa\u7684\u724c", `zh-Hant` = "\u6253\u51fa\u7684\u724c"
  ),
  "Capture" = c(
    es = "Captura", de = "Fang", it = "Presa",
    pt = "Captura", zh = "\u6355\u83b7", `zh-Hant` = "\u6355\u7372"
  ),
  "No capture" = c(
    es = "Sin captura", de = "Kein Fang", it = "Nessuna presa",
    pt = "Sem captura", zh = "\u4e0d\u6355\u83b7", `zh-Hant` = "\u4e0d\u6355\u7372"
  ),
  "Expected deal margin (points)" = c(
    es = "Diferencia esperada en el reparto (puntos)",
    de = "Erwartete Rundendifferenz (Punkte)",
    it = "Margine atteso nella smazzata (punti)",
    pt = "Margem esperada na rodada (pontos)",
    zh = "\u672c\u526f\u724c\u9884\u671f\u5206\u5dee\uff08\u5206\uff09",
    `zh-Hant` = "\u672c\u526f\u724c\u9810\u671f\u5206\u5dee\uff08\u5206\uff09"
  ),
  "95% simulation interval (points)" = c(
    es = "Intervalo de simulaci\u00f3n del 95 % (puntos)",
    de = "95-%-Simulationsintervall (Punkte)",
    it = "Intervallo di simulazione al 95% (punti)",
    pt = "Intervalo de simula\u00e7\u00e3o de 95% (pontos)",
    zh = "95% \u6a21\u62df\u533a\u95f4\uff08\u5206\uff09",
    `zh-Hant` = "95% \u6a21\u64ec\u5340\u9593\uff08\u5206\uff09"
  ),
  "Chance to finish the deal ahead" = c(
    es = "Probabilidad de acabar el reparto por delante",
    de = "Chance auf einen Vorsprung am Rundenende",
    it = "Probabilit\u00e0 di chiudere la smazzata in vantaggio",
    pt = "Probabilidade de terminar a rodada na frente",
    zh = "\u672c\u526f\u724c\u7ed3\u675f\u65f6\u9886\u5148\u7684\u6982\u7387",
    `zh-Hant` = "\u672c\u526f\u724c\u7d50\u675f\u6642\u9818\u5148\u7684\u6a5f\u7387"
  ),
  "%s simulated deals per move." = c(
    es = "%s repartos simulados por jugada.",
    de = "%s simulierte Runden pro Spielzug.",
    it = "%s smazzate simulate per mossa.",
    pt = "%s rodadas simuladas por jogada.",
    zh = "\u6bcf\u79cd\u51fa\u724c\u65b9\u5f0f\u6a21\u62df %s \u526f\u3002",
    `zh-Hant` = "\u6bcf\u7a2e\u51fa\u724c\u65b9\u5f0f\u6a21\u64ec %s \u526f\u3002"
  ),
  "The margin is your side's deal points minus the best other side's deal points." = c(
    es = "La diferencia se calcula restando a tus puntos del reparto los del rival o equipo rival con m\u00e1s puntos.",
    de = "Die Differenz ergibt sich aus den Rundenpunkten deiner Seite abz\u00fcglich der Rundenpunkte der besten gegnerischen Seite.",
    it = "Il margine \u00e8 la differenza tra i punti della tua parte e quelli della parte avversaria con pi\u00f9 punti nella smazzata.",
    pt = "A margem \u00e9 a diferen\u00e7a entre os pontos do seu lado e os do lado advers\u00e1rio com mais pontos na rodada.",
    zh = "\u5206\u5dee\u7b49\u4e8e\u4f60\u65b9\u672c\u526f\u724c\u7684\u5f97\u5206\u51cf\u53bb\u5f97\u5206\u6700\u9ad8\u7684\u5bf9\u65b9\u7684\u672c\u526f\u724c\u5f97\u5206\u3002",
    `zh-Hant` = "\u5206\u5dee\u7b49\u65bc\u4f60\u65b9\u672c\u526f\u724c\u7684\u5f97\u5206\u6e1b\u53bb\u5f97\u5206\u6700\u9ad8\u7684\u5c0d\u65b9\u7684\u672c\u526f\u724c\u5f97\u5206\u3002"
  ),
  "The simulation keeps visible cards and captures fixed, samples unseen cards uniformly, and uses baseline bots to finish the deal." = c(
    es = "La simulaci\u00f3n conserva las cartas visibles y las capturas. Todas las distribuciones posibles de las cartas ocultas son igual de probables y unos bots b\u00e1sicos completan el reparto.",
    de = "Sichtbare Karten und bisherige F\u00e4nge bleiben unver\u00e4ndert. Alle m\u00f6glichen Verteilungen unbekannter Karten sind gleich wahrscheinlich und einfache Bots spielen die Runde zu Ende.",
    it = "La simulazione mantiene fisse le carte visibili e le prese. Tutte le distribuzioni possibili delle carte nascoste hanno la stessa probabilit\u00e0 e bot di base completano la smazzata.",
    pt = "A simula\u00e7\u00e3o mant\u00e9m as cartas vis\u00edveis e as capturas. Todas as distribui\u00e7\u00f5es poss\u00edveis das cartas ocultas t\u00eam a mesma probabilidade e bots b\u00e1sicos terminam a rodada.",
    zh = "\u6a21\u62df\u4fdd\u7559\u53ef\u89c1\u724c\u548c\u5df2\u6355\u83b7\u7684\u724c\u3002\u672a\u77e5\u724c\u7684\u6bcf\u79cd\u53ef\u80fd\u5206\u914d\u7b49\u6982\u7387\u62bd\u53d6\uff0c\u7531\u57fa\u7840\u673a\u5668\u4eba\u5b8c\u6210\u672c\u526f\u724c\u3002",
    `zh-Hant` = "\u6a21\u64ec\u4fdd\u7559\u53ef\u898b\u724c\u548c\u5df2\u6355\u7372\u7684\u724c\u3002\u672a\u77e5\u724c\u7684\u6bcf\u7a2e\u53ef\u80fd\u5206\u914d\u7b49\u6a5f\u7387\u62bd\u53d6\uff0c\u7531\u57fa\u790e\u6a5f\u5668\u4eba\u5b8c\u6210\u672c\u526f\u724c\u3002"
  )
))
