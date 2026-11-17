# Provisional Southern Quechua interface. Unreviewed technical wording remains
# explicitly Spanish instead of presenting invented Quechua rules as authoritative.
language_names <- c(language_names, stats::setNames("qu","Southern Quechua / Runasimi (draft)"))
for(key in names(locale_catalog)) locale_catalog[[key]] <- c(locale_catalog[[key]],qu=unname(locale_catalog[[key]]["es"]))
qu_core <- c(Game="Pukllay",Players="Pukllaqkuna",Language="Simi",cards="Raphikuna",
  "New table"="Musuq mesa", "Your cards"="Qampa raphiykikuna",
  "Wait for your turn"="Suyay", "COMPLETE"="Tukusqa", "(none)"="(mana)")
for(key in names(qu_core)) locale_catalog[[key]]["qu"] <- qu_core[[key]]
action_aliases$qu <- action_aliases$es
# Card vocabulary and legal-action terminology await variety-specific review.
extra_card_locales$qu <- list(template="%s de %s %s",
  suits=c(Hearts="Corazones",Diamonds="Diamantes",Clubs="Treboles",Spades="Picas",Joker="Comodin",
    Oros="Oros",Copas="Copas",Espadas="Espadas",Bastos="Bastos",Denari="Oros",Coppe="Copas",Spade="Espadas",Bastoni="Bastos"),
  french=c(A="As",J="Jota",Q="Reina",K="Rey",Joker="Comodin"),
  spanish=c("1"="As","10"="Sota","11"="Caballo","12"="Rey"),
  italian=c("1"="As","8"="Sota","9"="Caballo","10"="Rey"))
table_help_catalog$qu <- table_help_catalog$es
table_help_catalog$qu$help <- "Yanapay / Ayuda"
table_help_catalog$qu$about <- c("Quechua: borrador pendiente de revision regional. Las instrucciones tecnicas se muestran en espanol.",table_help_catalog$es$about)
