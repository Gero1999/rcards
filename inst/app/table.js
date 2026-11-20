/* Delegated events carry the rendered turn version; the server rejects stale
   clicks. No game, private card, or controller state is stored in JavaScript. */
(function () {
  "use strict";
  // A selection redraws the hand. Keep a keyboard user on the same card as
  // long as the turn and card position are still current.
  window.jQuery(document).on("shiny:value", function (event) {
    if (event.name !== "table_own_cards") return;
    var focused = document.activeElement;
    if (!focused || !focused.matches("#table_own_cards [data-rc-card]")) return;
    var index = focused.dataset.rcCard;
    var version = focused.dataset.rcVersion;
    window.requestAnimationFrame(function () {
      if (document.activeElement !== document.body) return;
      var cards = document.querySelectorAll("#table_own_cards [data-rc-card]");
      var replacement = Array.prototype.find.call(cards, function (card) {
        return card.dataset.rcCard === index && card.dataset.rcVersion === version;
      });
      if (replacement) replacement.focus({preventScroll: true});
    });
  });
  document.addEventListener("click", function (event) {
    var action = event.target.closest("[data-rc-action]");
    var card = event.target.closest("[data-rc-card]");
    var continuation = event.target.closest("[data-rc-continue]");
    var el = action || card || continuation;
    if (!el || el.disabled || !window.Shiny) return;
    var payload = {version: Number(el.dataset.rcVersion)};
    if (continuation) {
      var wager = document.getElementById("next_wager");
      payload.wager = wager && wager.value !== "" ? Number(wager.value) : null;
      Shiny.setInputValue("table_continue", payload, {priority: "event"});
    } else if (action) {
      payload.action = el.dataset.rcAction;
      var amount = document.getElementById("amount");
      var rank = document.getElementById("rank");
      var capture = document.querySelector('input[name="capture"]:checked');
      payload.amount = amount && amount.value !== "" ? Number(amount.value) : null;
      payload.rank = rank ? rank.value : null;
      payload.capture = capture ? capture.value : null;
      Shiny.setInputValue("decision", payload, {priority: "event"});
    } else {
      payload.index = Number(el.dataset.rcCard);
      payload.zone = el.dataset.rcZone;
      Shiny.setInputValue("card_pick", payload, {priority: "event"});
    }
  });
}());
