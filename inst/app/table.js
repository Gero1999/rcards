/* Delegated events carry the rendered turn version; the server rejects stale
   clicks. No game, private card, or controller state is stored in JavaScript. */
(function () {
  "use strict";
  document.addEventListener("click", function (event) {
    var action = event.target.closest("[data-rc-action]");
    var card = event.target.closest("[data-rc-card]");
    var continuation = event.target.closest("[data-rc-continue]");
    var el = action || card || continuation;
    if (!el || el.disabled || !window.Shiny) return;
    var payload = {version: Number(el.dataset.rcVersion)};
    if (continuation) {
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
