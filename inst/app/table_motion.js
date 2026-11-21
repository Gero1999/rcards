/* Animate new public board cards and the last rendered pot. Rendering and
   game state remain authoritative when animation is unavailable. */
(function () {
  "use strict";

  document.addEventListener("DOMContentLoaded", function () {
    var output = document.getElementById("table");
    if (!output || !window.MutationObserver || !window.Element ||
        typeof window.Element.prototype.animate !== "function") return;

    var reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
    var currentHand;
    var dealt = 0;
    var boardFrame;
    var pot;

    function table() {
      return output.querySelector(".rc-table[data-rc-hand]");
    }

    function animateBoard() {
      boardFrame = undefined;
      var view = table();
      if (!view) return;

      var hand = view.dataset.rcHand;
      var board = view.querySelector('[data-zone="board"]');
      var cards = board ? Array.from(board.querySelectorAll(".rc-card")) : [];
      if (currentHand === undefined) {
        currentHand = hand;
        // An app can open with a public board already in play, before Shiny
        // mounts the nested card output for that initial snapshot.
        dealt = Number(view.dataset.rcBoardSize) || cards.length;
        return;
      }
      if (currentHand !== hand) {
        currentHand = hand;
        dealt = 0;
      }
      // The nested Shiny board output can be empty until its content is restored.
      if (cards.length <= dealt) return;
      var added = cards.slice(dealt);
      dealt = cards.length;
      if (reducedMotion.matches) return;

      added.forEach(function (card, index) {
        card.dataset.rcMotion = "deal";
        var motion = card.animate([
          { opacity: 0, transform: "translateY(-7px) scale(.98)" },
          { opacity: 1, transform: "none" }
        ], { duration: 220, delay: 32 * index, easing: "ease-out", fill: "backwards" });
        function clear() { delete card.dataset.rcMotion; }
        motion.finished.then(clear, clear);
      });
    }

    function scheduleBoard() {
      if (boardFrame === undefined) boardFrame = window.requestAnimationFrame(animateBoard);
    }

    function visibleCenter(rect) {
      var x = rect.left + rect.width / 2;
      var y = rect.top + rect.height / 2;
      if (rect.width > 0 && rect.height > 0 && x >= 0 && x < window.innerWidth &&
          y >= 0 && y < window.innerHeight) return { x: x, y: y };
      return null;
    }

    window.jQuery(document).on("shiny:value", function (event) {
      if (event.name !== "table" || reducedMotion.matches) return;
      var view = table();
      if (!view) return;
      var stack = view.querySelector("[data-rc-pot] .rc-chip-stack");
      if (!stack) return;
      var rect = stack.getBoundingClientRect();
      pot = visibleCenter(rect) ? {
        hand: view.dataset.rcHand,
        rect: rect,
        stack: stack.cloneNode(true),
        scrollX: window.scrollX,
        scrollY: window.scrollY
      } : null;
    });

    function flyPot(origin, view, winners) {
      var rect = origin.rect;
      if (reducedMotion.matches || origin.scrollX !== window.scrollX ||
          origin.scrollY !== window.scrollY) return;
      var from = visibleCenter(rect);
      if (!from) return;

      winners.forEach(function (seat, index) {
        var target = view.querySelector('[data-rc-seat="' + seat + '"]');
        var to = target && visibleCenter(target.getBoundingClientRect());
        if (!to) return;

        var clone = origin.stack.cloneNode(true);
        clone.classList.add("rc-pot-flight");
        clone.setAttribute("aria-hidden", "true");
        clone.style.left = rect.left + "px";
        clone.style.top = rect.top + "px";
        clone.style.width = rect.width + "px";
        clone.style.height = rect.height + "px";
        document.body.appendChild(clone);
        var motion = clone.animate([
          { opacity: 1, transform: "translate(0, 0) scale(1)" },
          { opacity: .85, offset: .72 },
          { opacity: 0, transform: "translate(" + (to.x - from.x) + "px, " +
            (to.y - from.y) + "px) scale(.65)" }
        ], { duration: 450, delay: 24 * index, easing: "ease-in-out", fill: "both" });
        function clear() { clone.remove(); }
        motion.finished.then(clear, clear);
      });
    }

    document.addEventListener("rc:table-event", function (event) {
      var banner = event.detail;
      var winners = banner && banner.dataset && banner.dataset.rcPotWinners;
      if (!winners || reducedMotion.matches) return;
      winners = winners.split(",").filter(function (seat) { return /^[1-9][0-9]*$/.test(seat); });
      window.requestAnimationFrame(function () {
        var view = table();
        var origin = pot;
        if (!banner.isConnected || !origin || !view || origin.hand !== view.dataset.rcHand) return;
        pot = null;
        flyPot(origin, view, winners);
      });
    });

    new MutationObserver(scheduleBoard).observe(output, { childList: true, subtree: true });
    scheduleBoard();
  });
}());
