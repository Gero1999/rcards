/* The live region stays mounted between turns. Only new public events enter it. */
(function () {
  "use strict";

  document.addEventListener("DOMContentLoaded", function () {
    var region = document.getElementById("table_event");
    if (!region) return;
    var currentBanner;

    new MutationObserver(function () {
      var banner = region.querySelector("[data-rc-table-event]");
      if (!banner || banner === currentBanner) return;
      currentBanner = banner;

      window.requestAnimationFrame(function () {
        window.requestAnimationFrame(function () {
          if (!banner.isConnected) return;
          banner.classList.add("rc-event-visible");
          document.dispatchEvent(new CustomEvent("rc:table-event", {detail: banner}));
        });
      });
      window.setTimeout(function () {
        banner.classList.add("rc-event-leaving");
      }, 2800);
      window.setTimeout(function () {
        banner.hidden = true;
      }, 3000);
    }).observe(region, {childList: true, subtree: true});
  });
}());
