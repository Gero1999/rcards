/* Expand and validate the raise editor before the table's action handler runs. */
(function () {
  "use strict";

  function update(editor) {
    var amount = editor.querySelector("#amount");
    var confirm = editor.querySelector("[data-rc-raise-label]");
    var total = amount.valueAsNumber;
    var valid = Number.isInteger(total) && amount.validity.valid;
    amount.setAttribute("aria-invalid", String(!valid));
    confirm.disabled = !valid;
    confirm.querySelector(".rc-action-label").textContent =
      confirm.dataset.rcRaiseLabel.replace("%s", valid ? String(total) : "\u2014");
    return valid;
  }

  function close(editor) {
    var toggle = editor.parentElement.querySelector("[data-rc-raise-toggle]");
    var amount = editor.querySelector("#amount");
    amount.value = amount.defaultValue;
    update(editor);
    editor.hidden = true;
    toggle.hidden = false;
    toggle.setAttribute("aria-expanded", "false");
    toggle.focus();
  }

  document.addEventListener("click", function (event) {
    var trigger = event.target.closest("[data-rc-raise-toggle]");
    if (trigger) {
      var opened = trigger.parentElement.querySelector("[data-rc-raise-editor]");
      trigger.setAttribute("aria-expanded", "true");
      trigger.hidden = true;
      opened.hidden = false;
      update(opened);
      opened.querySelector("#amount").focus();
      return;
    }
    var cancel = event.target.closest("[data-rc-raise-cancel]");
    if (cancel) {
      close(cancel.closest("[data-rc-raise-editor]"));
      return;
    }
    var confirm = event.target.closest("[data-rc-raise-label]");
    if (!confirm) return;
    var editor = confirm.closest("[data-rc-raise-editor]");
    if (editor.hidden || !update(editor)) event.stopPropagation();
  }, true);

  document.addEventListener("input", function (event) {
    if (!event.target.matches("#amount")) return;
    var editor = event.target.closest("[data-rc-raise-editor]");
    if (editor) update(editor);
  });

  document.addEventListener("keydown", function (event) {
    if (event.key !== "Escape") return;
    var editor = event.target.closest("[data-rc-raise-editor]");
    if (!editor || editor.hidden) return;
    event.preventDefault();
    close(editor);
  });
}());
