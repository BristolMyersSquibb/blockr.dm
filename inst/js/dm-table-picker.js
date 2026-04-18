/**
 * dm-table-picker — mounts a Blockr.Select (single or multi) for dm tables.
 *
 * R -> JS message "dm-table-picker":
 *   {
 *     id:        Shiny input id of the picker container,
 *     mode:      "single" | "multi",
 *     options:   [{value, label}] (labels from attr(tbl, "label")),
 *     selected:  string | string[] (matches mode),
 *     placeholder: optional string
 *   }
 *
 * The picker writes its value back to Shiny.setInputValue(id, value) on
 * every change. The message handler initializes the select on first use
 * and calls setOptions() on subsequent messages.
 */
(() => {
  'use strict';

  const mount = (msg) => {
    const root = document.getElementById(msg.id);
    if (!root) return;
    const mode = msg.mode === 'multi' ? 'multi' : 'single';
    const opts = Array.isArray(msg.options) ? msg.options : [];
    const sel  = msg.selected;
    const placeholder = msg.placeholder ||
      (mode === 'multi' ? 'Select tables\u2026' : 'Select table\u2026');

    if (!root._dmPicker) {
      const factory = mode === 'multi' ? Blockr.Select.multi : Blockr.Select.single;
      root._dmPicker = factory(root, {
        options: opts,
        selected: sel,
        placeholder: placeholder,
        onChange: (value) => Shiny.setInputValue(msg.id, value, { priority: 'event' })
      });
      root._dmPicker.el.classList.add('blockr-select--bordered');
      // Sync initial value so the block sees it even if onChange doesn't fire.
      Shiny.setInputValue(msg.id, root._dmPicker.getValue(), { priority: 'event' });
    } else {
      root._dmPicker.setOptions(opts, sel);
    }
  };

  // Shiny may not be ready when this IIFE runs; defer until it is.
  const register = () => {
    if (typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler) {
      setTimeout(register, 50);
      return;
    }
    Shiny.addCustomMessageHandler('dm-table-picker', mount);
  };
  register();
})();
