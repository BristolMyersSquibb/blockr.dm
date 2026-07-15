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

  // Messages that arrived before their container existed (a lazy dockview
  // panel mounts long after the block server flushed its restore push).
  // Held keyed by id, latest wins, replayed when the element turns up.
  // No expiry: a never-mounted panel just keeps its pending entry.
  const pending = {};

  const replayPending = () => {
    Object.keys(pending).forEach((id) => {
      if (document.getElementById(id)) {
        const msg = pending[id];
        delete pending[id];
        mount(msg);
      }
    });
  };

  const mount = (msg) => {
    const root = document.getElementById(msg.id);
    if (!root) {
      pending[msg.id] = msg;
      return;
    }
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
    // Dockview panels enter the DOM after the fact; every render/bind pass
    // is a chance that a pending picker's container now exists.
    $(document).on('shiny:value shiny:bound', () => {
      setTimeout(replayPending, 100);
    });
  };
  register();
})();
