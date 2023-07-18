const Clutter = imports.gi.Clutter;

const ExtensionUtils = imports.misc.extensionUtils;
const Me = ExtensionUtils.getCurrentExtension();
const EXT_NAME = `[${Me.metadata.name}]`;

const Meta = imports.gi.Meta;
const overview = imports.ui.main.overview;


function get_window_actor(window) {
  for (const actor of global.get_window_actors()) {
    if (!actor.is_destroyed() && actor.get_meta_window() === window) {
      return actor;
    }
  }

  return undefined;
}

function cursor_within_window(mouse_x, mouse_y, win) {
  let rect = win.get_frame_rect();

  return mouse_x >= rect.x &&
  mouse_x <= rect.x + rect.width &&
  mouse_y >= rect.y &&
  mouse_y <= rect.y + rect.height;
}

function focus_changed(win) {
  const actor = get_window_actor(win);
  if (actor) {
    let rect = win.get_frame_rect();

    let [mouse_x, mouse_y, _] = global.get_pointer();

    if (cursor_within_window(mouse_x, mouse_y, win) ||
    overview.visible ||
    rect.width < 10 && rect.height < 10) {
      return
    }

    else {
      let seat = Clutter.get_default_backend().get_default_seat()

      if (seat !== null) {
        const mon = global.display.get_monitor_geometry(
          global.display.get_current_monitor()
        )

        const full_width = rect.x + rect.width

        if (full_width > mon.width) {
          seat.warp_pointer(mon.width - (rect.width / 2), rect.height / 2)
        }
        else if (rect.x < 0) {
          seat.warp_pointer(rect.width / 2, rect.height / 2)
        }
        else {
          seat.warp_pointer(rect.x + rect.width / 2, rect.y + rect.height / 2)
        }
      }
    }
  }
}


function connect_to_window(win) {
  const type = win.get_window_type();
  if (type !== Meta.WindowType.NORMAL) {
    dbg_log(`ignoring window, window type: ${type}`);
    return;
  }

  win._mousefollowsfocus_extension_signal = win.connect('focus', focus_changed);
}

function get_focused_window() {
  return global.display.focus_window;
}

class Extension {
  constructor() {
  }

  enable() {
    dbg_log(`enabling ${Me.metadata.name}`);

    for (const actor of global.get_window_actors()) {
      if (actor.is_destroyed()) {
        continue;
      }

      const win = actor.get_meta_window();
      connect_to_window(win);
    }

    this.create_signal = global.display.connect('window-created', function (ignore, win) {
      dbg_log(`window created ${win}`);

      connect_to_window(win);
    });

    this.hide_signal = overview.connect('hidden', function() {
      // the focus might change whilst we're in the overview, i.e. by
      // searching for an already open app.
      const win = get_focused_window();
      if (win !== null) {
        focus_changed(win)
      }
    });
  }

  disable() {
    dbg_log(`disabling ${Me.metadata.name}`);

    if (this.create_signal !== undefined) {
      global.display.disconnect(this.create_signal);
      this.create_signal = undefined;
    }

    if (this.hide_signal !== undefined) {
      overview.disconnect(this.hide_signal);
      this.hide_signal = undefined;
    }

    for (const actor of global.get_window_actors()) {
      if (actor.is_destroyed()) {
        continue;
      }

      const win = actor.get_meta_window();
      if (win._mousefollowsfocus_extension_signal) {
        win.disconnect(win._mousefollowsfocus_extension_signal);
        delete win._mousefollowsfocus_extension_signal;
      }
    }
  }
}


function init() {
  dbg_log(`initializing ${Me.metadata.name}`);

  return new Extension();
}
