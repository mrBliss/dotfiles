
// To check if this page was successfully loaded
loaded_init = false;

require("clicks-in-new-buffer.js");
require("new-tabs.js");
require("session.js");
require("content-buffer.js");
require("favicon.js");
require("block-content-focus-change");
require("page-modes/google-images.js");
require("page-modes/google-reader.js");
require("page-modes/wikipedia.js");

// Homepage
homepage = "http://www.google.be";

// Correct the path on Windows
if (WINDOWS && cwd.path.indexOf("Roaming") < 0) {
    cwd.append("AppData");
    cwd.append("Roaming");
}

// Use the home row as base for the hint numbers
hint_digits = "jklmqsdf";

// Complete new urls with history
url_completion_use_history = true;
url_completion_use_bookmarks = false;

// External editor
editor_shell_command = "emacsclient -c";
view_source_use_external_editor = true;

// Show xkcd titles
xkcd_add_title = true;

// Show url of highlighted node during the hints interaction
hints_display_url_panel = true;

// Open downloads in a buffer
download_buffer_automatic_open_target = OPEN_NEW_BUFFER;

// Autocompletion in the minibuffer
minibuffer_auto_complete_default = true;

// Enable middle click
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND;

// Open links opened in applications in a new window
url_remoting_fn = load_url_in_new_window;

// Sessions
let (dir = cwd.clone()) {
    dir.append(".conkerorrc");
    dir.append("sessions");
    session_dir = dir;
};
session_auto_save_auto_load = "prompt";

// Automatically follow "did you mean" links on wikipedia
wikipedia_enable_didyoumean = true;

// Don't quit when killing the last buffer
can_kill_last_buffer = false;

// Store more history for the minibuffer
minibuffer_history_max_items = 1000;

// Show favicons on tabs
tab_bar_show_icon = true;

// Remove clock from mode-line and add loading_count and buffer_count
remove_hook("mode_line_hook", mode_line_adder(clock_widget));
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);
add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);

// Confirm quit
add_hook("before_quit_hook",
         function () {
             var w = get_recent_conkeror_window();
             var result = (w == null) ||
                 "y" == (yield w.minibuffer.read_single_character_option(
                             $prompt = "Quit Conkeror? (y/n)",
                             $options = ["y", "n"]));
             yield co_return(result);
         });

// Default webjump
read_url_handler_list = [read_url_make_default_webjump_handler("g")];

// Don't change the title (in the title bar)
title_format_fn = function(window) {return "conkeror";};

// Use zathura for PDF documents on GNU/Linux
if (get_os() == "Linux")
    external_content_handlers.set("application/pdf", "zathura");

// Add missing scrollbar in first buffer of window
add_hook("create_buffer_late_hook",
         function (buffer) {
             buffer.top_frame.scrollbars.visible = true;
         });

// To check if this page was successfully loaded
loaded_init = true;