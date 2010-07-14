//To check if this page was succesfully loaded
loaded_init = false;

require("clicks-in-new-buffer.js");
require("new-tabs.js");
require("session.js");
require("content-buffer.js");
require("page-modes/google-images.js");
require("page-modes/wikipedia.js");

//Homepage
homepage = "http://www.google.be";

//Complete new urls with history
url_completion_use_history = true;
url_completion_use_bookmarks = false;

//External editor
editor_shell_command = "emacsclient -c -n";

//Show xkcd titles
xkcd_add_title = true;

//Show url of highlighted node during the hints interaction
hints_display_url_panel = true;

//Open downloads in a buffer
download_buffer_automatic_open_target = OPEN_NEW_BUFFER;

//Autocompletion in the minibuffer
minibuffer_auto_complete_default = true;

//Enable middle click
clicks_in_new_buffer_target = OPEN_NEW_BUFFER;

//Open links opened in applications in a new buffer
url_remoting_fn = load_url_in_new_buffer;

//Sessions
let (dir = get_home_directory()) {
    dir.append(".conkerorrc");
    dir.append("sessions");
    session_dir = dir;
};
session_auto_save_auto_load = "prompt";

//Automatically follow "did you mean" links on wikipedia
wikipedia_enable_didyoumean = true;

//Load stylesheet
let (sheet = get_home_directory()) {
    sheet.append(".conkerorrc");
    sheet.append("css");
    sheet.append("chrome.css");
    register_user_stylesheet(make_uri(sheet));
};

//Don't quit when killing the last buffer
can_kill_last_buffer = false;

//Store more history for the minibuffer
minibuffer_history_max_items = 1000;

//Remove clock from mode-line and add loading_count and buffer_count
remove_hook("mode_line_hook", mode_line_adder(clock_widget));
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);
add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);

//To check if this page was succesfully loaded
loaded_init = true;