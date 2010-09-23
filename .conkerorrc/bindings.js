//To check if this page was successfully loaded
loaded_bindings = false;

/*
 * List of a-z bindings: (* indicates my bindings)
 * a = find-url-new-buffer*
 * r = reload
 * R = isearch-continue-backward
 * t = follow-top
 * T = browser-object-text
 * u = up
 * U = root*
 * i = browser-object-images
 * p = delicious-post*
 * P = delicious-post-link*
 * q = kill-current-buffer
 * s = save
 * S = isearch-continue-forward
 * d = follow-new-buffer*
 * D = follow-new-buffer-background*
 * f = follow
 * F = forward
 * g = find-url
 * G = find-alternate-url
 * h = cmd_scrollLeft*
 * j = cmd_scrollLineDown*
 * k = cmd_scrollLineUp*
 * l = cmd_scrollRight*
 * m = browser-object-frames
 * c = copy
 * v = view-as-mime-type
 * b = bookmark
 * B = back
 * n = browser-object-links
 */

//Follow in new buffers: d or D (in background)
define_key(content_buffer_normal_keymap, "d", "follow-new-buffer");
define_key(content_buffer_normal_keymap, "D", "follow-new-buffer-background");

//j/k and h/l VIM-scrolling
define_key(content_buffer_normal_keymap, "j", "cmd_scrollLineDown");
define_key(content_buffer_normal_keymap, "k", "cmd_scrollLineUp");
define_key(content_buffer_normal_keymap, "h", "cmd_scrollLeft");
define_key(content_buffer_normal_keymap, "l", "cmd_scrollRight");

//C-x f is the same as C-x C-f
define_key(default_global_keymap, "C-x f", "find-url-new-buffer");

//a is the same as C-x f
define_key(default_global_keymap, "a", "find-url-new-buffer");

//Use kill-and-save-buffer instead of kill-buffer
define_key(default_global_keymap, "C-x k", "kill-and-save-buffer");
define_key(default_global_keymap, "q", "kill-and-save-buffer");

//Restore the last killed buffer with C-x u, C-T or M-T (bindings I'm used to)
define_key(default_global_keymap, "C-x u", "undo-kill-buffer");
define_key(default_global_keymap, "C-T", "undo-kill-buffer");
define_key(default_global_keymap, "M-T", "undo-kill-buffer");

//Search for a killed buffer to restore with C-x C-u
define_key(default_global_keymap, "C-x C-u", "search-killed-buffer");

//Duplicate buffer with M-d
define_key(content_buffer_normal_keymap, "M-d", "duplicate-buffer");

//Delete a dom node with C-d
define_key(content_buffer_normal_keymap, "C-d", "delete");

//Reinit config
define_key(default_global_keymap, "C-c e", "reinit");
define_key(default_global_keymap, "C-c r", "reinit");

//Del.icio.us
define_key(default_global_keymap, "p", "delicious-post");
define_key(default_global_keymap, "P", "delicious-post-link");

//C-x v checks if the rc is valid
define_key(default_global_keymap, "C-x v", "valid-rc");

//U goes to the root of the url
define_key(content_buffer_normal_keymap, "U", "root");

//Save a session with C-x s and load one with C-x l (replacing the
//current buffer)
define_key(default_global_keymap, "C-x s", "session-save");
define_key(default_global_keymap, "C-x l",
           "session-load-window-current-replace");

//Open a new frame with C-c f
define_key(default_global_keymap, "C-c f", "new-frame");

//Unbind bookmark to b
undefine_key(content_buffer_normal_keymap, "b");

//Use C-M-h and C-M-k to kill a word (backwards)
define_key(text_keymap, 'C-M-h', 'cmd_deleteWordBackward');
define_key(text_keymap, 'C-M-k', 'cmd_deleteWordForward');

//To check if this page was successfully loaded
loaded_bindings = true;