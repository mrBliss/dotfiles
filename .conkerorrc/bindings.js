//To check if this page was succesfully loaded
loaded_bindings = false;

/*
 * List of a-z bindings: (* indicates my bindings)
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

//Duplicate buffer with M-d
define_key(content_buffer_normal_keymap, "M-d", "duplicate-buffer");

//Confirm exit
define_key(default_global_keymap, "C-x C-c", "confirm-quit");

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

//To check if this page was succesfully loaded
loaded_bindings = true;