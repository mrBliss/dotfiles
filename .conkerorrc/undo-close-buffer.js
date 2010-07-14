/**
 * undo-close-buffer
 * Thomas Winant 14/07/2010
 * 
 * Saves the urls of the last x buffers you close with
 * kill-and-save-buffer, the buffer last closed can be reopened with
 * undo-kill-buffer.
 * 
 */

var saved_buffers = [];
var max_saved_buffers = 30;

function save_buffer(buffer_location) {
    saved_buffers.push(buffer_location);
    if (saved_buffers.length > max_saved_buffers)
        saved_buffers.shift();
}

function get_last_saved_buffer() {
    if (saved_buffers.length > 1)
        return saved_buffers.pop();
    else
        return "about:blank";
}

interactive("kill-and-save-buffer",
            "Kills the buffer but saves it so it can be recovered",
            function (I) {
                save_buffer(I.buffer.browser.contentDocument.location.href);
                kill_buffer(I.buffer);
            });

interactive("undo-kill-buffer",
            "Restores the last killed buffer",
            "find-url-new-buffer",
            $browser_object = function () { return get_last_saved_buffer(); });
