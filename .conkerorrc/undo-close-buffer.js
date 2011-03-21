/**
 * undo-close-buffer
 * Thomas Winant 14/07/2010
 *
 * Saves the urls of the last x buffers you close with
 * kill-and-save-buffer, the buffer last closed can be reopened with
 * undo-kill-buffer. You can also search for a killed buffer to
 * restore with search-killed-buffer
 *
 */
// To check if this page was successfully loaded
loaded_undo_close_buffer = false;

// Don't overwrite the saved_buffers when reloading the rc file
if (typeof saved_buffers == "undefined")
    var saved_buffers = [];
var max_saved_buffers = 30;

function save_buffer(buffer_location) {
    if (saved_buffers[saved_buffers.length-1] != buffer_location) {
        saved_buffers.push(buffer_location);
        if (saved_buffers.length > max_saved_buffers)
            saved_buffers.shift();
    }
}

function get_last_saved_buffer() {
    if (saved_buffers.length > 0)
        return saved_buffers.pop();
    else
        return "about:blank";
}

// Return all the distinct elements of the given array
function distinct(a){
    a.sort();
    for(var i = 1;i < a.length;){
        if(a[i-1] == a[i]){a.splice(i, 1);}
        else{i++;}
    }
    return a;
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

interactive("search-killed-buffer",
            "Search for a killed buffer to reopen.",
            function (I) {
                var found_buffer =
                    yield I.minibuffer.read(
                        $prompt = "Killed buffer to restore:",
                        $completer = all_word_completer(
                            $completions = function(visitor) {
                                for (var i in distinct(saved_buffers)) {
                                    visitor(saved_buffers[i]);
                                }
                            },
                            $get_string = function (x) {return x;}));
                browser_object_follow(I.buffer, OPEN_NEW_BUFFER, found_buffer);
            });

// To check if this page was successfully loaded
loaded_undo_close_buffer = true;