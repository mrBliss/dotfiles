//To check if this page was successfully loaded
loaded_functions = false;

//Duplicate buffer
interactive("duplicate-buffer", "Duplicate buffer",
            function (I) {
                browser_object_follow(I.buffer, OPEN_NEW_BUFFER, I.buffer.current_uri.spec);
            });

//Got the to root of a URL
define_browser_object_class("root-url",
                            "Browser object which returns the root of a URL.",
                            function(I, prompt) {
                                return url_path_trim(I.buffer.current_uri.spec);
                            });
interactive("root",
            "Go to the root of the current URL",
            "find-url",
            $browser_object = browser_object_root_url);

//Delete dom object
interactive("delete", null,
    function (I) {
        var elem = yield read_browser_object(I);
        elem.parentNode.removeChild(elem);
    },
    $browser_object = browser_object_dom_node);

//Check if the rc was valid, i.e. all the js files loaded
function valid_rc (window) {
    var failed = "";
    var msg = "Valid RC";
    if (!loaded_init) failed += "init ";
    if (!loaded_bindings) failed += "bindings ";
    if (!loaded_functions) failed += "functions ";
    if (!loaded_webjumps) failed += "webjumps ";
    if (!loaded_undo_close_buffer) failed += "undo-close-buffer ";
    if (failed != "") msg = "INVALID RC: failed to load " + failed;
    window.minibuffer.message(msg);
}

//Del.icio.us
interactive("valid-rc",
            "Show if the rc file was successfully loaded",
            function (I) {
                valid_rc(I.window);
            });

interactive("delicious-post",
            "bookmark the page via delicious",
            function (I) {
                check_buffer(I.buffer, content_buffer);
                let posturl = 'https://api.del.icio.us/v1/posts/add?&url=' +
                    encodeURIComponent(
                        load_spec_uri_string(
                            load_spec(I.buffer.top_frame))) +
                    '&description=' +
                    encodeURIComponent(
                        yield I.minibuffer.read(
                            $prompt = "name (required): ",
                            $initial_value = I.buffer.title)) +
                    '&tags=' +
                    encodeURIComponent(
                        yield I.minibuffer.read(
                            $prompt = "tags (space delimited): ")) +
                    '&extended=' +
                    encodeURIComponent(
                        yield I.minibuffer.read(
                        $prompt = "extended description: "));

                try {
                    var content = yield send_http_request(
                        load_spec({uri: posturl}));
                    I.window.minibuffer.message(content.responseText);
                } catch (e) { }
            });

interactive("delicious-post-link",
            "bookmark the link via delicious",
            function (I) {
                bo = yield read_browser_object(I) ;
                mylink = load_spec_uri_string(
                    load_spec(encodeURIComponent(bo)));
                check_buffer(I.buffer, content_buffer);
                let postlinkurl = 'https://api.del.icio.us/v1/posts/add?&url=' +
                    mylink +
                    '&description=' +
                    encodeURIComponent(
                        yield I.minibuffer.read(
                            $prompt = "name (required): ",
                            $initial_value = bo.textContent)) +
                    '&tags=' +
                    encodeURIComponent(
                        yield I.minibuffer.read(
                            $prompt = "tags (space delimited): ")) +
                    '&extended=' +
                    encodeURIComponent(
                        yield I.minibuffer.read(
                            $prompt = "extended description: "));

                try {
                    var content = yield send_http_request(
                        load_spec({uri: postlinkurl}));
                    I.window.minibuffer.message(content.responseText);
                } catch (e) { }
            }, $browser_object = browser_object_links);

//Some fonts to chose from
var fonts = ['Helvetica', 'Helvetica Neue', 'Trebuchet MS', 'Monaco', 'Myriad Pro', 'DejaVu Sans Mono', 'Inconsolata', 'Century Old Style Std', 'Bembo Std', 'Grandesign Neue Serif', 'Lucida Mac', 'Lucida Grande'];

//Change font(size) of the body for the given buffer
function change_font(I, font_name, font_size) {
    make_css_data_uri(["body{font: " + font_size + "x "
                       + font_name + " !important;}"],
                      $url_prefixes = url_path_trim(I.buffer.current_uri.spec));
    I.window.minibuffer.message("Changed to " + font_name + " " + font_size + "px for " + url_path_trim(I.buffer.current_uri.spec));
}

interactive("change-font",
            "Prompts the user for a new font and size to use on the current page",
            function (I) {
                var font_name = 
                    yield I.minibuffer.read(
                        $prompt = "New font:",
                        $completer = all_word_completer(
                            $completions = function(push) {
                                for (var i in fonts) {push(fonts[i]);}},
                            $get_string = function (x) {return x;}));
                var font_size =
                    yield I.minibuffer.read($prompt = "New font size:");
                //Quote fonts containing a space
                if (font_name.indexOf(" ") >= 0)
                    font_name = '"' + font_name + '"';
                change_font(I, font_name, font_size);
            });

//To check if this page was successfully loaded
loaded_functions = true;