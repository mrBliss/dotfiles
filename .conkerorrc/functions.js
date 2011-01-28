//To check if this page was successfully loaded
loaded_functions = false;

//Duplicate buffer
interactive("duplicate-buffer", "Duplicate buffer",
            function (I) {
                browser_object_follow(I.buffer, OPEN_NEW_BUFFER, I.buffer.current_uri.spec);
            });

//Go to the root of a URL
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
    if (!loaded_userscripts) failed += "userscripts ";
    if (failed != "") msg = "INVALID RC: failed to load " + failed;
    window.minibuffer.message(msg);
}

interactive("valid-rc",
            "Show if the rc file was successfully loaded",
            function (I) {
                valid_rc(I.window);
            });

interactive("new-frame",
            "Opens a new frame",
            function (I) {
                browser_object_follow(I.buffer, OPEN_NEW_WINDOW, "about:blank");
            });

//Opens a new buffer with the selected image
interactive("view-image", "View the image in a buffer",
    function (I) {
        var image = yield read_browser_object(I);
        browser_object_follow(I.buffer, OPEN_NEW_BUFFER, image.src);
    },
    $browser_object = browser_object_images,
    $prompt = "View image:");

interactive("scrollDownMore",
            "Scroll down more than one line, less than a page",
            function (I) {
                for (var i = 1; i <= 10; i++)
                call_builtin_command(I.window, "cmd_scrollLineDown");
            });

interactive("scrollUpMore",
            "Scroll up more than one line, less than a page",
            function (I) {
                for (var i = 1; i <= 10; i++)
                call_builtin_command(I.window, "cmd_scrollLineUp");
            });

interactive("add-google-bookmark",
            "Bookmark the page via Google Bookmarks",
            function (I) {
                check_buffer(I.buffer, content_buffer);
                let (enc = encodeURIComponent,
                     title = I.buffer.title,
                     url = load_spec_uri_string(load_spec(I.buffer.top_frame)))
                    browser_object_follow(
                        I.buffer, OPEN_NEW_BUFFER,
                        'http://www.google.com/bookmarks/mark?op=edit'
                            + '&output=popup&bkmk=' + enc(url)
                            + '&title=' + enc(title));
            });

interactive("read-later",
            "Save a web page to Instapaper for reading later",
            function (I) {
                check_buffer(I.buffer, content_buffer);
                let doc = I.buffer.document;
                let newscript = doc.createElement('script');
                let srcurl = 'https://www.instapaper.com/j/xlSmtS0T8o6c?u='
                    + encodeURIComponent(doc.location.href) + '&t='
                    + (new Date().getTime());
                newscript.setAttribute('src', srcurl);
                doc.body.appendChild(newscript);
            });

interactive("subscribe-rss",
            "Subscribes to first encountered RSS feed with Google Reader",
            function (I) {
                let found = false;
                let reader = 'http://google.com/ig/add?feedurl=';
                let doc = I.buffer.document;
                let links = doc.getElementsByTagName("link");
                for(var i = 0, l; l = links[i]; i++) {
                    let type = l.getAttribute('type');
                    let rel = l.getAttribute('rel');
                    if (type && (type == 'application/rss+xml'
                              || type == 'application/atom+xml')
                        && rel && rel == 'alternate') {
                        let h = l.getAttribute('href');
                        if (h.indexOf('http') != 0) {
                            let p = (h.indexOf('/') != 0)
                                ? '/' : doc.location.pathname;
                            h = 'http://' + doc.location.hostname + p + h;
                        }
                        doc.location = reader + h;
                        found = true;
                    }
                }
                if(!found) I.minibuffer.message('Couldn\'t find a feed.');
            });


//To check if this page was successfully loaded
loaded_functions = true;