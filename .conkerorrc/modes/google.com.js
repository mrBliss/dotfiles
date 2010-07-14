/**
 * page-mode for search results on Google.com
 * j = highlight next result
 * k = highlight previous result
 * h = previous page
 * l = next page
 **/

require("content-buffer.js");

define_variable("google_end_behavior", "page",
                "Controls the behavior of the commands google-next-link and "+
                "google-prev-link when at the last or first link, respectively. "+
                "Given as a string, the supported values are 'stop', 'wrap', "+
                "and 'page'.  'stop' means to not move the highlight in any "+
                "way.  'wrap' means to wrap around to the first (or last) "+
                "link.  'page' means to navigate the buffer to the next (or "+
                "previous) page on google.");

register_user_stylesheet(
    "data:text/css," +
        escape (
            "@-moz-document url-prefix(http://www.google.com/) {" +
                ".last-clicked {" +
                " background-color: #bfb !important;" +
                " border: 0px !important;"+
                "}}"));


/* Scroll, if necessary, to make the given element visible */
function google_scroll_into_view (window, element) {
    var rect = element.getBoundingClientRect();
    if (rect.top < 0 || rect.bottom > window.innerHeight)
        element.scrollIntoView();
}


/* Move select the next link down from the currently highlighted one.
 * When the end of the page is reached, the behavior is controlled by
 * the variable google_end_behavior.
 */
function google_next (I) {
    var doc = I.buffer.document;
    // the behavior of this command depends on whether we have downloaded
    // enough of the page to include all of the article links.
    var complete = doc.getElementById('fll');
    var links = doc.getElementsByClassName('g');
    var first = null;
    var current = null;
    var next = null;
    for (var i = 0, llen = links.length; i < llen; i++) {
        if (links[i].style.display == 'none')
            continue;
        if (! first)
            first = links[i];
        if (current) {
            next = links[i];
            break;
        }
        if (links[i].className.indexOf("last-clicked") >= 0)
            current = links[i];
    }
    // The following situations are null-ops:
    //  1) there are no links on the page.
    //  2) page is incomplete and the current link is the last link.
    if (!first || (current && !next && !complete))
        return;
    if (! next) {
        if (current) {
            if (google_end_behavior == 'stop')
                return;
            if (google_end_behavior == 'wrap')
                next = first;
            if (google_end_behavior == 'page') {
                let (xpr = doc.evaluate(
                         '//a[@class="pn"][child::a[text()="Next"]]', doc, null,
                         Ci.nsIDOMXPathResult.ORDERED_NODE_ITERATOR_TYPE, null))
                {
                    let nextpage;
                    if (xpr && (nextpage = xpr.iterateNext())) {
                        dom_remove_class(current, "last-clicked");
                        browser_object_follow(I.buffer, FOLLOW_DEFAULT, nextpage);
                        return;
                    }
                };
            }
        } else {
            // Page may or may not be complete.  If the page is not
            // complete, it is safe to assume that there is no current
            // link because a current link can only persist on a
            // cached page, which would load instantaneously, not
            // giving the user the opportunity to run this command.
            //
            next = first;
        }
    }
    // ordinaries (highlight new, maybe dehighlight old)
    if (current)
        dom_remove_class(current, "last-clicked");
    dom_add_class(next, "last-clicked");
    let (anchor = doc.evaluate(
             '//*[contains(@class,"last-clicked")]//a[@class="l"]',
             next, null, Ci.nsIDOMXPathResult.FIRST_ORDERED_NODE_TYPE, null))
    {
        browser_set_element_focus(I.buffer, anchor.singleNodeValue);
    };
    google_scroll_into_view(I.buffer.focused_frame, next);
}
interactive("google-next-link",
            "Move the 'cursor' to the next search result.",
            google_next);


/* Select the link before the currently highlighted one.  When the
 * beginning of the page is reached, behavior is controlled by the
 * variable google_end_behavior.
 */
function google_prev (I) {
    var doc = I.buffer.document;
    // the behavior of this command depends on whether we have downloaded
    // enough of the page to include all of the article links.
    var complete = doc.getElementById('fll');
    var links = doc.getElementsByClassName('g');
    var llen = links.length;
    var first = null;
    var prev = null;
    var current = null;
    for (var i = 0; i < llen; i++) {
        if (links[i].style.display == 'none')
            continue;
        if (! first)
            first = links[i];
        if (links[i].className.indexOf("last-clicked") >= 0) {
            current = links[i];
            break;
        }
        prev = links[i];
    }
    if (! first || // no links were found at all.
        (!current && !complete)) // don't know where current is.
        return;
    if (! prev) {
        // the first visible link is the `current' link.
        // dispatch on google_end_behavior.
        if (google_end_behavior == 'stop')
            return;
        else if (google_end_behavior == 'wrap') {
            // need to get last link on page.
            if (complete) {
                for (var i = 0; i < llen; i++) {
                    if (links[i].style.display == 'none')
                        continue;
                    prev = links[i];
                }
            }
        } else if (google_end_behavior == 'page') {
            let (xpr = doc.evaluate(
                     '//a[@class="pn"][child::a[text()="Previous"]]', doc, null,
                     Ci.nsIDOMXPathResult.ORDERED_NODE_ITERATOR_TYPE, null))
            {
                let prevpage;
                if (xpr && (prevpage = xpr.iterateNext())) {
                    dom_remove_class(current, "last-clicked");
                    browser_object_follow(I.buffer, FOLLOW_DEFAULT, prevpage);
                    return;
                }
            };
        }
    }
    // ordinaries (highlight new, maybe dehighlight old)
    if (current)
        dom_remove_class(current, "last-clicked");
    dom_add_class(prev, "last-clicked");
    let (anchor = doc.evaluate(
             '//*[contains(@class,"last-clicked")]//a[@class="l"]',
             prev, null, Ci.nsIDOMXPathResult.FIRST_ORDERED_NODE_TYPE, null))
    {
        browser_set_element_focus(I.buffer, anchor.singleNodeValue);
    };
    google_scroll_into_view(I.buffer.focused_frame, prev);
}
interactive("google-prev-link",
            "Move the 'cursor' to the previous search result.",
            google_prev);



define_browser_object_class("google-current", null,
                            function (I, prompt) {
                                var xpr = I.buffer.document.evaluate(
                                    '//*[contains(@class,"last-clicked")]/*[@class="g"]/a',
                                    I.buffer.document, null,
                                    Ci.nsIDOMXPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
                                yield co_return(xpr.iterateNext());
                            });


define_keymap("google_keymap");
define_key(google_keymap, "j", "google-next-link");
define_key(google_keymap, "k", "google-prev-link");

// function google_modality (buffer, element) {
//     // terse but hacky way to get the effect we want.  the current "correct"
//     // way would be to write an entire long dispatcher like that used for the
//     // basic content-buffer modality in content-buffer-input.js.  we really
//     // need some abstraction to let us tersely express when to push keymaps.
//     if (! buffer.input_mode)
//         buffer.keymaps.push(google_keymap);
// }

define_page_mode("google_mode",
                 $display_name = "google",
                 $enable = function (buffer) {
                     // let (cmds = ["follow-current",
                     //              "follow-current-new-buffer",
                     //              "follow-current-new-buffer-background",
                     //              "follow-current-new-window",
                     //              "copy"]) {
                     //     for each (var c in cmds) {
                     //         buffer.default_browser_object_classes[c] =
                     //             browser_object_google_current;
                     //     }
                     // }
                     buffer.modalities.push(google_modality);
                 },
                 $keymaps = {normal_input_mode: google_keymap},
                 $disable = function (buffer) {
                     var i = buffer.modalities.indexOf(google_modality);
                     if (i > -1)
                         buffer.modalities.splice(i, 1);
                 },
                 $doc = "google page-mode: keyboard navigation for google.");

let (re = build_url_regex($domain = "google",
                          $allow_www = true,
                          $tlds = ["com", "be"],
                          $path = /search.+/)) {
    auto_mode_list.push([re, google_mode]);
};
