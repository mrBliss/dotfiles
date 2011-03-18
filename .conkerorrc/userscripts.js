
//To check if this page was successfully loaded
loaded_userscripts = false;

//Find the directory with CSS files
var cssdir = cwd.clone();
cssdir.append(".conkerorrc");
cssdir.append("css");

//Load the standard chrome.css
var chrome_css = cssdir.clone();
chrome_css.append("chrome.css");
register_user_stylesheet(chrome_css);

//Load the platform dependent chrome.css
var chrome_os_css = cssdir.clone();
let (os = get_os().toLowerCase()) {
    chrome_os_css.append("chrome-" + os + ".css");
    register_user_stylesheet(chrome_os_css);
};

//Bigger hints (Uppercase as well)
register_user_stylesheet(
    "data:text/css," +
        escape(
            "@namespace url(\"http://www.w3.org/1999/xhtml\");\n" +
                "span.__conkeror_hint {"+
                "font-size: 13px !important;"+
                "line-height: 13px !important;"+
                "text-transform: uppercase !important;"+
                "background: rgba(68, 68, 68, 0.8) !important;"+
                "padding: 3px 5px 3px 5px !important;"+
                "-moz-border-radius: 15px;"+
                "}"));

//Load google-reader.css
let (google_reader_css = cssdir.clone()) {
    google_reader_css.append("google-reader.css");
    register_user_stylesheet(google_reader_css);
};

//Nicer fonts on ubuntuforums.org
register_user_stylesheet(
    make_css_data_uri(["body{font: 12px Helvetica, sans-serif !important;}"],
                      $url_prefixes = "http://ubuntuforums.org/"));

//Nicer fonts on github.com
let (github_font = "Inconsolata, monospace !important") {
    register_user_stylesheet(
        make_css_data_uri(
            ["pre,code{font: 12px " + github_font + ";}",
             ".commit-ref {font-family: " + github_font + ";}",
            "div#browser table td {font: 12px " + github_font + ";}",
            "div.file div.meta div.info {font-family: " + github_font + ";}",
            "table#toc td.path a {font-family: " + github_font + ";}"],
            $url_prefixes = "https://github.com/"));
};

//Tweak wikipedia
register_user_stylesheet(
    make_css_data_uri(["div#p-personal{display:none !important;}"],
                      $url_prefixes = "http://en.wikipedia.org/"));

//Remove crap from engadget.com
register_user_stylesheet(
    make_css_data_uri(["div#adslice-logo {display:none !important;}",
                       "div#fb_container {display:none !important;}",
                       "div#GH_ {display:none !important;}",
                       "div#outerslice {display:none !important;}"],
                      $url_prefixes = "http://www.engadget.com/"));

//Remove Ads from Instapaper.com
register_user_stylesheet(
    make_css_data_uri(["div#bookmarkListDeckAdPlaceholder"
                       + " {display: none !important;}"],
                      $url_prefixes = "http://www.instapaper.com"));


//To check if this page was successfully loaded
loaded_userscripts = true;
