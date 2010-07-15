//To check if this page was successfully loaded
loaded_webjumps = false;

//Webjumps
define_webjump("imdb", "http://imdb.com/find?q=%s");
define_webjump("yt", "http://www.youtube.com/results?search_query=%s&search=Search");
define_webjump("tr", "http://translate.google.com/translate_t#auto|en|%s");
define_webjump("down?",
  function (url) {
    if (url) {
      return "http://downforeveryoneorjustme.com/" + url;
    } else {
      return "javascript:window.location.href='http://downforeveryoneorjustme.com/'+window.location.href;";
    }
  },
  $argument = "optional",
  $completer = history_completer($use_history = true, $use_bookmarks = false));
wikipedia_webjumps_format = "w%s";
define_wikipedia_webjumps("en", "nl");
define_webjump("bin", "http://binsearch.info/?q=%s&max=250&adv_age=240");
define_webjump("bier", "http://www.bierdopje.com/search/shows/%s");
define_webjump("sub", "http://ondertitel.com/?type=&trefwoord=%s&p=zoek");
define_webjump("nlsub", "http://nlondertitels.com/search/q/%s/1");
define_webjump("wolf", "http://www.wolframalpha.com/input/?i=%s");
define_webjump("tor", "http://torrentz.com/search?q=%s");
define_webjump("amazon", "http://www.amazon.com/s/?field-keywords=%s");
define_webjump("cov", "http://images.google.com/images?q=%s%20imagesize%3A300x300");
define_webjump("mer", "http://www.merriam-webster.com/dictionary/%s");
define_webjump("tvdb", "http://thetvdb.com/?string=%s&tab=listseries&function=Search");
define_webjump("elbow", "http://elbo.ws/mp3s/?q=%s");
define_webjump("dict", "http://www.google.com/dictionary?q=%s&hl=en&langpair=en|en&spell=1&oi=spell");
define_webjump("img", "http://images.google.com/images?q=%s");
add_delicious_webjumps("dewinant");
define_lastfm_webjumps("dewinant");
define_webjump("del", "http://delicious.com/search?p=%s&chk=&context=userposts%7Cdewinant&fr=del_icio_us&lc=");

//TODO XPather


//To check if this page was successfully loaded
loaded_webjumps = true;