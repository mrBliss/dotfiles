//To check if this page was successfully loaded
loaded_webjumps = false;

//Webjumps
define_webjump("imdb", "http://imdb.com/find?q=%s",
              $description = "Search for movies and actors on IMDB");
define_webjump("yt", "http://www.youtube.com/results?search_query=%s&search=Search",
               $description = "Search for Youtube videos");
define_webjump("tr", "http://translate.google.com/translate_t#auto|en|%s",
               $description = "Translate a word or webpage to english");
define_webjump("down?",
  function (url) {
    if (url) {
      return "http://downforeveryoneorjustme.com/" + url;
    } else {
      return "javascript:window.location.href='http://downforeveryoneorjustme.com/'+window.location.href;";
    }
  },
  $argument = "optional",
  $description = "Is this site or a given site down?",
  $completer = history_completer($use_history = true, $use_bookmarks = false));

define_webjump("amazon", "http://www.amazon.com/s/?field-keywords=%s",
               $description = "Search products on Amazon.com");
define_webjump("bin", "http://binsearch.info/?q=%s&max=250&adv_age=240",
              $description = "Search NZBs on Binsearch.net");
define_webjump("bier", "http://www.bierdopje.com/search/shows/%s",
               $description = "Search subtitles for tv shows on Bierdopje.com");
define_webjump("cov", "http://images.google.com/images?q=%s%20imagesize%3A300x300",
               $description = "Search for 300x300 album covers with Google Images");
define_webjump("dict", "http://www.google.com/dictionary?q=%s&hl=en&langpair=en|en&spell=1&oi=spell",
               $description = "Look up a word in Google's dictionary");
define_webjump("elbow", "http://elbo.ws/mp3s/?q=%s",
               $description = "Search for MP3s on Elbo.ws");
define_webjump("emw", "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&q=%s&sa=Search&siteurl=www.emacswiki.org%2F",
               $description = "Search on the Emacs Wiki");
define_webjump("g", "http://www.google.com/search?q=%s&ie=utf-8&oe=utf-8&aq=t",
               $description = "Google Search");
define_webjump("gb", "https://www.google.com/bookmarks/l#!view=threadsmgmt&q=%s",
               $description = "Google Bookmarks Search");
define_webjump("idiom", "http://www.idiomdictionary.com/search.php?search=%s",
               $description = "Search for idioms on IdiomDictionary");
define_webjump("img", "http://images.google.com/images?q=%s",
               $description = "Search for images on Google Images");
define_webjump("lastfm", "http://last.fm/music/%s",
               $description = "Search for an artist on Last.fm");
define_webjump("mer", "http://www.merriam-webster.com/dictionary/%s",
               $description = "Look up a word on Merriam-Webster");
define_webjump("nlsub", "http://nlondertitels.com/search/q/%s/1",
               $description = "Search subtitles on NLOndertitels.com");
define_webjump("sub", "http://ondertitel.com/?type=&trefwoord=%s&p=zoek",
               $description = "Search subtitles on Ondertitel.com");
define_webjump("tor", "http://torrentz.com/search?q=%s",
               $description = "Search torrents on Torrentz.com");
define_webjump("tvdb", "http://thetvdb.com/?string=%s&tab=listseries&function=Search",               
               $description = "Search for a tv show on TheTVDB.com");
define_webjump("w", "http://en.wikipedia.org/wiki/Special:Search?search=%s&go=Go",
               $description = "Look something up on the English Wikipedia");
define_webjump("wnl", "http://nl.wikipedia.org/wiki/Special:Search?search=%s&go=Go",
               $description = "Look something up on the Dutch Wikipedia");
define_webjump("wolf", "http://www.wolframalpha.com/input/?i=%s",
               $description = "Calculate something with WolframAlpha");


//Add missing descriptions
webjumps['conkerorwiki'].description = "Search the Conkeror Wiki";
webjumps['dictionary'].description = "Look up a word in an online dictionary";
webjumps['image'].description = "Search for images on Google Images";
webjumps['maps'].description = "Look up a place on Google Maps";
webjumps['slang'].description = "Look up slang word in the Urban Dictionary";


//TODO XPather

//Remove unused webjumps
var unused_webjumps = ['answers','bugzilla','buildd','buildd-ports','clhs','cliki','clusty','creativecommons','debbugs','debfile','debpkg','debpopcon','debpts','debqa','freshmeat','google','kuro5hin','launchpad','lucky','ratpoisonwiki','savannah','sadelicious','scholar','sdelicious','slashdot','sourceforge','stumpwmwiki','ubuntubugs','ubuntufile','ubuntupkg','wiktionary','yahoo'];

for (var i=0; i<unused_webjumps.length; i++) {
    delete webjumps[unused_webjumps[i]];
}

//To check if this page was successfully loaded
loaded_webjumps = true;