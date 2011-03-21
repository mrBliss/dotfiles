// Readability
// http://lab.arc90.com/experiments/readability/
interactive("readability_arc90",
            "Readability is a simple tool that makes reading on the web more enjoyable by removing the clutter around what you are reading",
            function readability_arc90(I) {
                var document = I.window.buffers.current.document;

                _readability_readStyle=document.createElement('SCRIPT');
                _readability_readStyle.text = 'var readStyle = style-newspaper;';
                document.getElementsByTagName('head')[0].appendChild(_readability_readStyle);
                _readability_readSize=document.createElement('SCRIPT');
                _readability_readSize.text = 'var readSize = size-medium;';
                document.getElementsByTagName('head')[0].appendChild(_readability_readSize);
                _readability_readMargin=document.createElement('SCRIPT');
                _readability_readMargin.text = 'var readMargin = margin-wide;';
                document.getElementsByTagName('head')[0].appendChild(_readability_readMargin);
                _readability_script=document.createElement('SCRIPT');
                _readability_script.type='text/javascript';
                _readability_script.src='http://lab.arc90.com/experiments/readability/js/readability.js?x='+(Math.random());
                document.getElementsByTagName('head')[0].appendChild(_readability_script);
                _readability_css=document.createElement('LINK');
                _readability_css.rel='stylesheet';
                _readability_css.href='http://lab.arc90.com/experiments/readability/css/readability.css';
                _readability_css.type='text/css';
                _readability_css.media='screen';
                document.getElementsByTagName('head')[0].appendChild(_readability_css);
                _readability_print_css=document.createElement('LINK');
                _readability_print_css.rel='stylesheet';
                _readability_print_css.href='http://lab.arc90.com/experiments/readability/css/readability-print.css';
                _readability_print_css.media='print';
                _readability_print_css.type='text/css';
                document.getElementsByTagName('head')[0].appendChild(_readability_print_css);
            });
define_key(content_buffer_normal_keymap, "C-x r", "readability_arc90");