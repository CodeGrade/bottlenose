/* eslint no-console:0 */
// This file is automatically compiled by Webpack, along with any other files
// present in this directory. You're encouraged to place your actual application logic in
// a relevant structure within app/javascript and only use these pack files to reference
// that code so it'll be compiled.
//
// To reference this file, add <%= javascript_pack_tag 'application' %> to the appropriate
// layout file, like app/views/layouts/application.html.erb


// Uncomment to copy all static images under ../images to the output folder and reference
// them with the image_pack_tag helper in views (e.g <%= image_pack_tag 'rails.png' %>)
// or the `imagePath` JavaScript helper below.
//
// const images = require.context('../images', true)
// const imagePath = (name) => images(name, true)

import 'tablesorter';
import 'tablesorter/dist/css/theme.bootstrap_2.min.css'

import 'jquery-ujs';

import 'bootstrap-toggle';

import '@nathanvda/cocoon';

import 'codemirror/lib/codemirror';
import 'codemirror/lib/codemirror.css';
import 'codemirror/theme/mdn-like.css';
import 'codemirror/addon/runmode/runmode';
import 'codemirror/addon/selection/active-line';
import 'codemirror/mode/clike/clike';
import 'codemirror/mode/mllike/mllike';
import 'codemirror/mode/haskell/haskell';
import 'codemirror/mode/haskell-literate/haskell-literate';
import 'codemirror/mode/ebnf/ebnf';
import 'codemirror/mode/javascript/javascript';
import 'codemirror/mode/markdown/markdown';
import 'codemirror/mode/scheme/scheme';
import 'codemirror/mode/commonlisp/commonlisp';
import 'codemirror/mode/python/python';
import 'codemirror/mode/css/css';
import 'codemirror/mode/stex/stex';
import 'codemirror/mode/xml/xml';
import 'codemirror/mode/yaml/yaml';
import 'codemirror/mode/htmlmixed/htmlmixed';


$(function() {

// require('../src/pdf_render');

require('jquery-ui/ui/widgets/tooltip');
require('../src/0startup');
require('../src/application');
require('../src/assignments')
require('../src/bootstrap.treeview');
require('../src/courses');
require('../src/form-tabs');
require('../src/grades');
require('../src/jquery.matchHeight');
require('../src/jquery.keyDecoder');
require('../src/makefile-mode');
require('../src/submissions');
require('../src/teamsets');

})

// import '../src/pdf_render';
// import '../src/0startup';
// import '../src/application';
// import '../src/assignments';
// import '../src/bootstrap.treeview';
// import '../src/courses';
// import '../src/form-tabs';
// import '../src/grades';
// import '../src/jquery.matchHeight';
// import '../src/jquery.keyDecoder';
// import '../src/makefile-mode';
// import '../src/submissions';
// import '../src/teamsets';
