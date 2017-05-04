// This is a manifest file that'll be compiled into application.js, which will include all the files
// listed below.
//
// Any JavaScript/Coffee file within this directory, lib/assets/javascripts, vendor/assets/javascripts,
// or vendor/assets/javascripts of plugins, if any, can be referenced here using a relative path.
//
// It's not advisable to add code directly here, but if you do, it'll appear at the bottom of the
// the compiled file.
//
// WARNING: THE FIRST BLANK LINE MARKS THE END OF WHAT'S TO BE PROCESSED, ANY BLANK LINE SHOULD
// GO AFTER THE REQUIRES BELOW.
//
//= require jquery
//= require jquery_ujs
//= require jquery.matchHeight
//= require jquery-tablesorter
//= require jquery.keyDecoder
//= require nicEdit
//= require moment
//= require bootstrap-sprockets
//= require bootstrap-datetimepicker
//= require bootstrap.treeview
//= require codemirror
//= require codemirror/addons/runmode/runmode
//= require codemirror/addons/selection/active-line
//= require codemirror/modes/clike
//= require codemirror/modes/mllike
//= require codemirror/modes/ebnf
//= require codemirror/modes/javascript
//= require codemirror/modes/scheme
//= require_tree .



// Based on http://www.simple10.com/code/2013/11/15/css-only-input-toggle-switch/
(function($) {
  'use strict';

  var Toggle = function(element, options) {
    this.$element = $(element);
    this.options = $.extend({}, this.defaults(), options);
    this.render();
  }
  Toggle.VERSION = '2017.1.10';
  Toggle.DEFAULTS = {
    on: 'On',
    off: 'Off',
    onstyle: 'primary',
    offstyle: 'default',
    size: 'normal',
    style: '',
    width: null,
    width: null
  }

  Toggle.prototype.defaults = function() {
    return {
      on: this.$element.attr('data-on') || Toggle.DEFAULTS.on,
      off: this.$element.attr('data-off') || Toggle.DEFAULTS.off,
      onstyle: this.$element.attr('data-onstyle') || Toggle.DEFAULTS.onstyle,
      offstyle: this.$element.attr('data-offstyle') || Toggle.DEFAULTS.offstyle,
      size: this.$element.attr('data-size') || Toggle.DEFAULTS.size,
      style: this.$element.attr('data-style') || Toggle.DEFAULTS.style,
      width: this.$element.attr('data-width') || Toggle.DEFAULTS.width,
      height: this.$element.attr('data-height') || Toggle.DEFAULTS.height
    }
  }

  Toggle.prototype.render = function() {
    var name = this.$element.attr("id") || this.$element.attr("name");
    this._onstyle = 'btn-' + this.options.onstyle;
    this._offstyle = 'btn-' + this.options.offstyle;
    var size = this.options.size === 'large' ? 'btn-lg'
	: this.options.size === 'small' ? 'btn-sm'
	: this.options.size === 'mini' ? 'btn-xs'
	: '';
    var $toggle = $("<div class='toggle'>");
    var $hidden = $("<input>").attr("name", name).attr("type", "hidden").val(0);
    var $toggleOn = $('<div class="toggle-on btn">').html(this.options.on)
	.addClass(this._onstyle + ' ' + size);
    var $toggleHandle = $('<div class="toggle-handle btn btn-default">')
	.addClass(size);
    var $toggleOff = $('<div class="toggle-off btn">').html(this.options.off)
	.addClass(this._offstyle + ' ' + size + ' active');
    
    var $btn =
        $("<div class='btn'>").append($("<label>")
                                      .attr("for", name)
                                      .append($toggleOn, $toggleHandle, $toggleOff));
    this.$element.wrap($toggle);
    this.$element.before($hidden);
    this.$element.after($btn);
  }


  // TOGGLE PLUGIN DEFINITION
  // ========================

  function Plugin(option) {
    return this.each(function () {
      var $this   = $(this);
      var data    = $this.data('bs.toggle');
      var options = typeof option == 'object' && option;

      if (!data) $this.data('bs.toggle', (data = new Toggle(this, options)));
      if (typeof option == 'string' && data[option]) data[option]();
    });
  }

  var old = $.fn.bootstrapToggle;

  $.fn.bootstrapToggle             = Plugin;
  $.fn.bootstrapToggle.Constructor = Toggle;

  // TOGGLE NO CONFLICT
  // ==================

  $.fn.toggle.noConflict = function () {
    $.fn.bootstrapToggle = old;
    return this;
  }

  // TOGGLE DATA-API
  // ===============

  $(function() {
    $('input[type=checkbox][data-toggle^=toggle]').bootstrapToggle();
  })

  $(document).on('click.bs.toggle', 'div[data-toggle^=toggle]', function(e) {
    var $checkbox = $(this).find('input[type=checkbox]');
    $checkbox.bootstrapToggle('toggle');
    e.preventDefault();
  });
  
})(jQuery);



// Based on https://stackoverflow.com/questions/14324919/status-of-rails-link-to-function-deprecation
function enableReflectiveCalls() {
  $('[data-on][data-call][data-args]').each(function(d){
    if ($(this).data("already-enabled-reflective-call")) return;
    var event = $(this).data('on');
    $(this).on(event, function(e) {
      var toCall = $(this).data('call');
      var args = $(this).data('args')
      if (typeof(window[toCall]) !== 'function')
        throw new Error("No such function to call: " + toCall);
      if (!(args instanceof Array))
        throw new Error("Arguments are not an array: " + args);
      args = args.slice();
      args.push(e);
      window[toCall].apply(this, args);
    });
    $(this).data("already-enabled-reflective-call", true);
  });
}


var validKeys = {
  "ArrowLeft": true,
  "ArrowRight": true,
  "Backspace": true,
  "Delete": true,
  "End": true,
  "Home": true,
  "Tab": true,
};
var validKeyCodes = {
  9: true, // Tab
};

function validateNumericInput(e) {
  if (e.key === undefined) {
    // We're on Safari :(
    switch (e.which) {
    case 8: e.key = "Backspace"; break;
    case 9: e.key = "Tab"; break;
    case 35: e.key = "End"; break;
    case 36: e.key = "Home"; break;
    case 37: e.key = "Left"; break;
    case 39: e.key = "Right"; break;
    case 46: e.key = "Delete"; break;
    case 48: case 49: case 50: case 51: case 52: case 53: case 54: case 55: case 56: case 57:
      e.key = String.fromCharCode(e.which); break;
    case 189: e.key = "-"; break;
    case 190: e.key = "."; break;
    default: e.key = "Unknown";
    }
  }
  if (validKeys[e.key] || validKeyCodes[e.keyCode] || validKeyCodes[e.which]) return;
  if (e.key.match(/^F\d+$/)) return;
  if (!Number.isNaN(Number(e.key)) && (Number(e.key) == Number.parseInt(e.key))) return;
  if (e.key === "." && e.currentTarget.value.indexOf(".") < 0) return;
  if (e.key === "-" && e.currentTarget.value.indexOf("-") < 0 && e.currentTarget.selectionStart === 0) return;
  if (e.ctrlKey || e.altKey || e.metaKey) return;
  e.preventDefault();
};

function ensureValidNumericInputOnSubmit(e, sel) {
  var problems = false;
  sel = sel || "input.numeric";
  $(sel).each(function(elt) {
    if (Number.isNaN(Number.parseFloat($(this).val()))) {
      problems = true;
      $(this)
        .addClass("badAnswer")
        .one("focus", function() { $(this).removeClass("badAnswer"); });
    }
  });
  if (problems) {
    e.preventDefault();
    alert("There are invalid values for some of the fields; please correct them before submitting.");
  }
  return !problems;
}

function ensureFilesPresentOnSubmit(e, sel) {
  var problems = false;
  sel = sel || "input[type='file']";
  $(sel).each(function(elt) {
    if (this.files.length === 0) {
      problems = true;
      $(this)
        .addClass("badAnswer")
        .one("focus", function() { $(this).removeClass("badAnswer"); });
    }
  });
  if (problems) {
    e.preventDefault();
    alert("There are missing required files for some of the fields; please correct them before submitting.");
  }
  return !problems;
}

$(function() {
  $('[data-toggle="tooltip"]').tooltip()
  
  $('.local-time').each(function(_) {
    var dd = moment(Date.parse($(this).text()));
    if (!dd.isValid()) { dd = moment($(this).text()); }
    
    if (dd.isValid()) {
      var today = moment().startOf('day');
      var tomorrow = moment(today).add(1, 'days');
      var twodays = moment(tomorrow).add(1, 'days');
      if (today.isSameOrBefore(dd) && dd.isBefore(tomorrow))
        $(this).text("Today, " + dd.format("h:mm:ssa"));
      else if (tomorrow.isSameOrBefore(dd) && dd.isBefore(twodays))
        $(this).text("Tomorrow, " + dd.format("h:mm:ssa"));
      else
        $(this).text(dd.format("MMM D YYYY, h:mm:ssa"));
    }
  });
  
  $("input.numeric").on("keydown", validateNumericInput);
})


function activateSpinner(obj, options) {
  var spinner = $(obj || this);
  var input = spinner.find('input');
  var upArrow = spinner.find('.btn:first-of-type');
  var downArrow = spinner.find('.btn:last-of-type');
  var upInterval, downInterval;
  var delta = input.data("delta") || 1;
  var max = input.data("max");
  var min = input.data("min");
  var val = parseFloat(input.val(), 10);
  var precision = parseInt((options && options.precision) || spinner.data("precision") || "0");
  if (max !== undefined && val >= max)
    upArrow.addClass("disabled");
  if (min !== undefined && val <= min)
    downArrow.addClass("disabled");
  function validate() {
    max = input.data("max");
    min = input.data("min");
    var val = parseFloat(input.val(), 10);
    if (max !== undefined && val >= max) {
      upArrow.addClass("disabled");
      clearInterval(upInterval);
      upInterval = undefined;
    }
    if (min === undefined || val > min)
      downArrow.removeClass("disabled");
    if (min !== undefined && val <= min) {
      downArrow.addClass("disabled");
      clearInterval(downInterval);
      downInterval = undefined;
    }
    if (max === undefined || val < max)
      upArrow.removeClass("disabled");
  }
  input.on("change", validate);
  function increment() {
    var newVal = (parseFloat(input.val(), 10) || 0) + delta;
    if (max !== undefined) {
      newVal = Math.min(max, newVal);
    }
    input.val(newVal.toFixed(precision)).change();
  }
  function decrement() {
    var newVal = (parseFloat(input.val(), 10) || 0) - delta;
    if (min !== undefined) {
      newVal = Math.max(min, newVal);
    }
    input.val(newVal.toFixed(precision)).change();
  }
  input.on("keydown", function(e) {
    validateNumericInput(e);
    if (e.key === "ArrowUp") { increment(); return; }
    if (e.key === "ArrowDown") { decrement(); return; }
    if (e.key === "ArrowLeft" || e.key === "ArrowRight") { return; }
    var curVal = $(this).val();
    var newVal = curVal.slice(0, this.selectionStart) + e.key + curVal.slice(this.selectionEnd, curVal.length);
    newVal = parseFloat(newVal, 10);
    if (max !== undefined && newVal > max) { e.preventDefault(); }
    if (min !== undefined && newVal < min) { e.preventDefault(); }
  });
  
  $(upArrow).on('mousedown', function() {
    upInterval = setInterval(increment, 200);
    increment();
  });
  $(downArrow).on('mousedown', function() {
    downInterval = setInterval(decrement, 200);
    decrement();
  });
  $(document).on('mouseup', function() {
    if (upInterval) clearInterval(upInterval);
    if (downInterval) clearInterval(downInterval);
    upInterval = undefined;
    downInterval = undefined;
    return false;
  });
  return input;
}

function makeSpinner(options) {
  var input = $("<input>")
      .addClass("form-control numeric")
      .val(options.val || 0)
      .bind("paste", function(e) { e.preventDefault(); });
  if (options.klass !== undefined)
    input.addClass(options.klass);
  if (options.max !== undefined)   input.data("max", options.max);
  if (options.min !== undefined)   input.data("min", options.min);
  if (options.delta !== undefined) input.data("delta", options.delta);
  var div = $("<div>").addClass("input-group spinner")
      .append(input)
      .append($("<div>").addClass("input-group-btn-vertical")
              .append($("<button>").addClass("btn btn-default")
                      .append($("<i>").addClass("fa fa-caret-up")))
              .append($("<button>").addClass("btn btn-default")
                      .append($("<i>").addClass("fa fa-caret-down"))));
  activateSpinner(div, options);
  return div;
}

$(function() {
  function fixSizes() {
    var $affixElement = $('[data-spy="affix"]');
    $affixElement.width($affixElement.parent().width());
  }
  $(window).resize(fixSizes);
  fixSizes();
});
