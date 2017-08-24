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
//= require jquery-ui/widgets/sortable
//= require jquery_ujs
//= require jquery.matchHeight
//= require jquery-tablesorter
//= require jquery.keyDecoder
//= require cocoon
//= require moment
//= require bootstrap-sprockets
//= require bootstrap-datetimepicker
//= require bootstrap.treeview
//= require bootstrap-toggle
//= require codemirror/lib/codemirror
//= require codemirror/addon/runmode/runmode
//= require codemirror/addon/selection/active-line
//= require codemirror/mode/clike/clike
//= require codemirror/mode/mllike/mllike
//= require codemirror/mode/ebnf/ebnf
//= require codemirror/mode/javascript/javascript
//= require codemirror/mode/scheme/scheme
//= require pyret-codemirror-mode/mode/pyret
//= require_tree .

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
  "ArrowUp": true,
  "ArrowDown": true,
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
    case 37: e.key = "ArrowLeft"; break;
    case 38: e.key = "ArrowUp"; break;
    case 39: e.key = "ArrowRight"; break;
    case 40: e.key = "ArrowDown"; break;
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
  $('[data-toggle="tooltip"]').each(function(elt) {
    $(this).tooltip({
      animated: 'fade',
      placement: $(this).data("placement") || 'right',
      html: true
    });
  });

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
});


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
    var disabled = input.prop("disabled");
    var val = parseFloat(input.val(), 10);
    if (max !== undefined && val >= max) {
      upArrow.addClass("disabled");
      clearInterval(upInterval);
      upInterval = undefined;
    }
    if (!disabled && (min === undefined || val > min))
      downArrow.removeClass("disabled");
    if (min !== undefined && val <= min) {
      downArrow.addClass("disabled");
      clearInterval(downInterval);
      downInterval = undefined;
    }
    if (!disabled && (max === undefined || val < max))
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
    if (input.prop("disabled")) return;
    validateNumericInput(e);
    if (e.key === "ArrowUp") { increment(); return; }
    if (e.key === "ArrowDown") { decrement(); return; }
    if (e.key === "ArrowLeft" || e.key === "ArrowRight") { return; }
    if (e.key === "Backspace" || e.key === "Delete") { return; }
    var curVal = $(this).val();
    var newVal = curVal.slice(0, this.selectionStart) + e.key + curVal.slice(this.selectionEnd, curVal.length);
    newVal = parseFloat(newVal, 10);
    if (max !== undefined && newVal > max) { e.preventDefault(); }
    if (min !== undefined && newVal < min) { e.preventDefault(); }
  });
  input.on("deactivate", function(e) {
    input.prop("disabled", true);
    upArrow.addClass("disabled");
    downArrow.addClass("disabled");
  });
  input.on("reactivate", function(e) {
    input.prop("disabled", false);
    validate();
  });
  input.bind("paste", function(e) { e.preventDefault(); });

  $(upArrow).on('mousedown', function() {
    if (input.prop("disabled")) return;
    upInterval = setInterval(increment, 200);
    increment();
  });
  $(downArrow).on('mousedown', function() {
    if (input.prop("disabled")) return;
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
function disableSpinner(divOrInput) {
  $(divOrInput).find("input").addBack().trigger("deactivate");
}
function enableSpinner(divOrInput) {
  $(divOrInput).find("input").addBack().trigger("reactivate");
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
