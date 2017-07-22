(function() {
  function renderComments(lineComments) {
    $(".file-pane").each(function(index) {
      var theseComments = lineComments[index] || {};
      if (theseComments["noCommentsFor"]) return;
      var cm = $(this).find(".CodeMirror")[0].CodeMirror;
      cm.operation(function() {
        Object.keys(theseComments).forEach(function(type, _) {
          var commentsByType = theseComments[type] || {};
          Object.keys(commentsByType).forEach(function(line, _) {
            var commentsOnLine = commentsByType[line] || {};
            commentsOnLine.forEach(function(comment, _) {
              renderComment(cm, type, line, comment);
            });
          });
        });
      });
    });
  }
  window.renderComments = renderComments;

  function renderComment(cm, type, line, comment) {
    var widget = $("<div>").addClass(comment.severity).addClass(type);
    var table = $("<table>");
    widget.append(table);
    var row = $("<tr>");
    table.append(row);
    var td = $("<td>").addClass("nowrap");
    row.append(td);
    var label = $("<span>").text(comment.label || comment.author).addClass("label label-default");
    td.append(label);
    if (comment.suppressed) {
      var icon = $("<span>").addClass("glyphicon glyphicon-flag")
          .data("toggle", "toolip").data("placement", "top")
          .attr("title", "Too many errors of this type were found; no further points were deducted");
      td.append(icon);
      var deduction = $("<span>").addClass("label label-default")
          .data("toggle", "tooltip").data("placement", "top")
          .attr("title", "This problem would normally deduct " + comment.deduction + " points");
      if (comment.deduction < 0)
        deduction.text("[+" + Math.abs(comment.deduction) + "]");
      else {
        deduction.text("[-" + comment.deduction + "]");
      }
      td.append(deduction);
    } else {
      var icon = $("<span>").addClass("glyphicon")
          .data("toggle", "tooltip").data("placement", "top");
      if (comment.severity === "Error")
        icon.addClass("glyphicon-ban-circle").attr("title", "Error");
      else if (comment.severity === "Warning")
        icon.addClass("glyphicon-warning-sign").attr("title", "Warning");
      else
        icon.addClass("glyphicon-info-sign").attr("title", "Suggestion");
      td.append(icon);
      var deduction = $("<span>").addClass("label label-danger");
      if (comment.deduction < 0)
        deduction.text("[+" + Math.abs(comment.deduction) + "]");
      else {
        deduction.text("[-" + comment.deduction + "]");
      }
      if (comment.deduction < 0)
        deduction.text("+" + Math.abs(comment.deduction));
      else {
        deduction.text("-" + comment.deduction);
      }    
      td.append(deduction);
    }
    td = $("<td>");
    row.append(td);
    if (comment.title !== "" && comment.title !== undefined) {
      td.append($("<span>").addClass("description").text(comment.title + ": " + comment.comment));
    } else {
      td.append($("<span>").addClass("description").text(comment.comment));
    }
    cm.addLineWidget(parseInt(line) - 1, widget[0], {coverGutter: false, noHScroll: true});
  }

  function init_datetime() {
    $('.datetime-picker').datetimepicker({
      sideBySide: true,
      format: "YYYY/MM/DD h:mm A",
      defaultDate: undefined
    });
  }

  var max_grader_order;
  function on_add_grader(evt, el) {
    el.find(".spinner").each(function(_ii, div) {
      activateSpinner(div);
    });
    el.find("input[name$='[order]']").val(++max_grader_order);
    form_tabs_init_all(el);
  }

  function form_init() {
    init_datetime();

    max_grader_order =
      Math.max(0,
               Math.max.apply(null,
                              $("input[name$='[order]'").map(function() { return $(this).val() }).toArray()));

    $('.graders-list').on('cocoon:after-insert', on_add_grader);
    $('.spinner').each(function (_ii, div) {
      activateSpinner(div);
    });

    $("#files-graders").sortable({
      placeholder: "ui-state-highlight",
      update: function(e, ui) {
        var orders = $("#files-graders input[name$='[order]']");
        orders.each(function(index, item) {
          $(item).val(++max_grader_order);
        });
      }
    });

    $("form").submit(function(e) {
      var graderTypeInputs =
          $("li.grader").filter(function(index) {
            return $(this).find("input[id^='assignment_graders_'][id$='_destroy'][value='1']").length == 0;
          }).find("input[id$='_type']");
      debugger
      var graderTypes = graderTypeInputs.map(function() { return $(this).val().replace(/^.*_/, ""); });
      var asHash = Object.create(null);
      graderTypes.map(function() { asHash[this] = (asHash[this] || 0) + 1; });
      var prompt = "";
      for (var type in asHash) {
        if (asHash[type] > 1) {
          prompt += "\t" + asHash[type] + " of " + type + "\n";
        }
      }
      if (prompt !== "") {
        if (!confirm("Are you sure you intended to have multiple graders of the same type?\n" + prompt)) {
          e.preventDefault();
          return false;
        }
      }
      return true;
    });

    $(".file-picker").each(function(index) {
      var $e = $(this);
      $e.find(".assignment-file").change(function() {
        var label = $(this).val().replace(/\\/g, '/').replace(/.*\//, '');
        $e.find(".current_file").text("New file: " + label);
        $e.find(".remove-assignment-file").prop('disabled', false).removeClass("btn-default").addClass("btn-warning");
        $e.find("input.assignment_removefile").val('');
      });
      $e.find(".remove-assignment-file").click(function() {
        $e.find("input[name='assignment_file']").replaceWith(
          $e.find("input[name='assignment_file']").val("<nothing>").clone(true));
        $(this).prop('disabled', true).addClass("btn-default").removeClass("btn-warning");
        $e.find(".current_file").text("New file: <nothing>");
        $e.find("input.assignment_removefile").val('remove');
      });
    });
  }

  run_on_page("assignments/new", form_init);
  run_on_page("assignments/create", form_init);
  run_on_page("assignments/edit", form_init);
  run_on_page("assignments/update", form_init);
})();

/*
run_on_page "assignments/show", () ->
  inputs  = []
  by_user = {}

  find_inputs = () ->
    inputs = $(".grade-entry-box")

    by_user = {}
    inputs.each (ii, box) ->
      user_id = $(box).data("user-id")
      by_user[user_id] = ii

  focus_row = (row_number) ->
    next = inputs[row_number]
    if next
      $(next).focus()


  handle_arrow_keys = (event) ->
    user_id    = $(event.target).data("user-id")
    row_number = by_user[user_id]

    switch $.keyDecoder.parse(event)
      when "Up"
        focus_row(row_number - 1)

      when "Down"
        focus_row(row_number + 1)

  handle_form_send = (event) ->
    box = $(event.target).find(".grade-entry-box")[0]
    user_id = $(box).data("user-id")

    row_number = by_user[user_id]
    focus_row(row_number + 1)

  setup_handlers = () ->
    find_inputs()

    $(".grade-entry-box").off "keyup", handle_arrow_keys
    $(".grade-entry-box").on "keyup", handle_arrow_keys

    $(".sub-form").off "ajax:send", handle_form_send
    $(".sub-form").on "ajax:send", handle_form_send

  $(document).ajaxComplete () ->
    $('#ajax-status').text("ajax-status: done")
    setup_handlers()

  setup_handlers()
*/
