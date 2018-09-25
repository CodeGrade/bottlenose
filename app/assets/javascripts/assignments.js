(function() {
  function renderComments(lineComments) {
    $(".file-pane").each(function(index) {
      var theseComments = lineComments[index] || {};
      if (theseComments["noCommentsFor"]) return;
      // Standard text-files get line comments
      $(this).find(".CodeMirror").each(function(_, cm) {
        renderLineComments(cm.CodeMirror, theseComments);
      });
      // Nonstandard files (jars, overly-large files) get page comments
      $(this).find(".pageContainer").each(function(pageNum, pageContainer) {
        renderPageComments(pageNum + 1, pageContainer, theseComments);
      });
      // Pdfs get page and region comments
      $(this).find("div[id^='pdf_']").each(function(_, pdfDiv) {
        var pageRendered = [];
        $(pdfDiv).pdfViewer({
          pageContainerReady: function(_, details) {
            renderPageComments(details.pageNum, details.pageContainer, theseComments);
          },
          pageReady: function(_, details) {
            if (pageRendered[details.pageNum]) return;
            pageRendered[details.pageNum] = true;
            renderRegionComments(details.pageNum, details.container.regionComments, theseComments);
          }
        });
      });
    });
  }
  window.renderComments = renderComments;
  function renderLineComments(cm, theseComments) {
    cm.operation(function() {
      Object.keys(theseComments).forEach(function(gradeId, _) {
        var commentsByGrade = theseComments[gradeId] || {};
        var type = commentsByGrade.type;
        Object.keys(commentsByGrade).forEach(function(line, _) {
          if (line === "type") return;
          var commentsOnLine = commentsByGrade[line] || {};
          commentsOnLine.forEach(function(comment, _) {
            renderLineComment(cm, gradeId, type, line, comment);
          });
        });
      });
    });
  }
  function renderPageComments(pageNum, pageContainer, theseComments) {
    var pageComments = $(pageContainer).find(".pageComments");
    Object.keys(theseComments).forEach(function(gradeId, _) {
      var commentsByGrade = theseComments[gradeId] || {};
      var type = commentsByGrade.type;
      var commentsForPage = commentsByGrade[pageNum] || [];
      commentsForPage.forEach(function(comment, _) {
        if (!comment.info) {
          renderPageComment(pageComments[0], gradeId, type, pageNum, comment);
        }
      });
    });
  }
  function renderRegionComments(pageNum, regionComments, theseComments) {
    Object.keys(theseComments).forEach(function(gradeId, _) {
      var commentsByGrade = theseComments[gradeId] || {};
      var type = commentsByGrade.type;
      var commentsForPage = commentsByGrade[pageNum] || [];
      commentsForPage.forEach(function(comment, _) {
        if (comment.info) {
          renderRegionComment(regionComments[0], gradeId, type, pageNum, comment);
        }
      });
    });
  }

  function renderPageComment(pc, gradeId, type, line, comment) {
    var widget = $("<div>").lineCommentView({
      gradeId,
      type,
      line,
      severity: comment.severity,
      label: comment.label,
      author: comment.author,
      deduction: comment.deduction,
      title: comment.title,
      comment: comment.comment,
      suppressed: comment.suppressed
    });
    $(pc).append(widget);
  }
  window.renderPageComment = renderPageComment;
  function renderRegionComment(rc, gradeId, type, line, comment) {
    var pdfViewer = $(rc).closest(".pdfDisplay").pdfViewer("instance");
    var $page = $(rc).find(".page");
    var infoJson;
    if (comment.info) {
      try {
        infoJson = JSON.parse(comment.info);
      } catch(e) {
        infoJson = undefined;
      }
    }
    if (infoJson && infoJson.type === "area") {
      pdfViewer.createAreaComment($page, false, {
        gradeId,
        left: infoJson.left,
        top: infoJson.top,
        width: infoJson.width,
        height: infoJson.height,
        dimensions: infoJson.dimensions,
        type,
        line,
        label: comment.label,
        id: comment.id,
        severity: comment.severity,
        label: comment.label,
        author: comment.author,
        deduction: comment.deduction,
        title: comment.title,
        comment: comment.comment,
        suppressed: comment.suppressed
      });
    }
  }
  window.renderRegionComment = renderRegionComment;

  function renderLineComment(cm, gradeId, type, line, comment) {
    var widget = $("<div>").lineCommentView({
      gradeId,
      type,
      line,
      severity: comment.severity,
      label: comment.label,
      author: comment.author,
      deduction: comment.deduction,
      title: comment.title,
      comment: comment.comment,
      suppressed: comment.suppressed
    });
    cm.addLineWidget(parseInt(line) - 1, widget[0], {coverGutter: false, noHScroll: true});
  }
  window.renderLineComment = renderLineComment;

  function init_datetime() {
    $('.datetime-picker').datetimepicker({
      sideBySide: true,
      format: "YYYY/MM/DD h:mm A",
      defaultDate: undefined
    });
  }

  var max_grader_order;
  const extraCreditWarning = "This assignment is already marked as extra-credit.  Only mark this grader as extra credit if you truly mean to have extra credit on top of extra credit.";
  function on_add_grader(evt, el) {
    el.find(".spinner").each(function(_ii, div) {
      activateSpinner(div);
    });
    var newToggles = el.find("input[data-toggle='toggle']");
    newToggles.bootstrapToggle();
    newToggles = newToggles.parent("div[data-toggle='toggle']");
    newToggles.attr('title', extraCreditWarning);
    newToggles.tooltip();
    if ($("input[data-toggle='toggle'][name='assignment[extra_credit]']").prop('checked')) {
      newToggles.tooltip('enable');
    } else {
      newToggles.tooltip('disable');
    }
    el.find("input[name$='[order]']").val(++max_grader_order);
    el.find(".file-picker").each(function(index) { activate_file_picker($(this)); });
    form_tabs_init_all(el);
  }

  function on_add_interlock(evt, el) {
    var relAssignment = el.find("select[name$='[related_assignment_id]']");
    el.find("select[name$='[constraint]']").change(function(){
      if (this.value == "check_section_toggles") {
        $(relAssignment).hide();
      } else {
        $(relAssignment).show();
      }
    });
  }

  function activate_file_picker($e) {
    $e.find(".custom-file").change(function() {
      var label = $(this).val().replace(/\\/g, '/').replace(/.*\//, '');
      $e.find(".current_file").text("New file: " + label);
      $e.find(".remove-custom-file").prop('disabled', false).removeClass("btn-default").addClass("btn-warning");
      $e.find("input.remove-custom-file").val('');
    });
    $e.find(".remove-custom-file").click(function() {
      $e.find("input.custom-file").replaceWith(
        $e.find("input.custom-file").clone(true));
      $(this).prop('disabled', true).addClass("btn-default").removeClass("btn-warning");
      $e.find(".current_file").text("New file: <nothing>");
      $e.find("input.remove-custom-file").val('remove');
    });
  }
  

  function form_init() {
    init_datetime();

    max_grader_order =
      Math.max(0,
               Math.max.apply(null,
                              $("input[name$='[order]']").map(function() { return $(this).val() }).toArray()));

    $('.graders-list').on('cocoon:after-insert', on_add_grader);
    $('.interlocks-list').on('cocoon:after-insert', on_add_interlock);

    $('.spinner').each(function (_ii, div) {
      activateSpinner(div);
      if ($(div).find("input").prop("disabled")) {
        disableSpinner(div);
      }
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

    $("input[name='assignment[extra_credit]']").change(function() {
      var newToggles = $(".graders-list .tab-pane").map(function(_, e) { return $(e).data("bn.detached-tab"); });
      newToggles = $.map(newToggles, function(kids) { return $.map(kids, function(k) { return k; }); });
      newToggles = $(newToggles).find("div[data-toggle='toggle']");
      debugger
      if ($(this).prop('checked')) {
        newToggles.tooltip('enable');
        newToggles.find("input[data-toggle='toggle']").bootstrapToggle('off');
      } else {
        newToggles.tooltip('disable');
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

    $(".file-picker").each(function(index) { activate_file_picker($(this)); });
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
