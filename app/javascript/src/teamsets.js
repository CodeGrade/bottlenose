(function() {
  var setup_form;

  setup_form = function() {
    $('.add-user-btn').click(function(e) {
      var opt, row;
      row = $(e.target).closest('tr.user');
      opt = $("<option></option>").attr("value", row.data().id).text(row.data().name);
      $('#users').append(opt);
      return row.hide();
    });
    $('.remove-users-btn').click(function(e) {
      return $('#users option').filter(function(_, e) {
        return e.selected;
      }).each(function(i, opt) {
        opt.selected = false;
        $(opt).detach();
        return $("#users [data-id='" + opt.value + "']").show();
      });
    });
    $('#submit-btn').click(function(ev) {
      return $('#users option').each(function(i, opt) {
        return opt.selected = true;
      });
    });
    return $("#existingTS").on("change", function() {
      $(".teamset-div").addClass("hidden");
      return $("#ts_" + $(this).val()).removeClass("hidden");
    }).change();
  };

  run_on_page("teamsets/create", setup_form);

  run_on_page("teamsets/new", setup_form);

  run_on_page("teamsets/edit", setup_form);

  run_on_page("teamsets/bulk_enter", setup_form);

}).call(this);
