setup_form = () ->
  $('.add-user-btn').click (e) ->
    row = $(e.target).closest('tr.user')
    opt = $("<option></option>")
            .attr("value", row.data().id)
            .text(row.data().name)
    $('#users').append(opt)
    row.hide()

  $('.remove-users-btn').click (e) ->
    $('#users option').filter((_,e) -> e.selected).each (i, opt) ->
      opt.selected = false
      $(opt).detach()
      $("#users [data-id='#{opt.value}']").show()

  $('#submit-btn').click (ev) ->
    $('#users option').each (i, opt) ->
      opt.selected = true

run_on_page "teams/create", setup_form
run_on_page "teams/new", setup_form
run_on_page "teams/edit", setup_form
