
window.run_on_page = (name, func) ->
  $(() ->
    if name == window.current_page_name
      func()
  )
