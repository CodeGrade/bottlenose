
(function() {
  window.run_on_page = function(name, func) {
    return $(function() {
      if (name === window.current_page_name) {
        return func();
      }
    });
  };

}).call(this);
