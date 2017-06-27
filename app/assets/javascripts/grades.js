(function() {
  function initFiles() {
    $(".symlink-jump").click(function(e) {
      e.preventDefault();
      e.stopPropagation();
      selectTreeviewFileByHref($(this).attr("href"));
    });
  }


  run_on_page("grades/show", initFiles);
  run_on_page("grades/edit", initFiles);
  run_on_page("grades/show", initFiles);
})();
