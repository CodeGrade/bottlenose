(function() {
  function initFiles() {
    $(".symlink-jump").click(function(e) {
      e.preventDefault();
      e.stopPropagation();
      selectTreeviewFileByHref($(this).data("root"), $(this).attr("href"));
    });
  }


  run_on_page("submissions/details", initFiles);
})();
