(function() {
  function initFiles() {
    $(".symlink-jump").click(function(e) {
      e.preventDefault();
      e.stopPropagation();
      selectTreeviewFileByHref($(this).data("root"), $(this).attr("href"));
    });
  }

  run_on_page("submissions/details", initFiles);
  

  function initWarnTooBig() {
    $('#submission_upload_file').change(function() {
      var label = $('#submission_upload_file').parent().find('label');
      if (this.files[0].size > (1024 * 1024 * 5)){
          label.text("Upload file (warning: >5 MB)");
      }
      else {
          label.text("Upload file (size ok)");
      }
    });
  }

  run_on_page("submissions/new", initWarnTooBig);
})();
