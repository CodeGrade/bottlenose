(function() {
    var section_template;

    function add_section(evt) {
        evt.preventDefault();

        content = section_template;
        console.log("click!", content);
        var new_id = new Date().getTime();
        var ids_rx = /course_sections_attributes_\d+/g;
        var nam_rx = /course\[sections_attributes\]\[\d+\]/g;
        content = content.replace(ids_rx, "course_sections_attributes_" + new_id);
        content = content.replace(nam_rx, "course[sections_attributes][" + new_id + "]");

        var newrow = $("<tr class='form-group row'>").html(content);
        $("#sections").append(newrow);
        newrow.find("span.findUser").each(function(i) { enableLookupUser($(this)); });
        newrow.find("input.numeric").on("keypress", validateNumericInput);
        enableReflectiveCalls();
    }

    function on_add_section(evt, row) {
        $(row).find("span.findUser").each(function(i) { enableLookupUser($(this)); });
        $(row).find("input.numeric").on("keypress", validateNumericInput);
    }

    function enableLookupUser(foundIt) {
        var username = foundIt.prev();
        username.on("blur", function() {
            $.ajax({
                type: "GET",
                url: "/users/lookup",
                contentType: 'application/json; charset=UTF-8',
                data: {username: username.val()},
                success: function(data) {
                    foundIt.text(" (" + data.name + ")");
                },
                error: function(xkr, status, error) {
                    foundIt.text(" (error looking up username)");
                }
            });
        });
    }

    function init() {
        enableReflectiveCalls();
        $("span.findUser").each(function(i) { enableLookupUser($(this)); });
        $(".add-section-button").click(add_section);
        $("input.numeric").on("keypress", validateNumericInput);

        var last_sec = $('.section-form').last();
        section_template = last_sec.html();
        last_sec.remove();

        $('#sections').on('cocoon:after-insert', on_add_section);
    }

    run_on_page("courses/new", init);
    run_on_page("courses/create", init);
    run_on_page("courses/edit", init);
    run_on_page("courses/update", init);
})();


