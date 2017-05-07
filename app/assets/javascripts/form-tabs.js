(function () {
    var vu;

    function click_tab(evt) {
        evt.preventDefault();

        var lnk  = $(evt.target);
        var tab  = lnk.closest('li');
        var tabs = tab.closest('.nav-tabs').find('li');
        tabs.each(function (_ii, el) {
            $(el).removeClass('active');
        });

        tab.addClass('active');

        vu.type = $(lnk).data('type');
    }

    function init_form_tabs(tabs_div, type) {
        vu = new Vue({
            el: $(tabs_div).find('.form-tabs-content')[0],
            data: {
                type: "default"
            }
        });

        window.vu = vu;

        $(tabs_div).find('.nav-tabs li a').each(function (_ii, lnk) {
            $(lnk).on("click", click_tab);
        });

        var lnk0 = $(tabs_div).find('.nav-tabs li a')[0];
        $(lnk0).click();

        $(tabs_div).find('.nav-tabs li a').each(function (_ii, lnk) {
            if (lnk.data('type') == type) {
                $(lnk).click();
            }
        });
    }

    function init() {
        $('.form-tabs-pane').each(function (_ii, el) {
            init_form_tabs(el, $(el).data('type'));
        });
    }

    $(init);
})();

