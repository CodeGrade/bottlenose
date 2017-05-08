window.form_tabs_init = (function () {
    function form_tabs_init(tabs_div) {
        var vu = new Vue({
            el: $(tabs_div).find('.form-tabs-content')[0],
            data: {
                type: $(tabs_div).data('type'),
            }
        });

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

        $(tabs_div).find('.nav-tabs li a').each(function (_ii, lnk) {
            $(lnk).on("click", click_tab);
        });

        var lnk0 = $(tabs_div).find('.nav-tabs li a')[0];
        $(lnk0).click();

        $(tabs_div).find('.nav-tabs li a').each(function (_ii, lnk) {
            if ($(lnk).data('type') == $(tabs_div).data('type')) {
                $(lnk).click();
            }
        });
    }

    $(function() {
        $('.form-tabs-pane').each(function (_ii, el) {
            form_tabs_init(el);
        });
    });

    return form_tabs_init;
})();
