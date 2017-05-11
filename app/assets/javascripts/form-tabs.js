window.form_tabs_init = function (tabs_div) {
    var top  = $(tabs_div);
    var val0 = top.data('type');
    var tabs = {};

    function show_tab(ty) {
        top.find('.nav-tabs a').each(function (_ii, lnk) {
            if (ty == $(lnk).data('type')) {
                $(lnk).addClass('active');
            }
            else {
                $(lnk).removeClass('active');
            }
        });

        FIXME: Finish this plan.
        top.find('.tab-pane')

        var tp = $($(tlnk).data('target'))[0];
        console.log("hide", tp);

        var uuid = $(tp).closest(".form-tabs-pane").data("uuid");
        tabs[uuid] = tabs[uuid] || {};

        var pid = tp.id;
        var fmg = $(tp).find(".form-group")[0];
        if (fmg) {
            console.log("Storing to", uuid, pid);
            tabs[uuid][pid] = fmg;
            $(fmg).detach();
        }
    }

    function show_tab(tlnk) {
        var tp = $($(tlnk).data('target'))[0];
        console.log("show", tp);

        var uuid = $(tp).closest(".form-tabs-pane").data("uuid");
        tabs[uuid] = tabs[uuid] || {};

        var pid = tp.id;
        var fmg = tabs[uuid][pid];
        $(tp).append(fmg);
    }

    top.find(".nav-tabs a").each(function(_ii, lnk) {
        if (val0 == $(lnk).data('type')) {
            $(lnk).click();
        }
    });

    top.find(".nav-tabs a").each(function(_ii, lnk) {
        if (val0 != $(lnk).data('type')) {
            hide_tab(lnk);
        }

        $(lnk).on("show.bs.tab", function(evt) {
            show_tab(evt.target);
        });
        $(lnk).on("hide.bs.tab", function(evt) {
            hide_tab(evt.target);
        });
    });
};

$(function() {
    $('.form-tabs-pane').each(function (_ii, el) {
        form_tabs_init(el);
    });
});

