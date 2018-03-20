/*

.form-tabs        <- data-init-tab = default tab
  .nav-tabs
     .li a ...    <- click this
  .tab-pane ...   <- to find these
     ....         <- and swap these, so only one .tab-pane at a time has attached children

Both "a" and ".tab-pane" should be marked with matching data-tab attrs.

*/


window.form_tabs_init = function (tabs_div) {
    var top  = $(tabs_div);
    var val0 = top.data('init-tab');

    function show_tab(tab) {
        top.find('.nav-tabs a').each(function (_ii, lnk) {
            var this_tab = $(lnk).data('tab');
            var item = $(lnk).closest('li');

            if (tab == this_tab) {
                item.addClass('active');
            }
            else {
                item.removeClass('active');
            }
        });

        top.find('.tab-pane').each(function (_ii, div) {
            var this_tab = $(div).data('tab');
            var $div = $(div);

            if (tab == this_tab) {
                // Note: this deliberately updates the data attribute after restoring the old contents,
                $div.addClass('active');
                $div.show();
                $div.append($div.data("bn.detached-tab"));
                $div.data("bn.detached-tab", $div.children());
            }
            else if ($div.hasClass('active')) {
                // while this saves the contents to the data attribute before detaching them all
                $div.data("bn.detached-tab", $div.children());
                $div.children().detach();
                $div.removeClass('active');
                $div.hide();
            }
        });
    }

    // Match height before we hide anything.
    var panes = top.find('.tab-pane');
    panes.matchHeight({byRow: false, property: 'height'});
    // matchHeight will trigger on onload and on window-resize
    // so be sure to refill and unhide all the inert tabs before trying to
    // recompute their heights
    var oldBefore = $.fn.matchHeight._beforeUpdate;
    var oldActive = null;
    $.fn.matchHeight._beforeUpdate = function(e, groups) {
      oldActive = top.find('.tab-pane.active');
      panes.each(function(_, div) {
        var $div = $(div);
        $div.addClass('active').append($div.data("bn.detached-tab")).show();
      });
      if (oldBefore)
        oldBefore(e, groups);
    }
    var oldAfter = $.fn.matchHeight._afterUpdate;
    $.fn.matchHeight._afterUpdate = function(e, groups) {
      if (oldAfter)
        oldAfter(e, groups);
      oldActive.each(function(_, div) {
        show_tab($(div).data('tab'));
      });
    }

    top.find(".nav-tabs a").each(function(_ii, lnk) {
        var tab = $(lnk).data('tab');
        if (tab == val0) {
            show_tab(tab);
        }

        $(lnk).on("click", function(evt) {
            evt.preventDefault();
            show_tab(tab);
        });
    });

};

window.form_tabs_init_all = function (thing) {
    $(thing).find('.form-tabs').each(function (_ii, el) {
        form_tabs_init(el);
    });
};

$(function() {
    form_tabs_init_all('body');
});

