const pdfjsLib = require('pdfjs-dist');
const worker = require('pdfjs-dist/build/pdf.worker');
pdfjsLib.GlobalWorkerOptions.workerSrc = worker;


var CSS_UNITS = 96/72;
var eventBus = new pdfjsViewer.EventBus();
$(document.body).prepend($("#selectionWrapperWrapper").detach());
function encodePath(str) {
  return str.split('/').map(encodeURIComponent).join('/');
}
window.jQuery.widget("bn.pdfViewer", {
  options: { },
  _create: function() {
    var thiz = this;
    var $div = this.element;
    var $pages = $div.find("div.pdfPages");
    ////////////////////////////////////////
    //////////////////// Rendering
    ////////////////////////////////////////
    pdfjsLib.getDocument(encodePath($div.data("rendered"))).promise.then(createPages);
    function createPages(pdf) {
      // Page numbers are 1-based
      $pages.data("pdf", pdf);
      var promises = []
      for (var i = 1; i <= pdf.numPages; i++) {
        var pageContainer = $("<div>").addClass("pageContainer");
        var pageComments = $("<div>").addClass("pageComments");
        var regionComments = $("<div>").addClass("regionComments")
        $pages.append(pageContainer.append(pageComments).append(regionComments));
        thiz._trigger("pageContainerReady", null, {
          pageNum: i,
          pdfViewer: thiz,
          pageContainer, pageComments, regionComments
        });
        promises.push(pdf.getPage(i));
      }
      $pages.data("pdfPages", Promise.all(promises));
      return enqueueAllPages();
    }
    this.pagesToRender = {promise: Promise.resolve, worklist: [], scale: ""};
    function enqueueAllPages(force) {
      return $pages.data("pdfPages").then(function(pages) {
        var renderScale = $div.find("#scaleSelect").val();
        if (!force && renderScale === thiz.pagesToRender.scale) {
          return thiz.pagesToRender.promise;
        }
        thiz.pagesToRender.worklist.forEach((item) => {if (item) { item.skip = true; }});
        var $allContainers = $pages.find("div.pageContainer").map(function(index, c) {
          return {
            index,
            outerContainer: c,
            pageComments: $(c).find("div.pageComments"),
            regionComments: $(c).find("div.regionComments")//.data("index", index)
          };
        });
        // console.log("AllPages: " + $allContainers.length + ", pageCount: " + pages.length);
        var worklist = [];
        // Page numbers are 1-based, but forEach() is 0-based
        pages.forEach(function(page, pageNum) {
          worklist.push({
            pdfViewer: thiz,
            pageData: page,
            pageNum: pageNum + 1,
            renderScale,
            numPages: pages.length,
            container: $allContainers[pageNum]});
        });
        var $loading = $div.find("div.loadingBar");
        var $progress = $loading.find("div.progress");
        $progress.addClass("indeterminate");
        $loading.removeClass("hidden").show();
        thiz.pagesToRender = {promise: renderPages(worklist, $loading, $progress, 0),
                              worklist: worklist,
                              scale: renderScale};
        return thiz.pagesToRender.promise;
      });
    }
    function renderPages(worklist, $loading, $progress, curIndex) {
      if (curIndex == worklist.length) {
        worklist.length = 0;
        $loading.addClass("hidden");
        $loading.css({"width": "0%"})
        $progress.removeClass("indeterminate");
        return Promise.resolve();
      } else {
        var workItem = worklist[curIndex];
        worklist[curIndex] = undefined;
        if (workItem.skip) {
          // console.log("Skipping " + workItem.pageNum + " at " + workItem.renderScale);
          return renderPages(worklist, $loading, $progress, curIndex + 1);
        } else {
          var comments = workItem.container.regionComments.find("div.Region").detach();
          workItem.container.regionComments.find("div.page").remove();
          var pct = 100 * (workItem.pageNum / workItem.numPages);
          $progress.addClass("indeterminate");
          $loading.removeClass("hidden").css({width: "" + Math.floor(pct) + "%"}).show();
          return renderPage(workItem.pageData, workItem.pageNum, workItem.container, workItem.renderScale)
            .then(() => {
              // console.log("Rendering " + workItem.pageNum + " at " + workItem.renderScale);
              workItem.container.regionComments.find("div.page")
                .append(comments);
              if (thiz.options.editable) {
                workItem.container.regionComments.find("div.page")
                  .off("click.page").on("click.page", (e, ui) => thiz._clickPage(e, ui, $(e.currentTarget)))
                  .selectable({stop: (e, ui) => thiz._stopCommentRegion(e, ui),
                                start: (e, ui) => thiz._startCommentRegion(e, ui),
                                delay: 1,
                                appendTo: $("#translateWrapper")[0]})
                  .selectable("enable");
              }
              comments.each((index, c) => {
                $(c).areaComment("instance").reinit();
              });
              thiz._trigger("pageReady", null, workItem);
              return renderPages(worklist, $loading, $progress, curIndex + 1);
            });
        }
      }
    }
    function renderPage(pdfPage, pageNum, container, renderScale) {
      // Creating the page view with default parameters.
      var width;
      var fontSize = Number.parseFloat($pages.css('font-size'));
      var viewport = pdfPage.getViewport({scale: 1});
      var containerHScale, containerVScale;
      if ($pages.is(":visible")) {
        containerHScale = ($pages.width() - 3 * fontSize) / viewport.width;
        containerVScale = ($pages.height() - 3 * fontSize) / viewport.height;
      } else {
        // If the PDF isn't currently visible, force its nearest invisible parent to be visible
        // long enough to compute dimensions
        var lastHidden = $pages.parentsUntil(":visible").last();
        var hiddenStyle = lastHidden.attr("style");
        lastHidden.show();
        containerHScale = ($pages.width() - 3 * fontSize) / viewport.width;
        containerVScale = ($pages.height() - 3 * fontSize) / viewport.height;
        if (hiddenStyle)
          lastHidden.attr("style", hiddenStyle);
        else
          lastHidden.removeAttr("style");
      }
      switch(renderScale) {
      case 'page-actual': width = 1; break;
      case 'page-width': width = containerHScale / CSS_UNITS; break;
      case 'page-fit': width = Math.min(containerHScale, containerVScale) / CSS_UNITS; break;
      case 'auto':
        if (containerHScale > containerVScale)
          width = Math.min(containerHScale, containerVScale) / CSS_UNITS;
        else
          width = containerHScale / CSS_UNITS;
        break;
      default:
        width = Number.parseFloat(renderScale);
        break;
      }
      var pdfPageView = new pdfjsViewer.PDFPageView({
        container: container.regionComments[0],
        id: pageNum,
        scale: width,
        defaultViewport: viewport,
        renderer: 'canvas',
        eventBus: eventBus,
        // // We can enable text/annotations layers, if needed
        // textLayerFactory: new pdfjsViewer.DefaultTextLayerFactory(),
        // annotationLayerFactory: new pdfjsViewer.DefaultAnnotationLayerFactory(),
      });
      // Associates the actual page with the view, and drawing it
      pdfPageView.setPdfPage(pdfPage);
      return pdfPageView.draw();
    }
    ////////////////////////////////////////
    //////////////////// Zooming
    ////////////////////////////////////////
    $div.find("#zoomOut").click(function(e) {
      var $scale = $div.find("#scaleSelect");
      var $options = $scale.find("option");
      var newIndex = Math.max($scale[0].selectedIndex - 1, 0);
      $scale.val($options[newIndex].value).change();
      e.stopImmediatePropagation();
      return false;
    });
    $div.find("#zoomIn").click(function(e) {
      var $scale = $div.find("#scaleSelect");
      var $options = $scale.find("option");
      var newIndex = Math.min($scale[0].selectedIndex + 1, $options.length - 1);
      $scale.val($scale.find("option")[newIndex].value).change();
      e.stopImmediatePropagation();
      return false;
    });
    $div.find("#scaleSelect").change(enqueueAllPages);
    var resizer;
    $(window).resize(function(e) {
      if (e.target !== window) return;
      if (resizer) { window.clearTimeout(resizer); resizer = undefined; }
      if ($pages.data("pdfPages")) {
        resizer = setTimeout(() => enqueueAllPages(true), 100);
      }
    });
    ////////////////////////////////////////
    //////////////////// Downloading
    ////////////////////////////////////////
    var clickingDownload = false;
    $div.find("#new_window").click(function(e) {
      window.open(encodePath($div.data("source")), "_blank");
      e.stopPropagation();
    });
    $div.find("#download").click(function(e) {
      // console.log("Clicking on button");
      var dm = new pdfjsViewer.DownloadManager({});
      dm.downloadUrl(encodePath($div.data("source")), $div.data("alt"));
      e.stopPropagation();
    });
  },
  
  ////////////////////////////////////////
  //////////////////// PDF Commenting
  ////////////////////////////////////////
  createAreaComment: function($page, editable, options) {
    var $final = $("<div>");
    $page.append($final);
    var combinedOptions = $.extend({}, options, {
      change: function(e, details) {
        // console.log("areaComment " + details.widget.id() + " change details:", details);
        if (details.newSeverity)
          $final.removeClass(details.oldSeverity).addClass(details.newSeverity);
        if (options.change)
          options.change.call(this, e, details);
      }
    });
    return $final.areaComment({
      container: "parent",
      editable: editable,
      left: options.left,
      top: options.top,
      width: options.width,
      height: options.height,
      dimensions: options.dimensions,
      gradeId: options.gradeId,
      severity: options.severity,
      commentEditor: combinedOptions
    });
  },
  _onMove: function(e, ui) {
    var $selHelper = $(".ui-selectable-helper");
    if ($selHelper.width() > 20 && $selHelper.height() > 20) {
      $selHelper.addClass("big-enough");
    } else {
      $selHelper.removeClass("big-enough");
    }
  },
  _startCommentRegion: function(e, ui) {
    var $div = this.element;
    var offsetDiv = $div.offset();
    $("#selectionWrapperWrapper").css({
      overflow: "hidden",
      position: "absolute",
      top: offsetDiv.top,
      left: offsetDiv.left,
      width: $div.width(),
      height: $div.height(),
      "z-index": 999
    }).removeClass("hidden").on("mousemove", (e, ui) => this._onMove(e, ui));
    var offsetTarget = $(e.target).offset();
    $("#selectionWrapper").css({
      overflow: "hidden",
      position: "absolute",
      border: "1px solid rgba(255, 255, 0, 0.5)",
      top: offsetTarget.top - offsetDiv.top,
      left: offsetTarget.left - offsetDiv.left,
      width: e.target.offsetWidth,
      height: e.target.offsetHeight,
    });
    $("#translateWrapper").css({
      top: -offsetTarget.top,
      left: -offsetTarget.left,
      position: "relative"
    });
  },
  _stopCommentRegion: function(e, ui) {
    var $region = $(e.target).selectable("instance").helper;
    if ($region.hasClass("big-enough")) {
      this._selectArea(e, ui, $region);
    } else {
      this._clickPage(e, ui, $(e.target));
    }
    $("#selectionWrapperWrapper").off("mousemove").addClass("hidden");
  },
  _clickPage: function(e, ui, $page) {
    var details = {
      pdfViewer: this,
      page: $page,
      regionComments: $page.closest(".regionComments"),
      pageComments: $page.closest(".regionComments").prev(".pageComments"),
      pageNum: $page.data("page-number")
    };
    this._trigger("pageClicked", e, details);
    e.stopImmediatePropagation();
    return false;
  },
  _selectArea: function(e, ui, $region) {
    var $page = $(e.target);
    var pageWidth = $page[0].offsetWidth;
    var pageHeight = $page[0].offsetHeight;
    var offset = $page.offset();
    var leftPx = $region[0].offsetLeft - offset.left;
    var topPx = $region[0].offsetTop - offset.top;
    var widthPx = $region[0].offsetWidth;
    var heightPx = $region[0].offsetHeight;
    if (leftPx < 0) {
      widthPx += leftPx; leftPx = 0;
    } else if (leftPx + widthPx >= pageWidth) {
      widthPx = pageWidth - leftPx;
    }
    if (topPx < 0) {
      heightPx += topPx; topPx = 0;
    } else if (topPx + heightPx >= pageHeight) {
      heightPx = pageHeight - topPx;
    }
    var left = leftPx / pageWidth;
    var top = topPx / pageHeight;
    var width = widthPx / pageWidth;
    var height = heightPx / pageHeight;
    var details = {
      pdfViewer: this,
      page: $page,
      regionComments: $page.closest(".regionComments"),
      selection: $region,
      left: left,
      top: top,
      width: width,
      height: height,
      pageWidth: pageWidth,
      pageheight: pageHeight,
      pageNum: $page.data("page-number")
    };
    this._trigger("pageAreaSelected", e, details);
    e.stopImmediatePropagation();
    return false;
  }
});
