function togglePopup(selector) {
    var w = Math.min(document.documentElement.clientWidth, document.body.parentNode.scrollWidth, window.innerWidth || 0 );
    var h = Math.min(document.documentElement.clientHeight, document.body.parentNode.scrollHeight, window.innerHeight || 0);

    var mw = Math.max(document.documentElement.clientWidth, document.body.parentNode.scrollWidth, window.innerWidth || 0 );
    var mh = Math.max(document.documentElement.clientHeight, document.body.parentNode.scrollHeight, window.innerHeight || 0);

    $('#blanket').css({
	"height": mh + 'px',
	"width": mw + 'px'
    });

    $(selector).css({
	"width": (w - 100 -50) + 'px',
	"top": '50px',
	"left": '50px',
	"height": (document.documentElement.clientHeight - 100 -100) + 'px',
	"padding": '50px 25px'
    });

    $('#blanket').toggle(300);
    $(selector).toggle(500);
}

// Customized GraphMLViewer RunPlayer function
function ZRunPlayer() {

    var hasRequestedVersion = DetectFlashVer(9, 0, 38);

    var params = CreateParams(arguments);

    if (!hasRequestedVersion) {
        return false;
    }

    initMouseWheel();

    var url = "scripts/GraphMLViewer/GraphMLViewer.swf";
    var protocol = location.protocol;
    if (protocol != "file:") {
        url = protocol + "//www.yworks.com/products/graphmlviewer/1.1/GraphMLViewer.swf";
    } else {
        protocol = "http:";
    }
    var str = '<div class="graphml-container"><object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000" ' +
            'id="viewer'+ params["id"] + '" width="' + params["width"] + '" height="' + params["height"] + '"' +
            ' codebase="' + protocol + '//fpdownload.macromedia.com/get/flashplayer/current/swflash.cab">' +
            ' <param name="movie" value="' + url + '" />' +
            ' <param name="quality" value="high" />' +
            ' <param name="bgcolor" value="#ffffff" />' +
            ' <param name="allowScriptAccess" value="always" />' +
            ' <param name="FlashVars" value="' + params["flashvars"] + '" />' +
            ' <embed src="' + url + '" quality="high" bgcolor="#ffffff"' +
            '  width="' + params["width"] + '" height="' + params["height"] + '" name="viewer'+ params["id"] + '" align="middle"' +
            '  play="true"' +
            '  loop="false"' +
            '  allowScriptAccess="always"' +
            '  type="application/x-shockwave-flash"' +
            '  pluginspage="' + protocol + '//www.adobe.com/go/getflashplayer"' +
            '  FlashVars="' + params["flashvars"] + '">' +
            '</embed>' +
            '</object></div>';

    return str;
}

$(document).ready(function(){
    // display all .graphml files incline
    $('a[href$=".graphml"]').each(function() {
        var path = $(this).attr('href');
        var str = ZRunPlayer(
            "width", "100%",
            "height", "600",
            "graphURL", path,
            "movable", "true",
            "overview", "true",
            "toolbar", "false",
            "tooltips", "false",
            "links", "true",
            "scrollbars", "false",
            "linksInNewWindow", "true",
            "viewport", "full"
        );
        $(this).replaceWith(str);
    });

    // content table
    $('#text-table-of-contents').before('<div id="blanket" style="display: none;"></div>');
    $('#text-table-of-contents').prepend('<a href="#">X</a>').hide();

    $('#table-of-contents h2').text("M").click(function() {
	togglePopup('#text-table-of-contents');
    });

    $('#text-table-of-contents a').each(function() {
	$(this).click(function() {
	    togglePopup('#text-table-of-contents');
	});
    });

    // decorate the all of the img
    $('img').each(function() {
	$(this).load(function () {
	    // image size
	    $(this).wrap('<a href="' + $(this).attr('src') + '">');
	    var nice_width = $(this).parent().parent().width(); // depends on size of the container 'div.figure'
	    if($(this).width() >  nice_width) {
		// $(this).css({'width' : nice_width + 'px', 'height' : $(this).height() * nice_width / $(this).width() + 'px'});
		$(this).css({'width' : nice_width + 'px'});
	    }
	});
    });
});
