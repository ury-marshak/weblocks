// From https://github.com/cprieto/TimeZoneForAspNet, slightly modified

// To activate this, say
//
//   <body onload="checkTZCookie();">

function setCookie(cookieName, cookieValue, expiredays) {
    var exdate = new Date();
    exdate.setDate(exdate.getDate() + expiredays);
    document.cookie = cookieName + "=" + escape(cookieValue) + ((expiredays == null) ? "" : ";expires=" + exdate.toUTCString());
}

function getCookie(cookieName) {
    if (document.cookie.length > 0) {
	cookieStart = document.cookie.indexOf(cookieName + "=");
	if (cookieStart != -1) {
	    cookieStart = cookieStart + cookieName.length + 1;
	    cookieEnd = document.cookie.indexOf(";", cookieStart);
	    if (cookieEnd == -1) cookieEnd = document.cookie.length;
	    return unescape(document.cookie.substring(cookieStart, cookieEnd));
	}
    }
    return "";
}

function getUtcOffset() {
    return (new Date()).getTimezoneOffset();
}

function checkTZCookie() {
    var timeOffset = getCookie("TimeZoneOffset");
    if (timeOffset == null || timeOffset == "") {
	setCookie("TimeZoneOffset", getUtcOffset(), 1);
	window.location.reload();
    }
}
