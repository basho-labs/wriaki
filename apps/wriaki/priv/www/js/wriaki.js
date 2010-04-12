function navToSearch() {
    var P = $('#searchtext').val();
    window.location.href = '/'+encodeURIComponent(P);
}

function articleURL() {
    L = window.location.href;
    return L.slice(0, L.indexOf('?'));
}

function readCookie(name) {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length;i++) {
	var c = ca[i];
	while (c.charAt(0)==' ') c = c.substring(1,c.length);
	if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
    }
    return null;
}

function clearCookie(name) {
    alert("TODO: clear "+name+" cookie");
}

function loginSignupSuccess() {
    if (window.location.href.indexOf('next=') > 0) {
        start = window.location.href.indexOf('next=')+('next='.length);
        end = window.location.href.indexOf('&', start)
        if (end < 0) end = window.location.href.length;
        window.location.href =
            decodeURIComponent(
                window.location.href.slice(start, end));
    } else {
        window.location.href = '/';
    }
}

$(function() {
    /* Header search buttons */
    $('#searchbutton').click(navToSearch);
    $('#searchtext').keyup(function(e) {
        if (e.keyCode == 10 || e.keyCode == 13)
            navToSearch();
    });

    /* Article editor buttons */
    $('#editcancel').click(function() {
        window.location.href = articleURL();
    });

    $('#editsave').click(function() {
        var req = {
            url: articleURL(),
            type: 'PUT',
            data: {
                text:$('#edittext').val(),
                msg:$('#editmsg').val(),
                vclock:$('#editvclock').val()
            },
            success: function() { window.location.href = req.url; }
        };
        $.ajax(req);
    });

    /* User settings buttons */
    $('#settingsave').click(function() {
        var data = {};

        var p = $('input[name=password]').val();
        if (p) data.password = p;
        
        var e = $('input[name=email]').val();
        if (e) data.email = e;

        var b = $('input[name=bio]').val();
        if (b) data.name = b;

        var u = $('input[name=username]');
        if (u.length) {
            data.username = u.val();
            if (!data.username) {
                alert("Please choose a username.");
                return;
            }
            if (!data.password) {
                alert("Please choose a password.");
                return;
            }
        }

        req = {
            url: data.username ? '/user/'+data.username : window.location.href,
            type: 'PUT',
            data: data,
            success: loginSignupSuccess,
            error: function(req) {
                if (req.status == 409)
                    $('#settingserror').text('the requested username is taken');
                else
                    $('#settingserror').text('an unknown error occured: '+req.responseText);
            }
        };
        $.ajax(req);
    });

    $('#loginbutton').click(function() {
        var username = $('input[name=login_username]').val();
        var password = $('input[name=login_password]').val();
        
        $.ajax({
            url:'/user/'+username,
            type:'POST',
            data:{'password':password},
            success:loginSignupSuccess,
            error:function(req) {
                $('#loginerror').text('incorrect username/password combination');
            }
        });
    });
    
    $('#logoutbutton').click(function() {
        
        $.ajax({
            url:'/user/'+readCookie('username')+'/'+readCookie('session'),
            type:'DELETE',
            success:function() { window.location.href = '/'; },
            error:function(req) {
                if (req.status == 404) //already logged out
                    window.location.href = '/';
            }
        });
    });

    if (readCookie('username') && readCookie('session')) {
        $.ajax({
            url:'/user/'+readCookie('username')+'/'+readCookie('session'),
            success:function() {
                $('#login').hide();
                $('#welcome')
                    .find('a').text(decodeURIComponent(readCookie('username')))
                    .attr('href', '/user/'+readCookie('username')+'?edit')
                    .end()
                    .css('display', 'block');
            },
            error:function(req) {
                if (req.status == 404) {
                    clearCookie('username');
                    clearCookie('session');
                }
            }
        });
    }
});