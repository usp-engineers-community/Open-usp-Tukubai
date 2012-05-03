window.onload = function() 
{
  adjustMegamenu({
    'Info':                      {cols: 1, width: 20},
    'UECジャーナル':             {cols: 1, width: 25},
    'Tukubai':                   {cols: 1, width: 27},
    'Tukubaiコマンドマニュアル': {cols: 4, width: 13},
    'コマンドTips':              {cols: 1, width: 23},
    'シェルプログラミングTips':  {cols: 1, width: 19}
  });
  setTimeout(adjustSubmenuPosition, 400/* wait for animation */);

  /* Twitter button */
  !function(d, s, id) {
    var js, fjs=d.getElementsByTagName(s)[0];
    if (!d.getElementById(id)) {
      js = d.createElement(s);
      js.id = id;
      js.src = "https://platform.twitter.com/widgets.js";
      fjs.parentNode.insertBefore(js, fjs);
    }
  }(document, "script", "twitter-wjs");

  /* Facebook like button */
  (function(d, s, id) {
    var js, fjs = d.getElementsByTagName(s)[0];
    if (d.getElementById(id)) return;
    js = d.createElement(s); js.id = id;
    js.src = "https://connect.facebook.net/ja_JP/all.js#xfbml=1";
    fjs.parentNode.insertBefore(js, fjs);
  }(document, 'script', 'facebook-jssdk'));
};

function getChildNodes(parent, name) 
{
  var children = parent.childNodes;
  var _name = name.toLowerCase();
  var nodes = [];
  for (var i = 0; i < children.length; i++) {
    if (children[i].tagName && 
        children[i].tagName.toLowerCase() === _name)
     nodes.push(children[i]);
  }
  return nodes;
};

function adjustMegamenu(conf) 
{
  var megamenu = document.getElementById('megamenu');
  if (!megamenu) return;
  var lis = getChildNodes(megamenu, 'li');
  for (var i = 0; i < lis.length; i++) {
    var anchor = getChildNodes(lis[i], 'a');
    if (anchor.length <= 0) continue;
    var sub = getSubmenu(lis[i]);
    if (!sub) continue;
    for (key in conf) {
      if (anchor[0].innerHTML === key) {
        adjustSubmenu(sub, conf[key]);
        break;
      }
    }
  }
};

function adjustSubmenu(sub, conf) 
{
  sub.style.width = (conf.cols * (conf.width - 1) + 1) + 'em';
  var lis = getChildNodes(sub, 'li');
  for (var i = 0; i < lis.length; i++) {
    lis[i].style.width = (conf.width -1) + 'em';
  }
};

function adjustSubmenuPosition() 
{
  var megamenu = document.getElementById('megamenu');
  if (!megamenu) return;
  var mlpos = megamenu.offsetLeft;
  var mwidth = megamenu.offsetWidth;
  var lis = getChildNodes(megamenu, 'li');
  for (var i = 0; i < lis.length; i++) {
    var sub = getSubmenu(lis[i]);
    if (!sub) continue;
    var lpos = lis[i].offsetLeft;
    var smwidth = sub.offsetWidth;
    if ((mlpos + mwidth) < (lpos + smwidth))
      sub.style.left = 
        ((mlpos + mwidth) - (lpos + smwidth)) + 'px';
  }
};

function getSubmenu(parent) {
  var ul = getChildNodes(parent, 'ul');
  if (ul.length <= 0) return null;
  return ul[0];
};

function hackIE() {
}

/*
 * Copyright (c) 2012 Universal Shell Programming Laboratoryratory
 *
 * Permission is hereby granted, free of charge, to any person 
 * obtaining a copy of this software and associated documentation 
 * files (the "Software"), to deal in the Software without restriction, 
 * including without limitation the rights to use, copy, modify, merge, 
 * publish, distribute, sublicense, and/or sell copies of the Software, 
 * and to permit persons to whom the Software is furnished to do so, 
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be 
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF 
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY 
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, 
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
