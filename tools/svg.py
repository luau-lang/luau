# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

class Node:
    def __init__(self):
        self.name = ""
        self.children = {}
        # computed
        self.depth = 0
        self.width = 0
        self.offset = 0

    def child(self, name):
        node = self.children.get(name)
        if not node:
            node = self.__class__()
            node.name = name
            self.children[name] = node
        return node

    def subtree(self):
        result = [self]
        offset = 0

        while offset < len(result):
            p = result[offset]
            offset += 1
            for c in p.children.values():
                result.append(c)

        return result

def escape(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

def layout(root, widthcb):
    for n in reversed(root.subtree()):
        # propagate width to the parent
        n.width = widthcb(n)
        for c in n.children.values():
            n.width += c.width

        # compute offset from parent for every child in width order (layout order)
        offset = 0
        for c in sorted(n.children.values(), key = lambda x: x.width, reverse = True):
            c.offset = offset
            offset += c.width

    for n in root.subtree():
        for c in n.children.values():
            c.depth = n.depth + 1
            c.offset += n.offset

# svg template (stolen from framegraph.pl)
template = r"""<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg version="1.1" width="1200" height="$height" onload="init(evt)" viewBox="0 0 1200 $height" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<!-- Flame graph stack visualization. See https://github.com/brendangregg/FlameGraph for latest version, and http://www.brendangregg.com/flamegraphs.html for examples. -->
<defs>
    <linearGradient id="background" y1="0" y2="1" x1="0" x2="0" >
        <stop stop-color="$gradient-start" offset="5%" />
        <stop stop-color="$gradient-end" offset="95%" />
    </linearGradient>
</defs>
<style type="text/css">
    text { font-family:Verdana; font-size:12px; fill:rgb(0,0,0); }
    #search, #ignorecase { opacity:0.1; cursor:pointer; }
    #search:hover, #search.show, #ignorecase:hover, #ignorecase.show { opacity:1; }
    #subtitle { text-anchor:middle; font-color:rgb(160,160,160); }
    #title { text-anchor:middle; font-size:17px}
    #unzoom { cursor:pointer; }
    #frames > *:hover { stroke:black; stroke-width:0.5; cursor:pointer; }
    .hide { display:none; }
    .parent { opacity:0.5; }
</style>
<script type="text/ecmascript">
<![CDATA[
    "use strict";
    var details, searchbtn, unzoombtn, matchedtxt, svg, searching, currentSearchTerm, ignorecase, ignorecaseBtn;
    function init(evt) {
        details = document.getElementById("details").firstChild;
        searchbtn = document.getElementById("search");
        ignorecaseBtn = document.getElementById("ignorecase");
        unzoombtn = document.getElementById("unzoom");
        matchedtxt = document.getElementById("matched");
        svg = document.getElementsByTagName("svg")[0];
        searching = 0;
        currentSearchTerm = null;
    }

    window.addEventListener("click", function(e) {
        var target = find_group(e.target);
        if (target) {
            if (target.nodeName == "a") {
                if (e.ctrlKey === false) return;
                e.preventDefault();
            }
            if (target.classList.contains("parent")) unzoom();
            zoom(target);
        }
        else if (e.target.id == "unzoom") unzoom();
        else if (e.target.id == "search") search_prompt();
        else if (e.target.id == "ignorecase") toggle_ignorecase();
    }, false)

    // mouse-over for info
    // show
    window.addEventListener("mouseover", function(e) {
        var target = find_group(e.target);
        if (target) details.nodeValue = g_to_text(target);
    }, false)

    // clear
    window.addEventListener("mouseout", function(e) {
        var target = find_group(e.target);
        if (target) details.nodeValue = ' ';
    }, false)

    // ctrl-F for search
    window.addEventListener("keydown",function (e) {
        if (e.keyCode === 114 || (e.ctrlKey && e.keyCode === 70)) {
            e.preventDefault();
            search_prompt();
        }
    }, false)

    // ctrl-I to toggle case-sensitive search
    window.addEventListener("keydown",function (e) {
        if (e.ctrlKey && e.keyCode === 73) {
            e.preventDefault();
            toggle_ignorecase();
        }
    }, false)

    // functions
    function find_child(node, selector) {
        var children = node.querySelectorAll(selector);
        if (children.length) return children[0];
        return;
    }
    function find_group(node) {
        var parent = node.parentElement;
        if (!parent) return;
        if (parent.id == "frames") return node;
        return find_group(parent);
    }
    function orig_save(e, attr, val) {
        if (e.attributes["_orig_" + attr] != undefined) return;
        if (e.attributes[attr] == undefined) return;
        if (val == undefined) val = e.attributes[attr].value;
        e.setAttribute("_orig_" + attr, val);
    }
    function orig_load(e, attr) {
        if (e.attributes["_orig_"+attr] == undefined) return;
        e.attributes[attr].value = e.attributes["_orig_" + attr].value;
        e.removeAttribute("_orig_"+attr);
    }
    function g_to_text(e) {
        var text = find_child(e, "details").firstChild.nodeValue;
        return (text)
    }
    function g_to_func(e) {
        var child = find_child(e, "rawtext");
        return child ? child.textContent : null;
    }
    function update_text(e) {
        var r = find_child(e, "rect");
        var t = find_child(e, "text");
        var w = parseFloat(r.attributes.width.value) -3;
        var txt = find_child(e, "rawtext").textContent.replace(/\([^(]*\)$/,"");
        t.attributes.x.value = parseFloat(r.attributes.x.value) + 3;

        // Smaller than this size won't fit anything
        if (w < 2 * 12 * 0.59) {
            t.textContent = "";
            return;
        }

        t.textContent = txt;
        // Fit in full text width
        if (/^ *$/.test(txt) || t.getSubStringLength(0, txt.length) < w)
            return;

        for (var x = txt.length - 2; x > 0; x--) {
            if (t.getSubStringLength(0, x + 2) <= w) {
                t.textContent = txt.substring(0, x) + "..";
                return;
            }
        }
        t.textContent = "";
    }

    // zoom
    function zoom_reset(e) {
        if (e.attributes != undefined) {
            orig_load(e, "x");
            orig_load(e, "width");
        }
        if (e.childNodes == undefined) return;
        for (var i = 0, c = e.childNodes; i < c.length; i++) {
            zoom_reset(c[i]);
        }
    }
    function zoom_child(e, x, ratio) {
        if (e.attributes != undefined) {
            if (e.attributes.x != undefined) {
                orig_save(e, "x");
                e.attributes.x.value = (parseFloat(e.attributes.x.value) - x - 10) * ratio + 10;
                if (e.tagName == "text")
                    e.attributes.x.value = find_child(e.parentNode, "rect[x]").attributes.x.value + 3;
            }
            if (e.attributes.width != undefined) {
                orig_save(e, "width");
                e.attributes.width.value = parseFloat(e.attributes.width.value) * ratio;
            }
        }

        if (e.childNodes == undefined) return;
        for (var i = 0, c = e.childNodes; i < c.length; i++) {
            zoom_child(c[i], x - 10, ratio);
        }
    }
    function zoom_parent(e) {
        if (e.attributes) {
            if (e.attributes.x != undefined) {
                orig_save(e, "x");
                e.attributes.x.value = 10;
            }
            if (e.attributes.width != undefined) {
                orig_save(e, "width");
                e.attributes.width.value = parseInt(svg.width.baseVal.value) - (10 * 2);
            }
        }
        if (e.childNodes == undefined) return;
        for (var i = 0, c = e.childNodes; i < c.length; i++) {
            zoom_parent(c[i]);
        }
    }
    function zoom(node) {
        var attr = find_child(node, "rect").attributes;
        var width = parseFloat(attr.width.value);
        var xmin = parseFloat(attr.x.value);
        var xmax = parseFloat(xmin + width);
        var ymin = parseFloat(attr.y.value);
        var ratio = (svg.width.baseVal.value - 2 * 10) / width;

        // XXX: Workaround for JavaScript float issues (fix me)
        var fudge = 0.0001;

        unzoombtn.classList.remove("hide");

        var el = document.getElementById("frames").children;
        for (var i = 0; i < el.length; i++) {
            var e = el[i];
            var a = find_child(e, "rect").attributes;
            var ex = parseFloat(a.x.value);
            var ew = parseFloat(a.width.value);
            var upstack;
            // Is it an ancestor
            if ($flip == 1) {
                upstack = parseFloat(a.y.value) > ymin;
            } else {
                upstack = parseFloat(a.y.value) < ymin;
            }
            if (upstack) {
                // Direct ancestor
                if (ex <= xmin && (ex+ew+fudge) >= xmax) {
                    e.classList.add("parent");
                    zoom_parent(e);
                    update_text(e);
                }
                // not in current path
                else
                    e.classList.add("hide");
            }
            // Children maybe
            else {
                // no common path
                if (ex < xmin || ex + fudge >= xmax) {
                    e.classList.add("hide");
                }
                else {
                    zoom_child(e, xmin, ratio);
                    update_text(e);
                }
            }
        }
        search();
    }
    function unzoom() {
        unzoombtn.classList.add("hide");
        var el = document.getElementById("frames").children;
        for(var i = 0; i < el.length; i++) {
            el[i].classList.remove("parent");
            el[i].classList.remove("hide");
            zoom_reset(el[i]);
            update_text(el[i]);
        }
        search();
    }

    // search
    function toggle_ignorecase() {
        ignorecase = !ignorecase;
        if (ignorecase) {
            ignorecaseBtn.classList.add("show");
        } else {
            ignorecaseBtn.classList.remove("show");
        }
        reset_search();
        search();
    }
    function reset_search() {
        var el = document.querySelectorAll("#frames rect");
        for (var i = 0; i < el.length; i++) {
            orig_load(el[i], "fill")
        }
    }
    function search_prompt() {
        if (!searching) {
            var term = prompt("Enter a search term (regexp " +
                "allowed, eg: ^ext4_)"
                + (ignorecase ? ", ignoring case" : "")
                + "\nPress Ctrl-i to toggle case sensitivity", "");
            if (term != null) {
                currentSearchTerm = term;
                search();
            }
        } else {
            reset_search();
            searching = 0;
            currentSearchTerm = null;
            searchbtn.classList.remove("show");
            searchbtn.firstChild.nodeValue = "Search"
            matchedtxt.classList.add("hide");
            matchedtxt.firstChild.nodeValue = ""
        }
    }
    function search(term) {
        if (currentSearchTerm === null) return;
        var term = currentSearchTerm;

        var re = new RegExp(term, ignorecase ? 'i' : '');
        var el = document.getElementById("frames").children;
        var matches = new Object();
        var maxwidth = 0;
        for (var i = 0; i < el.length; i++) {
            var e = el[i];
            var func = g_to_func(e);
            var rect = find_child(e, "rect");
            if (func == null || rect == null)
                continue;

            // Save max width. Only works as we have a root frame
            var w = parseFloat(rect.attributes.width.value);
            if (w > maxwidth)
                maxwidth = w;

            if (func.match(re)) {
                // highlight
                var x = parseFloat(rect.attributes.x.value);
                orig_save(rect, "fill");
                rect.attributes.fill.value = "rgb(230,0,230)";

                // remember matches
                if (matches[x] == undefined) {
                    matches[x] = w;
                } else {
                    if (w > matches[x]) {
                        // overwrite with parent
                        matches[x] = w;
                    }
                }
                searching = 1;
            }
        }
        if (!searching)
            return;

        searchbtn.classList.add("show");
        searchbtn.firstChild.nodeValue = "Reset Search";

        // calculate percent matched, excluding vertical overlap
        var count = 0;
        var lastx = -1;
        var lastw = 0;
        var keys = Array();
        for (k in matches) {
            if (matches.hasOwnProperty(k))
                keys.push(k);
        }
        // sort the matched frames by their x location
        // ascending, then width descending
        keys.sort(function(a, b){
            return a - b;
        });
        // Step through frames saving only the biggest bottom-up frames
        // thanks to the sort order. This relies on the tree property
        // where children are always smaller than their parents.
        var fudge = 0.0001; // JavaScript floating point
        for (var k in keys) {
            var x = parseFloat(keys[k]);
            var w = matches[keys[k]];
            if (x >= lastx + lastw - fudge) {
                count += w;
                lastx = x;
                lastw = w;
            }
        }
        // display matched percent
        matchedtxt.classList.remove("hide");
        var pct = 100 * count / maxwidth;
        if (pct != 100) pct = pct.toFixed(1)
        matchedtxt.firstChild.nodeValue = "Matched: " + pct + "%";
    }
]]>
</script>
<rect x="0.0" y="0" width="1200.0" height="$height.0" fill="url(#background)"  />
<text id="title" x="600.00" y="24" >$title</text>
<text id="unzoom" x="10.00" y="24" class="hide">Reset Zoom</text>
<text id="search" x="1090.00" y="24" >Search</text>
<text id="ignorecase" x="1174.00" y="24" >ic</text>
<text id="matched" x="1090.00" y="$status" > </text>
<text id="details" x="10.00" y="$status" > </text>
<g id="frames">
"""

def namehash(s):
    # FNV-1a
    hval = 0x811c9dc5
    for ch in s:
        hval = hval ^ ord(ch)
        hval = hval * 0x01000193
        hval = hval % (2 ** 32)
    return (hval % 31337) / 31337.0

def display(root, title, colors, flip = False):
    if colors == "cold":
        gradient_start = "#eef2ee"
        gradient_end = "#e0ffe0"
    else:
        gradient_start = "#eeeeee"
        gradient_end = "#eeeeb0"

    maxdepth = 0
    for n in root.subtree():
        maxdepth = max(maxdepth, n.depth)

    svgheight = maxdepth * 16 + 3 * 16 + 2 * 16

    print(template
        .replace("$title", title)
        .replace("$gradient-start", gradient_start)
        .replace("$gradient-end", gradient_end)
        .replace("$height", str(svgheight))
        .replace("$status", str(svgheight - 16 + 3))
        .replace("$flip", str(int(flip)))
    )

    framewidth = 1200 - 20

    for n in root.subtree():
        if n.width / root.width * framewidth < 0.1:
            continue

        x = 10 + n.offset / root.width * framewidth
        y = (maxdepth - 1 - n.depth if flip else n.depth) * 16 + 3 * 16
        width = n.width / root.width * framewidth
        height = 15

        if colors == "cold":
            fillr = 0
            fillg = int(190 + 50 * namehash(n.name))
            fillb = int(210 * namehash(n.name[::-1]))
        else:
            fillr = int(205 + 50 * namehash(n.name))
            fillg = int(230 * namehash(n.name[::-1]))
            fillb = int(55 * namehash(n.name[::-2]))

        fill = "rgb({},{},{})".format(fillr, fillg, fillb)
        chars = width / (12 * 0.59)

        text = n.text()

        if chars >= 3:
            if chars < len(text):
                text = text[:int(chars-2)] + ".."
        else:
            text = ""

        print("<g>")
        print("<title>{}</title>".format(escape(n.title())))
        print("<details>{}</details>".format(escape(n.details(root))))
        print("<rect x='{}' y='{}' width='{}' height='{}' fill='{}' rx='2' ry='2' />".format(x, y, width, height, fill))
        print("<text x='{}' y='{}'>{}</text>".format(x + 3, y + 10.5, escape(text)))
        print("<rawtext>{}</rawtext>".format(escape(n.text())))
        print("</g>")

    print("</g>\n</svg>\n")
