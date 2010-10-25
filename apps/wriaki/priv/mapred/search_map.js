// This function is used as the map phase implementation for
// searching article text
// Each element in the result should be a structure of
// {
//   key: (string), // key of the Riak object for the article
//   ranges: [ (string) ], // list of matching phrases
// }
function(v, d) {
    // always want the key in the result
    var result = {key: v.key};

    // if the object was found, and we have word position info
    if (v.values && v.values[0] && d.p) {
        // sort positions
        d.p.sort();

        // create a list of start/end position pairs that attempt
        // to group "close" positions (within 5 words) together
        result.ranges=[{start: d.p[0], end: d.p[0]}];
        for (var i = 1; i < d.p.length; i++) {
            if (result.ranges[result.ranges.length-1].end-d.p[i] < 5)
                result.ranges[result.ranges.length-1].end = d.p[i];
            else
                result.ranges[result.ranges.length] =
                {start: d.p[i], end: d.p[i]};
        }

        // extract the phrases from the text
        var text = JSON.parse(v.values[0].data).text;
        var words = text.match(/[a-z0-9\u80-\uff]*/g).filter(
            function(s) { return s != "" && s.length > 2; }).length

        for (var i = 0; i < result.ranges.length; i++) {
            var s = result.ranges[i].start < 5 ?
                0 : result.ranges[i].start-5;
            var e = result.ranges[i].end+5 > words ?
                words : result.ranges[i].end+5;

            // regexp is basically "match START words, then grab everything
            // until WORDS-END words from the end"
            var w = "[a-z0-9\u80-\uff]"; // word character
            var n = "[^a-z0-9\u80-\uff]"; // non-word character
            var skip = "(?:"+w+"{3,}"+n+"+(?:(?:"+w+"|"+w+w+")"+n+"+)*)"
            var match = (new RegExp(
                skip+"{"+s+"}"+
                "(.*)"+n+"*"+
                skip+"{"+(words-e)+"}"))
                .exec(text);
            result.ranges[i] = match[1];
        }
    }

    return [result];
}
